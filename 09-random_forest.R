exchage_text_df <- pin_read(.board, "exchage_text_df")
eurhuf_df <- pin_read(.board, "eurhuf_df")
#exchage_text_df <- read_csv("exchage_rate_df_log.csv")

leaded_close_df <- eurhuf_df |>
  transmute(
    time = lag(time, 2), close # for easier calculation
  )

set.seed(123)

linear_reg_glm_spec <- linear_reg() %>%
  set_engine('lm')

rand_forest_lm_spec <- rand_forest(
  trees = tune(),
  min_n = 5
) %>%
  set_engine("randomForest") %>%
  set_mode("regression")

lasso_spec <- linear_reg(penalty = 0.1, mixture = 1) %>%
  set_engine("glmnet")

svm_spec <- svm_linear(mode = "regression", engine = "LiblineaR")


design_df <- exchage_text_df |>
  mutate(e_rate = lead(e_rate, 2)) |>
  na.omit()

folds <- design_df |>
  rolling_origin(
    initial = 300,
    assess = 100,
    cumulative = FALSE
  )

rec <- recipe(e_rate ~ ., data = design_df) |>
  step_rm(time) |>
  step_zv() |>
  step_normalize(all_numeric_predictors()) |>
  step_corr(all_numeric_predictors(), threshold = .5)

wf <- workflow() |>
  add_recipe(rec) |>
  add_model(rand_forest_lm_spec)

mda_vec <- function(data, truth, estimate, na_rm = TRUE) {
  
  MDirAcc <- function(truth, estimate) {
    return( mean(sign(truth)==sign(estimate)) )
  }
  
  
  metric_vec_template(
    metric_impl = MDirAcc,
    truth = truth, 
    estimate = estimate,
    na_rm = na_rm,
    cls = "numeric"
  )
  
}

mda <- new_numeric_metric(mda_vec, direction = "maximize")

mda.data.frame <- function(data, truth, estimate, na_rm = TRUE, case_weights = ~NULL) {
  
  yardstick::metric_summarizer(
    metric_nm = "mda",
    metric_fn = mda_vec,
    data = data,
    truth = !! enquo(truth),
    estimate = !! enquo(estimate), 
    na_rm = na_rm,
  )
  
}

mda_df <- new_numeric_metric(mda.data.frame, direction = "maximize")

magok <- detectCores()-1
cl <- makeCluster(magok)
registerDoParallel(cl)

z<-fit_resamples(
  wf,
  resamples = folds,
  metrics = metric_set(mape, rmse, rsq)
)


zx <- tune_grid(
  wf,
  resamples = folds,
  grid = 15,
  metrics = metric_set(mape, rmse, rsq)
  
)

stopCluster(cl)
show_notes(.Last.tune.result)

qq<-z %>% unnest(cols = c(splits, .metrics, .notes))

zy <- map(z$splits, \(s) {
  training_set <- (s) |>
    mutate(e_rate = lead(e_rate, 2)) |>
    filter(time < ymd_hms("2022-08-01 00:00:00"))
  
  testing_set <- (s) |>
    mutate(e_rate = lead(e_rate, 2)) |>
    filter(time >= ymd_hms("2022-08-01 00:00:00"))
  
  prediction_df <- augment(fit(wf, analysis(s)), assessment(s)) |>
    select(time, e_rate, .fitted = .pred) |>
    mutate(mda = mda_vec(e_rate, .fitted, lag = 0))|>
    left_join(leaded_close_df, by = join_by(time))
  
  # starting with 100 euro
  balance_df <- tibble(balance = 100, currency = "eur")
  for (i in 2:(nrow(prediction_df))) { # iteration for calculating the balance
    
    p_balance <- last(balance_df$balance)
    p_currency <- last(balance_df$currency)
    
    if (prediction_df$.fitted[i] < l_bound) { # euro gets cheaper > buy huf
      balance <- ifelse(p_currency == "eur", p_balance * prediction_df$close[i - 1], p_balance)
      currency <- "huf"
      
    } else if (prediction_df$.fitted[i] > u_bound) { # euro gets more expensive > buy euro
      balance <- ifelse(p_currency == "huf", p_balance / prediction_df$close[i - 1], p_balance)
      currency <- "eur"
    } else { # do nothing
      balance <- p_balance
      currency <- p_currency
    }
    
    balance_df <- bind_rows(balance_df, tibble(balance, currency))
  }
  
  bind_cols(prediction_df, balance_df)
  
})
df<-z[[1]]
mean_interval <- function(x) {
  if (sd(x) == 0) {
    tibble(estimate = mean(x), conf.low = mean(x), conf.high = mean(x))
  } else {
    t.test(x, mu = 0) |>
      broom::tidy() |>
      select(estimate, conf.low, conf.high)
  }
}

z |>
  map_dfr(mutate, balance = ifelse(currency == "huf", balance / close, balance), t = row_number()) |>
  group_by(t) |>
  group_modify(\(x, y) mean_interval(x$balance)) |>
  ggplot(aes(t)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "plum4", alpha = .3) +
  geom_line(aes(y = estimate))

#mda maximalizálása a célfüggvény?
#hiperparaméterek, 300/100-as mintákon megnézni, hogy fut-e 