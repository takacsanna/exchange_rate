exchage_text_df <- pin_read(.board, "exchage_text_df")
eurhuf_df <- pin_read(.board, "eurhuf_df")
exchage_text_df <- read_csv("exchage_rate_df_log.csv")

leaded_close_df <- eurhuf_df |>
  transmute(
    time = lag(time, 2), close # for easier calculation
  )

set.seed(123)

linear_reg_glm_spec <- linear_reg() %>%
  set_engine('lm')

rand_forest_lm_spec <- rand_forest(
  trees = 50,
  min_n =5
) %>%
  set_engine("ranger") %>%
  set_mode("regression") %>%
  translate()

design_df <- exchage_text_df |>
  mutate(e_rate = lead(e_rate, 2)) |>
  na.omit()

folds <- design_df |>
  rolling_origin(
    initial = 3000,
    assess = 1000,
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
library(parallel)
library(doParallel)
magok <- detectCores()-1
cl <- makeCluster(magok) #számítási klaszter
registerDoParallel(cl)

z<-fit_resamples(
  wf,
  resamples = folds,
  metrics = metric_set(mape, rmse, rsq, mda_df)
)

stopCluster(cl)
show_notes(.Last.tune.result)
