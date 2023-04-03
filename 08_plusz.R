exchage_text_df <- pin_read(.board, "exchage_text_df")
eurhuf_df <- pin_read(.board, "eurhuf_df")

leaded_close_df <- eurhuf_df |>
  transmute(
    time = lag(time, 2), close # for easier calculation
  )

set.seed(123)

linear_reg_glm_spec <- linear_reg() %>%
  set_engine('lm')

rand_forest_lm_spec<-rand_forest() %>%  
      set_engine("ranger") %>% 
      set_mode("regression") %>% 
      translate()

design_df <- exchage_text_df |>
  mutate(e_rate = lead(e_rate, 2)) |>
  na.omit() #%>% 
  select(-starts_with("blikk_"))

folds <- design_df |>
  rolling_origin(
    initial = 3000,
    assess = 1000,
    cumulative = FALSE
  )

rec <- recipe(e_rate ~ ., data = design_df) |>
  step_rm(time)

wf <- workflow(rec, rand_forest_lm_spec)

l_bound = -0.02
u_bound = 0.02

#MDA

mda_vec <- function(truth, estimate, na_rm = TRUE, lag = 1) {
  
  MDirAcc <- function(truth, estimate, lag=1) {
    return( mean(sign(diff(truth, lag=lag))==sign(diff(estimate, lag=lag))) )
  }
  
  
  metric_vec_template(
    metric_impl = MDirAcc,
    truth = truth, 
    estimate = estimate,
    na_rm = na_rm,
    lag = 1,
    cls = "numeric"
  )
  
}

mda_vec(x[[1]]$e_rate, x[[1]]$.fitted)

#pred
x <- map(folds$splits, \(s) {
  prediction_df <- augment(fit(wf, analysis(s)), assessment(s)) |>
    select(time, e_rate, .fitted = .pred) |>
    mutate(mda = mda_vec(e_rate, .fitted))|>
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


mean_interval <- function(x) {
  if (sd(x) == 0) {
    tibble(estimate = mean(x), conf.low = mean(x), conf.high = mean(x))
  } else {
    t.test(x, mu = 0) |>
      broom::tidy() |>
      select(estimate, conf.low, conf.high)
  }
}

x |>
  map_dfr(mutate, balance = ifelse(currency == "huf", balance / close, balance), t = row_number()) |>
  group_by(t) |>
  group_modify(\(x, y) mean_interval(x$balance)) |>
  ggplot(aes(t)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "plum4", alpha = .3) +
  geom_line(aes(y = estimate))

x |>
  map_dfr(mutate, t = row_number()) |>
  group_by(t)
  
x %>% 
  ggplot(aes(y = mda))+
  geom_line()


#NW OLS-sel

y <- map(folds$splits, \(s) {
  prediction_df <- augment(fit(wf, analysis(s)), assessment(s)) |>
    select(time, e_rate, .fitted = .pred) %>% 
    mutate(.res = e_rate-.fitted)|>
    left_join(leaded_close_df, by = join_by(time))})

v <- (y[[1]]$e_rate - mean(y[[1]]$e_rate)) * y[[1]]$.res
var_beta_hat <- 1/length(v) * (1/(length(v)-2) * sum((y[[1]]$e_rate - mean(y[[1]]$e_rate))^2 * y[[1]]$.res^2) ) / 
  (1/length(v) * sum((y[[1]]$e_rate - mean(y[[1]]$e_rate))^2))^2
m <- floor(0.75 * length(v)^(1/3))
acf_c <- function(x, j) {
  return(
    t(x[-c(1:j)]) %*% na.omit(lag(x, j)) / t(x) %*% x
  )
}
f_hat_T <- 1 + 2 * sum(
  (m - 1:(m-1))/m * sapply(1:(m - 1), function(i) acf_c(x = v, j = i))
) 
sqrt(var_beta_hat * f_hat_T)

ols<-lm(e_rate ~ ., data = design_df)

NW_VCOV <- sandwich::NeweyWest(lm(e_rate ~ ., data = design_df), 
                               lag = m - 1, prewhite = F, 
                               adjust = T)
lmtest::coeftest(ols, vcov = NW_VCOV)
summary(ols)
options(max.print=1000000)