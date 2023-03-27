exchage_text_df <- pin_read(.board, "exchage_text_df")
eurhuf_df <- pin_read(.board, "eurhuf_df")

leaded_close_df <- eurhuf_df |>
  transmute(
    time = lag(time, 2), close # for easier calculation
  )

set.seed(123)

linear_reg_glm_spec <- linear_reg() %>%
  set_engine('lm')

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
  step_rm(time)

wf <- workflow(rec, linear_reg_glm_spec)

l_bound = -2
u_bound = 2


fit(wf, analysis(folds$splits[[1]]))

map(head(folds$splits), \(s) {
  prediction_df <- augment(fit(wf, analysis(s)), assessment(s)) |>
    select(time, e_rate, .fitted = .pred) |>
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
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "red", alpha = .3) +
  geom_line(aes(y = estimate))
