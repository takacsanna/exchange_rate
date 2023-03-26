exchage_text_df <- pin_read(.board, "exchage_text_df")
eurhuf_df <- pin_read(.board, "eurhuf_df")

leaded_close_df <- eurhuf_df |>
  transmute(
    time = lag(time, 2), close # for easier calculation
  )

calculate_balance <- function(start_trading = "2022-08-01 00:00:00", l_bound, u_bound) {

  training_set <- exchage_text_df |>
    mutate(e_rate = lead(e_rate, 2)) |>
    filter(time < ymd_hms(start_trading))

  testing_set <- exchage_text_df |>
    mutate(e_rate = lead(e_rate, 2)) |>
    filter(time >= ymd_hms(start_trading))

  prediction_df <- augment(lm(e_rate ~ . - time, data = training_set), newdata = na.omit(testing_set)) |>
    select(time, e_rate, .fitted, .resid) |>
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
}

calculate_balance(start_trading = "2022-08-01 00:00:00", l_bound = -2, u_bound = 2) |>
  mutate(
    balance = ifelse(currency == "huf", balance / close, balance)
  ) |>
  ggplot() +
  geom_line(aes(time, balance))
