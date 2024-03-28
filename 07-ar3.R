library(forecast)

exchage_text_df <- pin_read(.board, "design_df") %>% 
  select(time, price)
eurhuf_df <- pin_read(.board, "design_df") %>% 
  select(time, price)

start_trading = "2022-08-01 00:00:00"

training_set <- exchage_text_df |>
  filter(time < ymd_hms(start_trading))

testing_set <- exchage_text_df |>
  filter(time >= ymd_hms(start_trading))


training_ts <- xts::xts(training_set$price, training_set$time)
attr(training_ts, "frequency") <- 60
training_ts <- as.ts(training_ts)

teljes_ts <- xts::xts(exchage_text_df$price, exchage_text_df$time)
attr(teljes_ts, "frequency") <- 60
teljes_ts <- as.ts(teljes_ts)

test_ts <- xts::xts(testing_set$price, testing_set$time)
attr(test_ts, "frequency") <- 60
test_ts <- as.ts(test_ts)

fit <- auto.arima(training_ts)
summary(fit)

refit <- Arima(test_ts, model=fit)
testing_set$fc <- (fitted(refit))
testing_set$resid <- refit$residuals

prediction_df <- testing_set

prediction_df %>% 
  ggplot(aes(x=time, y=resid)) + 
  geom_line()

testing_set$szazalek <- testing_set$price / lag(testing_set$price)
ggplot(testing_set, aes(x=time, y=szazalek))+geom_line()


balance_df <- tibble(balance = 1000000, stock = T)
l_bound = 0.9975
u_bound = 1.0025

for (i in 2:(nrow(prediction_df)-1)) {
  # iteration for calculating the balance
  
  p_balance <- last(balance_df$balance)
  p_stock <- last(balance_df$stock)

  if (prediction_df$fc[i+1]/prediction_df$price[i] < l_bound) {
    # stop loss --> sell 
    balance <- 
      ifelse(stock == T, p_balance + prediction_df$price[i], p_balance)
    stock <- F
    
  } else if (prediction_df$fc[i+1]/prediction_df$price[i] > u_bound) {
    # expected rise in price --> buy
    balance <- ifelse(stock == F, p_balance - prediction_df$price[i], p_balance)
    stock <- T
  } else {
    # do nothing
    balance <- p_balance
    stock <- p_stock
  }
  
  balance_df <- bind_rows(balance_df, tibble(balance, stock))
}
balance_df$index <- 1:nrow(balance_df)  
ggplot(balance_df, aes(x=index, y=balance))+geom_line()
