library(forecast)

design_df <- pin_read(.board, "design_df") %>% 
  select(time, price)

start_trading = "2022-08-01 00:00:00"

training_set <- design_df |>
  filter(time < ymd_hms(start_trading))

testing_set <- design_df |>
  filter(time >= ymd_hms(start_trading))


training_ts <- xts::xts(training_set$price, training_set$time)
test_ts <- xts::xts(testing_set$price, testing_set$time)

fit <- auto.arima(training_ts)
summary(fit)

refit <- Arima(test_ts, model=fit)
testing_set$fc <- (fitted(refit))
testing_set$resid <- refit$residuals

prediction_df <- testing_set

prediction_df %>% 
  ggplot(aes(x=time, y=resid)) + 
  geom_line()

testing_set$percent <- testing_set$price / lag(testing_set$price)
hist(testing_set$percent, xlim = c(0.9998,1.0002), breaks = 1500)
p<-ggplot(testing_set, aes(x=percent))+geom_histogram(bins = 1500)+xlim(c(0.999,1.001))
p2<-p+ggbreak::scale_y_cut(c(500))+theme_classic()+xlab("Százalékos változás")+ylab("Darab")+
  ggtitle("A BUX-index százalékos változásának eloszlása")

p2
ggplot(testing_set, aes(time, percent))+geom_line()

balance_df <- tibble(balance = 1000000, stock = T)
l_bound = 0.9998
u_bound = 1.0002

for (i in 2:(nrow(prediction_df)-1)) {
  # iteration for calculating the balance
  
  p_balance <- last(balance_df$balance)
  p_stock <- last(balance_df$stock)

  if (prediction_df$fc[i+1]/prediction_df$price[i] < l_bound) {
    # stop loss --> sell 
    balance <- 
      ifelse(p_stock == T, p_balance + prediction_df$price[i], p_balance)
    stock <- F
    
  } else if (prediction_df$fc[i+1]/prediction_df$price[i] > u_bound) {
    # expected rise in price --> buy
    balance <- ifelse(p_stock == F, p_balance - prediction_df$price[i], p_balance)
    stock <- T
  } else {
    # do nothing
    balance <- p_balance
    stock <- p_stock
  }
  
  balance_df <- bind_rows(balance_df, tibble(balance, stock))
}
balance_df$index <- 1:nrow(balance_df)  
ggplot(balance_df, aes(x=index, y=balance))+geom_line()+theme_classic()+
  xlab("Időpont")+ylab("Egyenleg")+
  ggtitle("Az ARIMA kereskedés eredménye")

