design_df <- pin_read(.board, "design_df") %>%
  group_by(g = (as.numeric(time) - 1) %/% (60 * 5)) %>%
  summarise(
    time = last(time),
    price = last(price),
    across(- (time:price), sum)
  ) %>%
  select(- c(g))

start_trading = "2022-08-01 00:00:00"

testing_set <- design_df |>
  filter(time >= ymd_hms(start_trading)) %>% select(-time)
training_set <- design_df |>
  filter(time < ymd_hms(start_trading)) %>% select(-time)

l_bound = 0.9998
u_bound = 1.0002
svmFit<-e1071::svm(price~., data=training_set, 
           cost = 0.000992594, epsilon = 0.001301303)

summary(svmFit)
fit2<-predict(svmFit, testing_set[,-1])
head(fit2)
testing_set$fitted<-fit2
prediction_df <- testing_set
balance_df <- tibble(balance = 1000000, stock = T)

for (i in 2:(nrow(prediction_df)-1)) {
  # iteration for calculating the balance
  
  p_balance <- last(balance_df$balance)
  p_stock <- last(balance_df$stock)
  
  if (prediction_df$fitted[i+1]/prediction_df$price[i] < l_bound) {
    # stop loss --> sell 
    balance <- 
      ifelse(p_stock == T, p_balance + prediction_df$price[i], p_balance)
    stock <- F
    
  } else if (prediction_df$fitted[i+1]/prediction_df$price[i] > u_bound) {
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
  ggtitle("Az SVM kereskedés eredménye 5 perces aggregációval")
