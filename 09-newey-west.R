exchage_text_df <- pin_read(.board, "design_df")

m <- floor(0.75 * nrow(exchage_text_df)^(1/3))
NW_VCOV <- sandwich::NeweyWest(lm(price~.-time, exchage_text_df), 
                               lag = m - 1, prewhite = F, 
                               adjust = T)

proba1 <- lm(price~.-time, exchage_text_df)
summary(proba1)
lmtest::coeftest(proba1, vcov = NW_VCOV)

stargazer::stargazer(lmtest::coeftest(proba1, vcov = NW_VCOV), type = "text", out = "test.html")
