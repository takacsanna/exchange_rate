library(ggplot2)
library(tidyverse)

arfolyam <- pin_read(.board, "eurhuf_df")
arfolyam <- arfolyam[,c(1,5)]
arfolyam$time <- as.Date(arfolyam$time)

ggplot(arfolyam, aes(x=time)) + geom_line(aes(y=close))

aTSA::adf.test(arfolyam$close)

tseries::adf.test(arfolyam$close)
tseries::kpss.test(arfolyam$close)

#1 diff
d_arfolyam <- diff(arfolyam$close)[-1]
plot(d_arfolyam)
aTSA::adf.test(d_arfolyam)

tseries::adf.test(d_arfolyam)
tseries::kpss.test(d_arfolyam)

log_ar <- log(arfolyam$close)
plot(log_ar)
#kpss és adf alapján is stac

#logdiff

tseries::adf.test(log_ar)
tseries::kpss.test(log_ar)

logdiff <- diff(log_ar)[-1]
plot(logdiff)

tseries::adf.test(logdiff)
tseries::kpss.test(logdiff)
#szóval nyilván itt is 
