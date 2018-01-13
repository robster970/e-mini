esdata.daily <- read.zoo("C:\\SierraChart\\Data\\ESZ16.dly_BarData.txt", sep = ",", index = 1:2, header=TRUE)
vix.daily <- read.zoo("C:\\SierraChart\\Data\\$VIX.dly_BarData.txt", sep = ",", index = 1:2, header=TRUE)

esdata.diff <- tail(diff(esdata.daily$Last)/esdata.daily$Last,20)
vixdata.diff <- tail(diff(vix.daily$Last)*100/vix.daily$Last,20)

Data <- data.frame(esdata.daily[,0], diff(esdata.daily$Last))
loess.r <- loess(diff(esdata.daily$Last) ~ esdata.daily[,0], Data)
lines(Data$time, predict(loess.r), col ="blue", lwd=2, lty=2)
