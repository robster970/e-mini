
#########################Changes################################################
#16/02/2015	Version 1.0 including (1) Plot of daily analysis and H/L assessment#
#		(2) Plot of M30 with H/L assessment						 #
#		(3) Volume analysis of opening 30mins of RTH session to look at 	 #
#		propensity for the day to be a high or low volume day			 #
#		(4) Quadratic curve fitting to look at buy/sell signals on M30	 #
################################################################################

# Include all libraries
library(IBrokers)
library(zoo)
library(xts)
library(tseries)
library(lubridate)

#set some variables
Sys.setenv(TZ="Europe/London")
get.data <- 1 ; # This connects to live feed when set to (1) and does not connect but uses data in workspace when (0)
ma.size.hourly <- 30
ma.size.daily <- 10
pause <- 2; # Sets a pause between collecting data from IB to stop IB from having a little paddy

min.dataset <- 10
sysdatetime <- format(Sys.time(), "%Y%m%d %H:%M:%S")
hist.gran <- 40
contract <-"ESZ5"

#Close all old graphics windows
graphics.off()

# Check to see that the get_data parameter is set to 1 to get live data
if (get.data == 1){
#  Connect to Interactive Brokers via TWS
tws <- twsConnect()
print("Connected to TWS Live feed for Hourly")

# Get some parameters to check connection and display on screen
tws
reqCurrentTime(tws)
serverVersion(tws)

# Get the market data for ES
# Define the contract - THIS NEEDS TO CHANGE EVERY QUARTER
es <- twsFuture("ES", "GLOBEX", "201512")

#Get the market data
esdata.hourly <- reqHistoricalData(tws, es, endDateTime=sysdatetime, barSize="30 mins", duration="34 D", useRTH = "0")
#esdata.hourly <- reqHistoricalData(tws, es, endDateTime=sysdatetime, barSize="3 mins", duration="3 D", useRTH = "0")

#Disconnect
twsDisconnect(tws)

Sys.sleep(pause)

#  Connect to Interactive Brokers via TWS
tws <- twsConnect()
print("Connected to TWS Live feed for Daily")

# Get some parameters to check connection
tws
reqCurrentTime(tws)
serverVersion(tws)

# Get the market data for ES
# Define the contract - THIS NEEDS TO CHANGE EVERY QUARTER
es <- twsFuture("ES", "GLOBEX", "201512")

#Get the market data
esdata.daily <- reqHistoricalData(tws, es, endDateTime=sysdatetime, barSize="1 day", duration="9 M", useRTH = "0")

#Disconnect
twsDisconnect(tws)

}

#Now check the number of rows before doing any processing
rows.returned.hourly <- nrow(esdata.hourly)
rows.returned.daily <- nrow(esdata.daily)

#Carve up the dataset
open.hourly <-xts(esdata.hourly$ESZ5.Open)
close.hourly <-xts(esdata.hourly$ESZ5.Close)
high.hourly <-xts(esdata.hourly$ESZ5.High)
low.hourly <-xts(esdata.hourly$ESZ5.Low)
mid.hourly <- (high.hourly+low.hourly)/2
ma.hourly <-rollmean(mid.hourly, ma.size.hourly, align="right")
lagged.ma.hourly <-lag(ma.hourly,k=-(ma.size.hourly/2))

open.daily <-xts(esdata.daily$ESZ5.Open)
close.daily <-xts(esdata.daily$ESZ5.Close)
high.daily <-xts(esdata.daily$ESZ5.High)
low.daily <-xts(esdata.daily$ESZ5.Low)
mid.daily <- (high.daily+low.daily)/2
ma.daily <-rollmean(mid.daily, ma.size.daily, align="right")
lagged.ma.daily <-lag(ma.daily,k=-(ma.size.daily/2))

f.max <- function(x) {
max(x, na.rm=TRUE)
}

f.min <- function(x) {
min(x, na.rm=TRUE)
}

f.mean <- function(x) {
mean(x, na.rm=TRUE)
}

#win.graph()
#par(mfrow=c(1,1))

#Plot the Daily Candle, MA and Rotation
#title.1=paste("Candle plot of Daily","\n")
#plot(esdata.daily, main=title.1, type='candles', bar.col="black", candle.col="black")
#lines(lagged.ma.daily, col="red", lwd="1")

#Calculate the rolling High/Low for the daily prices
#daily.lows <- rollapply(low.daily, ma.size.daily*2, f.min)
#daily.highs <- rollapply(high.daily, ma.size.daily*2, f.max)

#Draw the High/Low boundaries
#lines(daily.lows, col="blue", lwd="1")
#lines(daily.highs, col="purple", lwd="1")

#daily.low.freq <- table(daily.lows)
#daily.high.freq <- table(daily.highs)

#Create new window for composite view of price and volatility
#win.graph()
#par(mfrow=c(1,1))

#Plot the 30Min Candle, MA and Rotation
#title.2=paste("Candle plot of 30min","\n","Moving Average:",ma.size.hourly,"\n")
#plot(esdata.hourly, main=title.2, type='candles', bar.col="black", candle.col="black")
#lines(lagged.ma.hourly, col="red", lwd="1")

#hourly.lows <- rollapply(low.hourly, ma.size.hourly*2, f.min)
#hourly.highs <- rollapply(high.hourly, ma.size.hourly*2, f.max)

#lines(hourly.lows, col="blue", lwd="1")
#lines(hourly.highs, col="purple", lwd="1")

#hourly.low.freq <- table(hourly.lows)
#hourly.high.freq <- table(hourly.highs)
#head(sort(hourly.low.freq, decreasing=TRUE),10)
#head(sort(hourly.high.freq, decreasing=TRUE),10)

########################################################################
# Working out the initial opening volume from initial M30 candle       #
########################################################################
# Plotting the profile of how this changes and distrbution             #
# Change rth.time if you want to see different distributions for       #
# different parts of the day.							     #
########################################################################

# Set filtering parameters
iterations <- 1
contract.rollover.date <-"2015-09-18"

while (iterations < 4) {

win.graph()
par(mfrow=c(1,2))

if (iterations == 1){

filter.time.1 <-"T09:30/T09:59"
filter.time.2 <-"T09:30/T10:29"
plot.title <- "Opening Volume"
plot.colour <-"Orange"

}

else if (iterations == 2) {

filter.time.1 <-"T15:30/T16:29"
filter.time.2 <-"T15:00/T16:29"
plot.title <-"Closing Volume"
plot.colour <-"Cyan"

}

else {

filter.time.1 <-"T16:30/T23:59"
filter.time.2 <-"T00:00/T09:29"
plot.title <-"Overnight Volume"
plot.colour <-"Grey"

}

# Don't have days with low volume before full rollover
start.date <- paste(contract.rollover.date,"/")

#Filter out dataset for contract rollover
eshourly.volume.alldays <- esdata.hourly[,5]
eshourly.volume.days <- eshourly.volume.alldays[start.date]

#Adjust index to NYSE and move volume data into its own object with time filter
indexTZ(eshourly.volume.days) <-"America/New_york"
eshourly.volume.open.1 <- apply.daily((eshourly.volume.days[filter.time.1]),sum)
eshourly.volume.open.2 <- apply.daily((eshourly.volume.days[filter.time.2]),sum)
 
perc.rank <- function(x, xo)  length(x[x <= xo])/length(x)*100;
eshourly.volume.open.percentile <- perc.rank(eshourly.volume.open.2, as.numeric(coredata(tail(eshourly.volume.open.2,1))))

colours <- c("black", "red")
title.3=paste("Plot of", plot.title,"\n", index(tail(eshourly.volume.open.2,1)),"\n Vol: ",coredata(tail(eshourly.volume.open.2,1)),", Percentile: ",round(eshourly.volume.open.percentile,0),"%")
plot(eshourly.volume.open.2, main=title.3, ylim=c(min(eshourly.volume.open.1),max(eshourly.volume.open.2)))
lines(eshourly.volume.open.1, col="red")
#legend("topleft", inset=.05,c(paste(filter.time.2,"\n"), paste(filter.time.1,"\n")), fill=colours, horiz=TRUE)

title.4=paste("Histogram of",plot.title,"\n","Mean:",round(mean(eshourly.volume.open.2),0),"\n","SD:",round(sd(eshourly.volume.open.2),0),"\n")
hist(eshourly.volume.open.2, 10, prob=T, col=plot.colour, main=title.4)
lines(density(eshourly.volume.open.2, na.rm=TRUE), col="red", lwd=2)
abline(v=mean(eshourly.volume.open.2), col="blue", lwd="2")
abline(v=(mean(eshourly.volume.open.2)+sd(eshourly.volume.open.2)), col="purple", lwd="1", lty=2)
abline(v=(mean(eshourly.volume.open.2)-sd(eshourly.volume.open.2)), col="purple", lwd="1", lty=2)
abline(v=(mean(eshourly.volume.open.2)+2*sd(eshourly.volume.open.2)), col="purple", lwd="1", lty=2)
abline(v=(mean(eshourly.volume.open.2)-2*sd(eshourly.volume.open.2)), col="purple", lwd="1", lty=2)

iterations <- iterations+1

}

########################################################################
# Fitting quadratic to M30 data to look for entry signals (fade)       #
# Gradient should be flat to -ve. R^2 should be high			     #
########################################################################
# Identify start date on Sierra and populate target.start.date         #
# Identify end date on Sierra and populate target.end.date if          #
# backtesting. Then uncomment rows.returned.hourly to pick up end date.#
# Comment this line out if running in real time.                       #
########################################################################

target.start.date <- "2015-11-15 23:00:00"
#target.start.date <- "2015-11-16 14:30:00"
mid.hourly.time.index <- as.vector(index(mid.hourly))
start.point <- match(as.POSIXct(target.start.date), mid.hourly.time.index)

# Only use this if backtesting within a dataset
target.end.date <- "2015-10-22 14:30:00"
mid.hourly.time.index <- as.vector(index(mid.hourly))
end.point <- match(as.POSIXct(target.end.date), mid.hourly.time.index)

# Next line only used when looking at historic data
#rows.returned.hourly <- end.point
price <- coredata(mid.hourly[start.point:rows.returned.hourly])

time <- c(1:nrow(price));#time

lm.r <- lm(price~time+I(time^2));lm.r
coefficients <- coef(lm.r)
c.term <- coefficients[1];c.term
b.term <- coefficients[2];b.term
a.term <- coefficients[3];a.term

fit <- c.term + b.term*time + a.term*time^2;fit

current.gradient <- (2*a.term*nrow(price)) + b.term
point.of.turn <- (-b.term)/(2*a.term);point.of.turn
time.of.turn <-  index(mid.hourly[start.point])+point.of.turn*60*30
end.of.turn <- (a.term*point.of.turn^2)+(b.term*point.of.turn)+c.term
#time.of.turn <- mid.hourly[start.point+point.of.turn,];time.of.turn

win.graph()
par(mfrow=c(2,2))
plot(lm.r)

win.graph()
par(mfrow=c(1,2))

#Plot the 30min area of inspection
title.5=paste("Point plot on 30min","\n","| Gradient:",round(current.gradient,3)," | EoT:",round(end.of.turn,2)," | When:",round(point.of.turn-max(time),2),"|\n", "| Last price, fit,(price-fit):",round(tail(price,1),2),",",round(tail(fit,1),2),",",round(tail((price-fit),1),2)," |\n")
plot(price, main=title.5)
lines(time, fit, lwd=2, col="red")

#Plot the LOESS fit line
Data <- data.frame(time, price)
loess.r <- loess(price ~ time, Data)
lines(Data$time, predict(loess.r), col ="blue", lwd=2, lty=2)

price.fit.percentile <-perc.rank(price-fit, as.numeric(coredata(tail(price-fit,1))))

title.6=paste("Distribution of (price-fit)","\n","| Mean:",mean(price-fit)," |\n", "| STDev:",round(sd(price-fit),2)," | Percentile(",round(tail((price-fit),1),2),"):",round(price.fit.percentile,2),"\n")
hist(price-fit, 30, prob=T, col="green", main=title.6)
lines(density(price-fit, na.rm=TRUE), col="red", lwd=2)

win.graph()
par(mfrow=c(1,2))

# 5 is a good fit for small moves of 2-4 days, 20 for larger multi-day moves
ma.factor <- 5
title.7=paste("(price-fit), ",ma.factor,"MA& SD (price-fit)","\n","|Max (",ma.factor,"MA) Deviation:",round(max(rollmean(price-fit,ma.factor)),2),"|\n","|Max (",ma.factor,"MA)+fit:",round(tail(fit,1)+max(rollmean(price-fit,ma.factor)),2),"| 2SD(price-fit): ",(2*round(sd(price-fit),2)),"|\n")
plot(price-fit, col="orange", main=title.7)
grid(NULL,NULL,lwd=1)
lines(rollmean(price-fit,ma.factor), col="purple", lwd=2)
lines(rollapply((price-fit), ma.factor,sd)+min(price-fit), col="red", lwd=2)

title.5=paste("Point plot on 30min","\n","| Gradient:",round(current.gradient,3)," | EoT:",round(end.of.turn,2)," | When:",round(point.of.turn-max(time),2),"|\n", "| Last price, fit,(price-fit):",round(tail(price,1),2),",",round(tail(fit,1),2),",",round(tail((price-fit),1),2)," |\n")
plot(price, main=title.5)
grid(NULL,NULL,lwd=1)
lines(time, fit, lwd=2, col="red")

#high.price <- coredata(high.hourly[start.point:rows.returned.hourly])
#low.price <- coredata(low.hourly[start.point:rows.returned.hourly])
#atr <- rollapply(high.price-low.price, 14, f.mean)
#plot(atr)

last <- tail(esdata.hourly,2)
sysdatetime
last[2,0]
cat("Last 2 Closes", last[1,4]," & ", last[2,4],"\n")
summary(lm.r)

cat("last (price) :",tail((price),1),"\nlast (fit) :",tail((fit),1),"\nlast (price-fit) :",tail((price-fit),1),"\n")
cat("max (",ma.factor,"MA) Deviation :",round(max(rollmean(price-fit,ma.factor)),2),"\nmax (",ma.factor,"MA)+fit:",round(tail(fit,1)+max(rollmean(price-fit,ma.factor)),2),"\n2SD(price-fit): ",(2*round(sd(price-fit),2)),"\n")


