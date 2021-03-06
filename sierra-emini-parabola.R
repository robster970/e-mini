#########################Changes################################################
#	26/09/2016	Version 2.0 								 #
#	Re-factored to ingest from Sierra Charts historic data			 #
#												 	 #
#													 #
#													 #
################################################################################

# Include all libraries for time series analysis
library(zoo)
library(xts)
library(tseries)

#set some variables
Sys.setenv(TZ="Europe/London")# Used only for IB feed
get.data <- 1 ; # This connects to live feed when set to (1) and does not connect but uses data in workspace when (0)
ma.size.hourly <- 30
ma.size.daily <- 10
pause <- 2; # Sets a pause between collecting data from IB to stop IB from having a little paddy
min.dataset <- 10
sysdatetime <- format(Sys.time(), "%Y%m%d %H:%M:%S")
hist.gran <- 40

#Close all old graphics windows
graphics.off()

# Check to see that the get_data parameter is set to 1 to get live data. Only applicable for IB feed but kept in.
if (get.data == 1){

# Read in Sierra csv file as a Zoo object where the index is created from date and time column
esdata.hourly <- read.zoo("C:\\SierraChart\\Data\\ESH18.scid_BarData.txt", sep = ",", index = 1:2, header=TRUE)

}

#Convert Zoo object to XTS object to keep IB manipulation intact.
esdata.hourly <- as.xts(esdata.hourly)

#Now check the number of rows before doing any processing
rows.returned.hourly <- nrow(esdata.hourly)
rows.returned.daily <- nrow(esdata.daily)

# Read in the data to variables.
open.hourly <-xts(esdata.hourly$Open)
close.hourly <-xts(esdata.hourly$Close)
high.hourly <-xts(esdata.hourly$High)
low.hourly <-xts(esdata.hourly$Low)
mid.hourly <- (high.hourly+low.hourly)/2
ma.hourly <-rollmean(mid.hourly, ma.size.hourly, align="right")
lagged.ma.hourly <-lag(ma.hourly,k=-(ma.size.hourly/2))


f.max <- function(x) {
max(x, na.rm=TRUE)
}

f.min <- function(x) {
min(x, na.rm=TRUE)
}

f.mean <- function(x) {
mean(x, na.rm=TRUE)
}


########################################################################
# Working out the initial opening volume from initial M30 candle       #
########################################################################
# Plotting the profile of how this changes and distrbution             #
# Change rth.time if you want to see different distributions for       #
# different parts of the day.							     #
########################################################################

# Set filtering parameters
iterations <- 1
contract.rollover.date <-"2016-09-16" # This is the date that the contract actually switched

while (iterations < 4) {

win.graph()
par(mfrow=c(1,2))

if (iterations == 1){

filter.time.1 <-"T09:30:00/T09:59:00"
filter.time.2 <-"T09:30:00/T10:29:00"
plot.title <- "Opening Volume"
plot.colour <-"Orange"

}

else if (iterations == 2) {

filter.time.1 <-"T15:30:00/T16:29:00"
filter.time.2 <-"T15:00:00/T16:29:00"
plot.title <-"Closing Volume"
plot.colour <-"Cyan"

}

else {

filter.time.1 <-"T16:30:00/T23:59:00"
filter.time.2 <-"T00:00:00/T09:29:00"
plot.title <-"Overnight Volume"
plot.colour <-"Grey"

}

# Don't have days with low volume before full rollover
start.date <- paste(contract.rollover.date,"/")

#Filter out dataset for contract rollover
eshourly.volume.alldays <- esdata.hourly[,5]
eshourly.volume.days <- eshourly.volume.alldays[start.date]
eshourly.volume.days <- eshourly.volume.alldays

#Adjust index to NYSE and move volume data into its own object with time filter
#indexTZ(eshourly.volume.days) <-"America/New_york" # Note this is not required for Sierra feed which is already time adjusted.

#Calculate the rolling volume data
eshourly.volume.open.1 <- apply.daily((eshourly.volume.days[filter.time.1]),sum)
eshourly.volume.open.2 <- apply.daily((eshourly.volume.days[filter.time.2]),sum)

#Calculate where the current data sits in the historic distribution
perc.rank <- function(x, xo)  length(x[x <= xo])/length(x)*100;
eshourly.volume.open.percentile <- perc.rank(eshourly.volume.open.2, as.numeric(coredata(tail(eshourly.volume.open.2,1))))

#Plot everything
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

target.start.date <- "2017-09-25 11:00:00"
#target.start.date <- "2017-09-05 13:00:00"
mid.hourly.time.index <- as.vector(index(mid.hourly))
start.point <- match(as.POSIXct(target.start.date), mid.hourly.time.index)

# Only use this if backtesting within a dataset
target.end.date <- "2016-04-19 20:00:00"
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
lines(price-fit, col="orange")
grid(NULL,NULL,lwd=1)
lines(rollmean(price-fit,ma.factor), col="purple", lwd=2)
lines(rollapply((price-fit), ma.factor,sd)+min(price-fit), col="red", lwd=2)

title.5=paste("Point plot on 30min","\n","| Gradient:",round(current.gradient,3)," | EoT:",round(end.of.turn,2)," | When:",round(point.of.turn-max(time),2),"|\n", "| Last price, fit,(price-fit):",round(tail(price,1),2),",",round(tail(fit,1),2),",",round(tail((price-fit),1),2)," |\n")
plot(price, main=title.5)
grid(NULL,NULL,lwd=1)
lines(time, fit, lwd=2, col="red")

#########################################################################
# Residual analysis to look at how the fitted line and volatility       #
# is changing as the move begins and ends      			            #
#########################################################################

source <- mid.hourly[start.point:rows.returned.hourly]
residuals <- data.frame(index(source), residuals(lm.r))
residuals.xts <- xts(residuals[,-1], order.by=residuals[,-2])
residuals.min <- apply.daily(residuals.xts, min)
residuals.max <- apply.daily(residuals.xts, max)
residuals.mean <- apply.daily(residuals.xts, mean)
residuals.sd <- apply.daily(residuals.xts, sd)
residual.compare <- data.frame(residuals.min, coredata(residuals.max))

win.graph()
par(mfrow=c(1,1))
title.residual <- paste("Residuals min:", round(tail(residual.compare[,1],1),2),"\n Residuals max:", round(tail(residual.compare[,2],1),2),"\n ")
plot(fitted(lm.r)~residuals(lm.r), main=title.residual)
lines(fitted(lm.r)~residuals(lm.r), col="blue")
abline(v=0, col="purple")

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
residual.compare,"\n")
plot(fitted(lm.r)~residuals(lm.r), main=title.residual)
lines(fitted(lm.r)~residuals(lm.r), col="blue")
abline(v=0, col="purple")

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
residual.compare