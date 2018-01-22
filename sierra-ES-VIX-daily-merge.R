#Last updated 2018/01/21
# Include all libraries for time series analysis
library(zoo)
library(xts)
library(tseries)
#Clear all variables
rm(list=ls())

atrStopFactor = 0.70

#Close all old graphics windows
graphics.off()


# Read in Sierra csv file as a Zoo object where the index is created from date and time column
esdata <- read.zoo("C:\\SierraChart\\Data\\ESH18.dly_BarData.txt", sep = ",", index.column=1:2, header=TRUE)
vixdata <- read.zoo("C:\\SierraChart\\Data\\$VIX.dly_BarData.txt", sep = ",", index.column=1:2, header=TRUE)

# Added in to handle working away from the windows server
#setwd("/home/robster970/repo/e-mini/sierrafiles")
#esdata <- read.zoo("ESH18.dly_BarData.txt", sep = ",", index.column=1:2, header=TRUE)
#vixdata <- read.zoo("$VIX.dly_BarData.txt", sep = ",", index.column=1:2, header=TRUE)

# Merge ES and VIX data into a single dataframe and the remove columns not required
mergeddata = merge(vixdata, esdata)
cleanData = subset(mergeddata, select = c(4,10,11,12,13))

# Add calculations associated with VIX
cleanData$Avg.vixdata <- rollapply(cleanData$Last.vixdata, width = 10, mean, fill=NA, align="right", na.rm=TRUE)
cleanData$STDev.vixdata <- rollapply(cleanData$Last.vixdata, width = 10, sd, fill=NA, align="right", na.rm=TRUE)
cleanData$NDist.vixdata <- pnorm(cleanData$Last.vixdata, cleanData$Avg.vixdata, cleanData$STDev.vixdata)
cleanData$PDiff.vixdata <- diff(cleanData$Last.vixdata)/lag(cleanData$Last.vixdata, k=-1)

# Add calculations associated with ES
cleanData$TR.esdata <- (cleanData$High.esdata-cleanData$Low.esdata)
cleanData$ATR.esdata <- rollapply(cleanData$TR.esdata, width = 10, mean, fill=NA, align="right", na.rm=TRUE)
cleanData$Stop.atr <- cleanData$ATR.esdata * (1+atrStopFactor)

# Select rows for investigation - Long Entry
entrySift <- subset(cleanData, cleanData$NDist.vixdata > 0.841 & cleanData$PDiff.vixdata > -0.03 & cleanData$PDiff.vixdata < 0.03)

# Extract VIX and PDiff values and make numeric for subsequent manipulation of last valid entry
#lastVixEntry <- as.numeric(tail(entrySift$Last.vixdata,1))
#lastPDiffEntry <- as.numeric(tail(entrySift$PDiff.vixdata,1))
#lastEntryDate <- as.Date(tail(entrySift[,0],1))

# Select rows for investigation - Long Exit
exitSift <- subset(cleanData, cleanData$NDist.vixdata < 0.159 & cleanData$PDiff.vixdata > -0.03 & cleanData$PDiff.vixdata < 0.03)

#############################################################################

#Trying to ascertain entry signal automatically

entryCandidates <- tail(cleanData, n=2)
pdiffSignal.1 <- as.numeric(entryCandidates$PDiff.vixdata[1,])
pdiffSignal.2 <- as.numeric(entryCandidates$PDiff.vixdata[2,])
ndistSignal.1 <- as.numeric(entryCandidates$NDist.vixdata[1,])
ndistSignal.2 <- as.numeric(entryCandidates$NDist.vixdata[2,])

if (ndistSignal.2 > 0.841 & pdiffSignal.2 > 0 & pdiffSignal.2 < 0.03) {
  cat("LONG ENTRY confirmed by high NDist & negative PDiff on same day of signal\n")
  tail(entryCandidates, n=1)
} else {
  
  if (ndistSignal.1 > 0.841 & pdiffSignal.1 > -0.03 & pdiffSignal.1 < 0.03 & pdiffSignal.1 > -0.03 & pdiffSignal.2 < 0) {
    cat("LONG ENTRY confirmed by negative PDiff on following day after inital signal\n")
    tail(entryCandidates, n=2)
  } else {
    
    cat("LONG ENTRY NOT confirmed by negative PDiff on following day after inital signal\n")
    tail(entryCandidates, n=2)
  }
}

##########################################################################

#Print stuff out to manually eyeball it for the time being
tail(entrySift, n=2)
tail(exitSift, n=2)
tail(cleanData, n=3)