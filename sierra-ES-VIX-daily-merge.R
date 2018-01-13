# Include all libraries for time series analysis
library(zoo)
library(xts)
library(tseries)

atrStopFactor = 0.68

#Close all old graphics windows
graphics.off()

# Read in Sierra csv file as a Zoo object where the index is created from date and time column
esdata <- read.zoo("C:\\SierraChart\\Data\\ESH18-daily.txt", sep = ",", index.column=1:2, header=TRUE)
vixdata <- read.zoo("C:\\SierraChart\\Data\\VIX-daily.txt", sep = ",", index.column=1:2, header=TRUE)

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

# Select rows for investigation
entryNDistSift <- subset(cleanData, cleanData$NDist.vixdata > 0.841)
entryPDiffSift <- subset(entryNDistSift, entryNDistSift$PDiff.vixdata > -0.03 & entryNDistSift$PDiff.vixdata < 0.03)

# Extract VIX and PDiff values and make numeric for subsequent manipulation
lastVixEntry <- as.numeric(tail(entryPDiffSift$Last.vixdata,1))
lastPDiffEntry <- as.numeric(tail(entryPDiffSift$PDiff.vixdata,1))
lastEntryDate <- tail(entryPDiffSift[,0],1)

exitNDistSift <- subset(cleanData, cleanData$NDist.vixdata < 0.159)
exitPDiffSift <- subset(exitNDistSift, exitNDistSift$PDiff.vixdata > -0.03 & exitNDistSift$PDiff.vixdata < 0.03)

if (lastPDiffEntry > 0 ) {
cat("High NDist NOT confirmed on ")
print(lastEntryDate)
cat("by negative PDiff - wait for negative PDiff on day after signal\n")
cleanData[which(cleanData$Last.vixdata == lastVixEntry) + c(1,0) ,]
} else {
cat("High NDist confirmed by negative PDiff\n")
tail(entryPDiffSift, n=2)
}

tail(exitPDiffSift, n=2)
tail(cleanData, n=3)


