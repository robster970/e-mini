#Close all old graphics windows
graphics.off()

#Move data into new variables to test and filter for RTH
sample.data <- open.hourly
indexTZ(sample.data) <-"America/New_york"
sample.data <- diff(sample.data)
sample.data <- sample.data["T09:30/T16:30"]

#Split the dataset into two even halves
a <- nrow(sample.data)
b <- head(sample.data,a/2)
c <- tail(sample.data, a/2)

#Start the Plotting
win.graph()
par(mfrow=c(1,2))

#Plot histogram for B.Sample
hist(b,30, prob=T, col="green")
lines(density(b, na.rm=TRUE), col="red", lwd=2)

#Plot histogram for C.Sample
hist(c,30, prob=T, col="yellow")
lines(density(c, na.rm=TRUE), col="blue", lwd=2)

win.graph()
par(mfrow=c(1,3))

#Plot PDFs for B & C Sample
plot(density(b, na.rm=TRUE), col="red", lwd=1)
polygon(density(b, na.rm=TRUE), col="red", lwd=1)
lines(density(c, na.rm=TRUE), col="blue", lwd=1)
polygon(density(c, na.rm=TRUE), col="blue", lwd=1)

#Manually calculate CDF's and plot
b.sorted <- sort(coredata(b))
b.n <- nrow(b)
c.sorted <- sort(coredata(c))
c.n <- nrow(c)

if (b.n < c.n) {d.n <- b.n}
if (c.n < b.n) {d.n <- c.n}

plot(b.sorted, (1:b.n)/b.n, type = 's', ylim = c(0, 1), col="red", lwd=1, main="CDF for (b) and (c)")
lines(c.sorted, (1:c.n)/c.n, type = 's', ylim = c(0, 1), col="blue", lwd=1)

#Plot of B and C sample compared to straight line to see whether distributions are broadly equal

diag.1 <- seq(min(coredata(sample.data)),max(coredata(sample.data)),0.1)
diag.2 <- seq(min(coredata(sample.data)),max(coredata(sample.data)),0.1)

plot(b.sorted~head(c.sorted, d.n), col="orange", lwd=2, main="b vs c sample")
lines(diag.1~diag.2)




