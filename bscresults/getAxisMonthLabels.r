# results a list of vectors to use with a month axis.
# ticks1 is the position of major tick marks
# ticks2 is the position of minor tick marks
# label2 is the label used at minor tick marks (in the middle of the month)

# implementation example (not tested yet):

# lab <- bscresults.getAxisMonthLabels(1, 366)
# plot(y ~ x, xlab=NA, ylab=NA, data=table1, axes=FALSE, main=NA, xlim=c(lab[[1]][1], lab[[1]][len(lab[[1]])]))
# axis(1, at=lab[[1]], labels=NA)
# axis(1, at=lab[[2]], tick=F, labels= lab[[3]])

bscresults.getAxisMonthLabels <- function(doy.start, doy.end) {
   ticks1 <- c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 365)
   ticks2 <- c(16, 46, 75, 106, 136, 167, 197, 228, 259, 289, 320, 350)
   label2 <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

   return(list(
   ticks1 = ticks1[which(ticks1 >= doy.start & ticks1 <= doy.end)],
   ticks2 = ticks2[which(ticks2 >= doy.start & ticks2 <= doy.end)],
   label2 = label2[which(ticks2 >= doy.start & ticks2 <= doy.end)]
   ))
}
