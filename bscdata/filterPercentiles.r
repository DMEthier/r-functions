# this function filters exclude data at the tail of the seasonal or hourly distribution (days with count > 0).

# Parameters:
# df: dataframe to filter. Must contain at least the following fields using BMDE names:
# count.var: vector of variables used as count (default: ObservationCount = number of individuals detected at each instance)
# use.date: whether or not data is filtered based on date.
#           required BMDE variables: YearCollected, MonthCollected and DayCollected.
#           If YearCollected is missing, uses 2001 as the base year for all data.
# use.time: whether or not data is filtered based on time.
#           required BMDE variable: TimeObservationsStarted.
# count.var: name of the count variable(s). If more than one is provided, the percentiles are based on the first one only.
# by.var: list of variable names used to subset the data (eg, if SpeciesCode is used, the percentiles are
#         calculated individually for each SpeciesCode value). If not value is provided, the percentile is
#         calculated on the entire data frame.
# pct: percentile used (eg, 0.95 means that only the 95% middle values are preserved)
# printinfo: boolean. whether or not information stats are printed

bscdata.filterPercentiles <- function(df, use.date=T, use.time=F,
    count.var=c("ObservationCount"), by.var=c(), pct=0.95, print.info=T) {

  if (print.info) print(paste("dataframe size before filter(s) applied:", length(df[,1]), "records"))

  dt <- bscdata.getPOSIXlt(ifelse(is.na(df$YearCollected),2001,df$YearCollected), df$MonthCollected, df$DayCollected, df$TimeObservationsStarted)
  df$doy <- dt$yday
  df$d.t <- as.numeric(df$TimeObservationsStarted)

  q.lo <- (100 - pct*100) / 2
  q.hi <- 100 - q.lo

  # generate a single unique group variable, based on the combinations of values
  # from the group.var variables (eg, "LPBO_SSHA_"). Percentiles will be calculated
  # independently for each unique values of group.var

  df$group.var <- ""
  if (length(by.var) > 0) {
    for (i in 1:length(by.var)) {
      df$group.var <- paste(df$group.var, by.var[i], ": ", df[[by.var[i]]], "; ", sep="")
    }
  }

  # adds a column called gr.index that is the index values that corresponds
  # to the windows tables (for subsetting the data later)

  gr <- unique(df$group.var)
  gr.names <- as.data.frame(cbind(gr, 1:length(gr)))
  names(gr.names) <- c("group.var", "gr.index")
  df <- merge(df, gr.names)

  if (use.time) {

    # calculates, for each level of group.var, percentiles of hours with data

    windows.hrly <- sapply(as.factor(gr), USE.NAMES = TRUE,
      function(x){
      q.r <- with(subset(df, df[[count.var[1]]] > 0 & df$group.var == x),
      round(quantile(d.t, probs = c(q.lo, q.hi)/100, na.rm = T), digits = 0))
      return(q.r)
    })

    df <- subset(df, d.t >= windows.hrly[,gr.index][1] &
    d.t <= windows.hrly[,gr.index][2])

  }
  if (use.date) {

    # use recast to calculate the sum of counts within each day (ie, for hourly data)
    by.var.dt <- c("YearCollected", "doy", "group.var")
    if (length(by.var) > 0) by.var.dt <- c(by.var.dt, by.var)
    fmla <- as.formula(paste(paste(by.var.dt, collapse="+"), " ~ variable"))
    date.tot <- recast(df, fmla, fun.aggregate = sum, na.rm = TRUE,
      id.var = by.var.dt, measure.var = count.var)

    # calculates, for each level of group.var, percentiles of days with data

    windows.date <- sapply(as.factor(gr), USE.NAMES = TRUE,
      function(x){
      q.r <- with(subset(date.tot, date.tot[[count.var[1]]] > 0 & date.tot$group.var == x),
      round(quantile(doy, probs = c(q.lo, q.hi)/100, na.rm = T), digits = 0))
      return(q.r)
    })

    df <- subset(df, doy >= windows.date[,gr.index][1] &
     doy <= windows.date[,gr.index][2])

  }

  if (print.info) print(paste("dataframe size after filter(s) applied:", length(df[,1]), "records"))

  return(subset(df, select = -c(doy, d.t, group.var, gr.index)))

}

