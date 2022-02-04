# loops to get all CSV files matching certains ranges of years for a set of stations, and returns a dataframe
# that contains all the data together. This can get big, so it is reccommended to only run a few stations per instance

# seasonal: boolean. If true, the month filters are applied for all years (eg, May-Sep). Otherwise, the month filters are
#           used as a continuous filter (May 2005 to Apr 2007 would include all months from 2006, for instance). Year range MUST
#           also be provided for non-seasonal filters.

bscdata.readECWeather <- function(stations, startyr, endyr=startyr, startmonth=1, endmonth=12, seasonal=F, save=F, dir="T:/cmmn/climatedata/", type="dly") {
   i = 0
   for (s in list(stations)) {
      if (type == "mly") {
        # monthly data comes all together in one file, so the filters are applied after reading.
        try(df2 <- readECWeatherSingle(s, startyr, 1, 0, save, dir, type))
        if (!is.null(df2)) {
           df2$stationID <- s

           if (startyr > 0) df2 <- subset(df2, Year >= startyr)
           if (endyr > 0) df2 <- subset(df2, Year <= endyr)
           if (seasonal) {
             if (startmonth > 0) df2 <- subset(df2, Month >= startmonth)
             if (endmonth > 0) df2 <- subset(df2, Month <= endmonth)
           } else {
             if (startmonth > 0 & startyr > 0) df2 <- subset(df2, Year > startyr | Month >= startmonth)
             if (endmonth > 0 & endyr > 0) df2 <- subset(df2, Year < endyr | Month <= endmonth)
           }

           if (i == 0) df1 <- df2
           else try(df1 <- rbind(df1, df2))
           i <- i+1

        }
      } else if (type == "dly") {
        # daily data comes all together in one file per year.
        for (y in startyr:endyr) {
           try(df2 <- readECWeatherSingle(s, y, 0, 0, save, dir, type))
           if (!is.null(df2)) {

              df2$stationID <- s
              if (seasonal) {
                if (startmonth > 0) df2 <- subset(df2, Month >= startmonth)
                if (endmonth > 0) df2 <- subset(df2, Month <= endmonth)
              } else {
                if (startmonth > 0 & startyr > 0) df2 <- subset(df2, Year > startyr | Month >= startmonth)
                if (endmonth > 0 & endyr > 0) df2 <- subset(df2, Year < endyr | Month <= endmonth)
              }

              if (i == 0) df1 <- df2
              else try(df1 <- rbind(df1, df2))
              i <- i+1
           }
         }
      } else if (type == "hly") {
        # hourly data comes all together in one file per month.
        for (y in startyr:endyr) {
          if (seasonal | y == startyr) m1 <- startmonth
          else m1 <- 1
          if (seasonal | y == endyr) m2 <- endmonth
          else m2 <- 12
          for (m in m1:m2) {
             try(df2 <- readECWeatherSingle(s, y, m, 0, save, dir, type))
             if (!is.null(df2)) {
                df2$stationID <- s
                if (i == 0) df1 <- df2
                else try(df1 <- rbind(df1, df2))
                i <- i+1
             }
          }
         }
      } #end of else block
   }
   return(df1)
}

#df <- readECWeather(stations=c(3783), startyr=2007, endyr=2007, startmonth=1, endmonth=2)

