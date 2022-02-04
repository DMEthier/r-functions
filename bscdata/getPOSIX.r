# this function returns a POSIXlt date object(s) from day, month, year and decimal time values.

bscdata.getPOSIXlt <- function(year, month, day, dec.time) {

   dec.time[is.na(dec.time)] <- 0
   hms <- bscdata.getHHMMSS(dec.time)

   return(as.POSIXlt(ISOdatetime(year, month, day, hms$hour, hms$minute, hms$second)))

}

# this function returns a POSIXct date object(s) from day, month, year and decimal time values.

bscdata.getPOSIXct <- function(year, month, day, dec.time) {

   dec.time[is.na(dec.time)] <- 0
   hms <- bscdata.getHHMMSS(dec.time)

   return(as.POSIXct(ISOdatetime(year, month, day, hms$hour, hms$minute, hms$second)))

}


