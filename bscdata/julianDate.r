# Script to transform dates (year, month, day) to Julian dates,
# with January 1 as yday = 0

# to take care of leap years, such that May 15 has the same Julian date
# in all years, could use a base.year (e.g., 2001) when calling in this code,
# instead of the year data was collected.
# In such case, Year would need to be kept as an independent variable.

bscdata.julianDate <- function(year, month, day) {
  if (min(year) < 1000) {
    warning("Year should have 4 digits")
    return(NA)
  } else {
   return(as.POSIXlt(ISOdate(year, month, day))$yday)
  }
}

