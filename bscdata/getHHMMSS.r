# this function returns the hour, minute, seconds from a vector of values in decimal time format.

# Parameters:
# dec.time: time in 24-h decimal format (eg, 15h30 = 15.5)

bscdata.getHHMMSS <- function(dec.time) {

    hour <- as.integer(dec.time)
    minute <- as.integer((dec.time - hour) * 60)
    second <- as.integer((dec.time - hour - minute/60) * 3600)

    return(as.data.frame(cbind(hour,minute,second)))

}

