# returns the standard significance levels symbols for a value or a vector

bscdata.signLevel <- function(x) {

   # if this is a vector, recurse for each value
   if (length(x) > 1) return(sapply(x, bscdata.signLevel))

   if (is.na(x) | is.nan(x)) return(NA)
   else if (x > 0.1) return("ns")
   else if (x > 0.05) return(".")
   else if (x > 0.01) return("*")
   else if (x > 0.001) return("**")
   else return("***")
}

