# this function converts a vector as a SQL "in" string
# for example bscdata.getSQLlist(c("HOWA","ACFL","PIPL")) will return
# 'HOWA','ACFL','PIPL'
# this function will normally only be called by other functions that build SQL strings
# Note: NA's are ignored and do not generate any errors.
# Duplicates are also ignored.

# parameters:
# v: vector of values to enumerate
# numeric: whether the list contains numeric values only or not. If not, values are enclosed by quotes.
#          Should only be FALSE for lists that always only contain numbers.

bscdata.getSQLINList <- function(v, numeric=FALSE) {

  return(bscdata.getDelimitedList(v, quotechar=ifelse(numeric, "", "'"), escapechar="'", delim=", ", keepdupl=FALSE))

}

