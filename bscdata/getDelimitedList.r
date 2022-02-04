# this function converts a vector to a delimited list. This is generic function that can be used to create
# SQL "in" strings for example, or formulas, or any concatenated string based on a vector
# this function will normally only be called by other functions, such as bscdata.getSQLINList()
# Note: NA's are ignored and do not generate any errors.
# Duplicates are also ignored.

# parameters:
# v: vector of values to enumerate
# quotechar: character to use as quotes (ie, before and after the element)
# quotechar2: character to use as ending quotes (if different from starting quotechar)
# escapechar: character to use to escape the inner quotechar itself when present in the element (eg, backslash "\")
# delim: delimiter between elements (such as a comma, a semi-colon, an empty string "", etc.)
# keepdupl: whether duplicates should be preserved

# examples:
#   bscdata.getDelimitedList(c("paruline","d'alberta","à gorge rousse"), quotechar="'", escapechar="'", delim=",")
#   bscdata.getDelimitedList(c("age","year","habitat"), quotechar="", escapechar="", delim=" + ")

bscdata.getDelimitedList <- function(v, quotechar="", quotechar2=quotechar, escapechar="", delim=", ", keepdupl=TRUE) {

  v <- v[!is.na(v)]
  if (!keepdupl) v <- unique(v)
  str <- ""
  if (length(v) > 0) {
    b <- ""
    if (quotechar != "" & escapechar != "") v <- gsub(paste("(", quotechar, ")", sep=""), paste(escapechar, "\\1", sep=""), v)
    for (i in 1:length(v)) {
      str <- paste(str, b, quotechar, v[i], quotechar2, sep="")
      b <- delim
    }
  }

  return(str)

}

