# this function converts a vector as a list of SQL "like" strings. This is useful for query variables based on partial matches.
# for example bscdata.getSQLlist("sitecode", c("VIMA%","VIVV-001","VIPC%")) will return
# sitecode like 'VIMA%' OR sitecode like 'VIVV-001' OR sitecode like 'VIPC%'
# this function will normally only be called by other functions that build SQL strings
# Note: NA's are ignored and do not generate any errors.

# parameters:
# varname: name of the SQL variable(s). You can either provide a single name, or a vector of the same length as v
# v: vector of values to enumerate

bscdata.getSQLLike <- function(varname, v) {

   sql <- ""
   a <- ""
   for (i in 1:length(v)) {
     sql <- paste(sql, a, " ", ifelse(length(varname) > 1, varname[i], varname), " LIKE '", v[i], "\' ", sep="")
    a = " OR"
   }

   return (sql)
}

