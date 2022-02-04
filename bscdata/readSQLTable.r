# this function creates a dataframe from any odbc connection (eg, a SQL table)

# Parameters:
# datasource: name of ODBC datasource used to create connection to database
# table: table name
# fields: vector of field names required. Default is all fields (*).
# maxrec: max. number of records returned in the dataframe. Use 0 to return all records.
# distinct: whether the dataframe only includes records with distinct values.
# where: where portion of the SQL command (eg, where = " year >= 2001")

bscdata.readSQLTable <- function(datasource, table, fields=c("*"), maxrec, distinct=FALSE, where="") {

  #build SQL string
  sql <- "SELECT "
  if (maxrec > 0) sql <- paste(sql, "top", maxrec)
  if (distinct) sql <- paste(sql, "distinct")

  if (length(fields) > 0) {
    b <- " "
    for (i in 1:length(fields)) {
      sql <- paste(sql, b, fields[i], sep="")
      b <- ", "
    }
  }
  sql <- paste(sql, "FROM ", table)
  if (where != "") sql <- paste(sql, " WHERE ", where, sep="")

  channel <- odbcConnect(datasource)
  df <- sqlQuery(channel, toString(sql))
  odbcClose(channel)
  rm(channel)

  return(df)
}
