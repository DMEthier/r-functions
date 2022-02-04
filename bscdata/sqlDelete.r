# delete records from an existing ODBC table.

# tablename:  name of the ODBC table containing records to delete
# textfile:   optional. full path of a text file where the SQL commands for batch insert are saved. this option allows
#             to generate a batch upload file (as an alternative or in option to saving the records in the ODBC table)
# append:     boolean. whether batch commands should be appended to an existing textfile (textfile must be provided)
# deleterec:  boolean. whether the SQL INSERT commands are executed immediately. Insert permissions are required for
#             your personal database login associated with your ODBC connection.
# datasource: name of ODBC datasource used to create connection to database (required when deleterec is TRUE)
# ... :       criteria used to delete records. See bscdata.getBMDESQLFilter().

# example: bscdata.sqlDelete("results_annual_indices", project=c(1005), textfile="c:/temp1.sql")
# example: bscdata.sqlDelete("results_trends", collection=c("CMMN-DET-LPBO"), textfile="c:/temp1.sql", deleterec=T, datasource="dendroica")

bscdata.sqlDelete <- function(tablename, textfile=NA, append=T, deleterec=F, datasource=NA, ...) {

  if (deleterec) {
     if (is.na(datasource)) {
       warning("the ODBC data source was not specified. the records will not be deleted.")
       deleterec = F
     } else {
       channel <- odbcConnect(datasource)
     }
  }

  sql1 <- paste("DELETE FROM ", tablename, bscdata.getBMDESQLFilter(..., prefix=NA), sep="")

  if (deleterec) {
     i <- sqlQuery(channel, sql1)
     if (length(i) > 0) warning(i)
     odbcClose(channel)
     rm(channel)
  }

  if (!is.na(textfile)) write(sql1, textfile, append=append)

}

