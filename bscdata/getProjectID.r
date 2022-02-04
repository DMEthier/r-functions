# Returns a numeric project ID matching a alphanumeric code (eg, CMMN=1005).

# Parameters:
# datasource: ODBC datasource
# code: alphanumeric code for a project

bscdata.getProjectID <- function(datasource="bmdedata", code) {

  sql <- paste("SELECT project_code, project_id from projects where project_code in (", bscdata.getSQLINList(code), ")", sep="")

  channel <- odbcConnect(datasource)
  df <- sqlQuery(channel, toString(sql))
  odbcClose(channel)
  rm(channel)

  if (length(df$project_id) > 0) {
    return(df$project_id)
  } else {
    return(NA)
  }

}

