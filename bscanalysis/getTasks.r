# this function returns a list of analytical tasks to perform based on a schedule

# parameters:
# maxrec: maximum number of tasks to return at once
# type: type of tasks to return (the table can contain R-PROJECT, SQL-SPROC, DTS, SSIS, etc.). 
#     Normally, only R-PROJECT tasks need to be run from R
# alltasks: if TRUE, returns all tasks. Else, returns only tasks that are due to run based on a manual flag or a schedule
# datasource: name of ODBC datasource (normally dendroica, in this case)

# examples:
#   bscanalysis.getTasks()
#   bscanalysis.getTasks(alltasks=TRUE)


bscanalysis.getTasks <- function(maxrec=1, type="R-PROJECT", alltasks=FALSE, datasource="dendroica") {

  sql <- paste("SELECT top ", maxrec, " * FROM adm_tasks where enabled = 1 and task_type = ", bscdata.sqlString(type) , sep="")
  if (!alltasks) sql <- paste(sql, " and (manual_run = 1 OR (freq_days > 0 and dateadd(day, freq_days, last_run_dt) < getdate())) ", sep="")

  channel <- odbcConnect(datasource)
  df <- sqlQuery(channel, toString(sql))
  odbcClose(channel)
  rm(channel)

  return(df)

}

