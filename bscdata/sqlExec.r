# execute a SQL command

bscdata.sqlExec <- function(sql, datasource) {

       channel <- odbcConnect(datasource)
       for (i in 1:length(sql)) {
         df <- sqlQuery(channel, sql[i])
       }
       odbcClose(channel)
       rm(channel)

       return(df)

}
