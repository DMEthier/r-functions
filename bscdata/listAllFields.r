#returns a list of all fields available in the bmde_data table

bscdata.listAllFields <- function(datasource="bmdedata") {
 sql <- "select COLUMN_NAME from INFORMATION_SCHEMA.COLUMNS where table_name = 'bmde_data' order by ordinal_position"
 channel <- odbcConnect(datasource)
 fields.df <- sqlQuery(channel, sql)
 odbcClose(channel)
 rm(channel)
 return(fields.df$COLUMN_NAME)
}
