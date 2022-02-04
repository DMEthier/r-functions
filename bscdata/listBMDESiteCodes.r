# returns a unique list of SiteCode values from the bmde_data table

bscdata.listBMDESiteCodes <- function(datasource="bmdedata", project=c(), collection=c()) {

 if (length(project) == 1 && project == 0) project <- c()
 if (length(collection)  == 1 && collection == 0) collection <- c()

 sql <- "select distinct project_id, collection, SiteCode from bmde_data"

 a <- " WHERE"
 if (length(project) > 0) {
   sql <- paste(sql, a, " project_id in (", bscdata.getSQLINList(project), ")", sep="")
   a = "AND"
 }
 if (length(collection) > 0) {
   sql <- paste(sql, a, " collection in (", bscdata.getSQLINList(collection), ")", sep="")
   a = "AND"
 }

 channel <- odbcConnect(datasource)
 table1 <- sqlQuery(channel, sql)
 odbcClose(channel)
 rm(channel)
 return(table1)

}
