bscdata.listBMDECollections <- function(showall=FALSE, datasource="bmdedata", project=0, collections=c()) {

 a <- " WHERE "

 sql <- "select collection_code, collection_name, project_id, akn_level from collections"
 if (showall) {
   sql <- paste(sql, a, " exists (select record_id from bmde_data where bmde_data.collection = collections.collection_code COLLATE Latin1_General_CI_AS)", sep="")
   a <- " AND "
 }
 if (project > 0) {
   sql <- paste(sql, a, " project_id in (", bscdata.getSQLINList(project, TRUE), ")", sep="")
   a <- " AND "
 }
 if (length(collections) > 0) {
   sql <- paste(sql, a, " collection_code in (", bscdata.getSQLINList(collections, FALSE), ")", sep="")
   a <- " AND "
 }

 channel <- odbcConnect(datasource)
 table1 <- sqlQuery(channel, sql)
 odbcClose(channel)
 rm(channel)
 return(table1)

}
