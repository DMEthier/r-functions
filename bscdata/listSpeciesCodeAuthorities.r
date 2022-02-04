# returns a list of unique species code authorities (eg, "BBS" or "CMMN")

bscdata.listSpeciesCodeAuthorities <- function(datasource="bmdedata", bycollection=FALSE) {

 if (bycollection) {
   sql <- "select collection_code, species_authority from collections"
 } else {
   sql <- "select distinct authority from lk_species_codes"
 }

 channel <- odbcConnect(datasource)
 table1 <- sqlQuery(channel, sql)
 odbcClose(channel)
 rm(channel)
 return(table1)

}

