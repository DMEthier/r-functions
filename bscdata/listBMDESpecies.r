bscdata.listBMDESpecies <- function(showall=FALSE, datasource="bmdedata", project=c(), collection=c(), authority="", species=c()) {

 if (length(project) == 1 && project == 0) project <- c()
 if (length(collection)  == 1 && collection == 0) collection <- c()

 sql <- "select lk_species.species_id, english_name as CommonName, scientific_name as ScientificName, french_name as FrenchName, d.sort_order"

 if (authority != "") {
   sql <- paste(sql, ", c.species_code, c.rank from lk_species LEFT JOIN (select * from lk_species_codes where authority = '", authority, "') c on lk_species.species_id = c.species_id ", sep="")
 } else {
   sql <- paste(sql, ", NULL as species_code, NULL as rank from lk_species", sep="")
 }

 sql <- paste(sql, " LEFT JOIN (select species_id, sort_order from lk_species_sort_order where sort_id = 1) d ON lk_species.species_id = d.species_id", sep="")

 a <- " WHERE "
 if (!showall) {
   sql <- paste(sql, a, " ( exists (select record_id from bmde_data where bmde_data.species_id = lk_species.species_id", sep="")
   a <- " AND"
   if (length(project) > 0) {
     sql <- paste(sql, a, " project_id in (", bscdata.getSQLINList(project), ")", sep="")
     a <- " AND"
   }
   if (length(collection) > 0) {
     sql <- paste(sql, a, " collection in (", bscdata.getSQLINList(collection), ")", sep="")
     a <- " AND"
   }
   sql <- paste(sql, "))", sep="")
 }

 if (length(species) > 0) {
   sql <- paste(sql, a, " lk_species.species_id in (", bscdata.getSQLINList(species, numeric=T), ")", sep="")
   a <- " AND"
 }
 #print(sql)

 channel <- odbcConnect(datasource)
 table1 <- sqlQuery(channel, sql)
 odbcClose(channel)
 rm(channel)
 return(table1)

}

