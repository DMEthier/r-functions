# Converts a vector of species alphanumeric codes (eg, BCCH = Black-capped Chickadee) into species numeric ID's.

# Returns: a dataframe that contains the original code vector, and a new vector with the species numeric ID's. Codes that cannot be resolved are returned as NA.

# Parameters:
# datasource: ODBC datasource (default="bmdedata")
# authority: authority used to describe species codes (eg, CMMN, BBS, ONATLAS, etc.).
#            Use the bscdata.listSpeciesCodeAuthorities() function to list all authorities available.
# spcodes: vector of species codes to translate. These can include NA's and duplicate values.
# aggregate: whether subspecies codes should be aggregated as fully recognized species (eg, if false, YRWA, MYWA and AUWA
#            would translate to different id's; if true, they would all translate to the same id).

bscdata.getSpeciesID <- function(datasource="bmdedata", authority, spcodes, aggregate=TRUE) {

  sql <- paste("SELECT * from lk_species_codes where authority = '", authority, "'", sep="")

  channel <- odbcConnect(datasource)
  species.df <- sqlQuery(channel, toString(sql))
  odbcClose(channel)
  rm(channel)

  spcodes <- toupper(spcodes)

  # if aggregate is false, replace the species_id by species_id2 where they exist
  if (!aggregate) species.df$species_id[!is.na(species.df$species_id2)] <- species.df$species_id2[!is.na(species.df$species_id2)]

  spcodes.df <- data.frame(species_code=spcodes)
  species.df <- merge(spcodes.df, species.df, sort=F, all.x=T)

  return(data.frame(species_code=spcodes, species_id=species.df$species_id))

}

