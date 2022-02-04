# Converts a vector of species ID's (eg, 16610 = Yellow-rumped Warbler) into species alphanumeric codes (eg. YRWA).

# Returns: a dataframe with 3 vectors, at least as long as the original species id vector (or longer if multiple
# codes are used for a given species and showall=TRUE). species_id1 is the original vector provided. species_id2 is
# the vector of id's used to match to species codes. species_code is the vector of codes used for the given authority.

# Parameters:
# datasource: ODBC datasource
# authority: authority used to describe species codes (eg, CMMN, BBS, ONATLAS, etc.).
#            Use the listSpeciesCodeAuthorities.r function to list all authorities available.
# species: vector of species id's to translate. These can include NA's and duplicate values.
# aggregate: whether subspecies codes should be aggregated as fully recognized species (eg, if false, 16610, 16620 and 16630
#            would translate to different codes (YRWA, MYWA and AUWA); if true, they would all translate to the same code: YRWA).
# showall: whether the results should return only the main species codes for a given authority, or all possible
#          variants for the same species (eg, YRWA, MYWA, AUWA, UYRW).
#          WARNING: If you choose showall=TRUE, your resulting vector may have more rows than the original one provided.
#          When showall=FALSE, the vector size returned should be the same as the original vector.

# example1 (returns all possible codes used for Yellow-rumped Warbler)

# bscdata.getSpeciesCodes(species=c(16610), authority="CMMN", aggregate=T, showall=T)

# example2 (returns all possible unique ID's used for Yellow-rumped Warbler, Dark-eyed Junco and Northern Flicker)
# this example could be used to generate a list of all ID's that should be queried against the database to make
# sure that all forms are returned.

# unique(bscdata.getSpeciesCodes(authority="CMMN",
#   species=bscdata.getSpeciesID(authority="CMMN", spcodes=c("YRWA","NOFL","DEJU"), aggregate=F)$species_id,
#   aggregate=F, showall=T)$species_id2)

bscdata.getSpeciesCodes <- function(datasource="bmdedata", authority, species, aggregate=TRUE, showall=FALSE) {

  sql <- paste("SELECT * from lk_species_codes where authority = '", authority, "'", sep="")

  channel <- odbcConnect(datasource)
  spcodes.df <- sqlQuery(channel, toString(sql))
  odbcClose(channel)
  rm(channel)

  if (length(spcodes.df$species_id) > 0) {

  spcodes.df$species_id2[is.na(spcodes.df$species_id2)] <- spcodes.df$species_id[is.na(spcodes.df$species_id2)]

    # if the aggregate option is TRUE, convert the species ID's into their equivalent species
    if (aggregate) {
      spcodes.df2 <- subset(spcodes.df, spcodes.df$species_id != spcodes.df$species_id2 | spcodes.df$rank == 1)
      species.df2 <- data.frame(species_id2=species)
      species.df2 <- merge(species.df2, spcodes.df2, sort=F, all.x=T)
      species.df1 <- data.frame(species_id1=species.df2$species_id2, species_id2=species.df2$species_id)
    } else {
      if (showall) species.df1 <- data.frame(species_id1=species, species_id=species)
      else species.df1 <- data.frame(species_id1=species, species_id2=species)
    }

    if (!showall) {
      spcodes.df <- subset(spcodes.df, spcodes.df$species_id != spcodes.df$species_id2 | spcodes.df$rank == 1)
    }

    species.df1 <- merge(species.df1, spcodes.df, sort=F, all.x=T)
    return(data.frame(species_id1=species.df1$species_id1, species_id2=species.df1$species_id2, species_code=species.df1$species_code))

  } else {
    return(NA)
  }
}

