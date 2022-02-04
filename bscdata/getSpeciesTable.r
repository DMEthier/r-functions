# Creates a dataframe that contains the list of species from a specific authority with name and sorting order (eg AOU)
# Each authority has a distinct list of species alphanumeric codes that can be converted to and from species numeric ID's.
# The function listSpeciesCodeAuthorities can be used to list the authority used for each dataset.

# Returns: a dataframe with a list of species, species alphanumeric code, species ID and sort order (default = AOU).

# Parameters:
# datasource: ODBC datasource
# authority: authority used to describe species codes (eg, CMMN, BBS, ONATLAS, etc.).
#            Use the listSpeciesCodeAuthorities.r function to list all authorities available.
# showall:  boolean that determines whether all possible alphanumeric codes for a given species are returned,
#           or only the prefered ones (eg, LTDU and OLDS for Long-tailed Duck vs. Oldsquaw).
# sortorder:   which sorting order should be used (only option for now is 1 = AOU)

# example: bscdata.getSpeciesTable(authority="BSCDATA", showall=T)

bscdata.getSpeciesTable <- function(datasource="bmdedata", authority, showall=FALSE, sortorder=1) {

  sql <- paste("SELECT * from lk_species_codes where authority = '", authority, "'", sep="")

  sql <- paste("SELECT c.authority, c.species_code, c.rank, c.species_id, c.species_id2, a.sort_order, ",
            "b.english_name, b.scientific_name, b.french_name, d.scientific_name AS group_name ",
            "FROM lk_species_sort_order a INNER JOIN lk_species b ON a.species_id = b.species_id INNER JOIN ",
            "lk_species_codes c ON b.species_id = c.species_id LEFT OUTER JOIN lk_species_groups d ON ",
            "a.sort_id = d.sort_id AND a.group_id = d.group_id WHERE (c.authority = '", authority , "') AND ",
            "(a.sort_id = ", sortorder, ") ", sep="")

  if (!showall) sql <- paste(sql, "AND c.rank = 1 ", sep="")
  sql <- paste(sql, " ORDER BY a.sort_order", sep="")

  channel <- odbcConnect(datasource)
  table1 <- sqlQuery(channel, toString(sql))
  odbcClose(channel)
  rm(channel)

  return(table1)
}


