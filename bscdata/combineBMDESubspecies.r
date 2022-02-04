# this function converts a BMDE dataframe to change subspecies ID's into species ID's (eg, Myrtle & Audubon
# Warblers are treated as Yellow-rumped Warblers), aggregate the resulting rows by calculating the total
# ObservationCount at a site in a single days (also deals with possible duplicates), and returns an aggregated
# dataframe

# Parameters:
# df: dataframe to aggregate
# authority: taxononic authority used to perform the aggregation
# datasource: name of ODBC datasource used to create connection to database

bscdata.combineBMDESubspecies <- function(df, authority, datasource="bmdedata") {

   print(paste("dataframe size before transformation:", length(df[,1]), "records"))

   # read species table as first step (subspecies are transformed to species)
   sc.df <- bscdata.getSpeciesCodes(authority="CMMN", species=unique(df$species_id),
      aggregate=TRUE, showall=FALSE)

   # merge with original data
   df <- merge(df, sc.df, by.x="species_id", by.y="species_id1", sort=FALSE)

   # remove the original species code and ID, and rename the species_id2 to species_id
   df <- subset(df, select = -c(species_id,SpeciesCode))
   names(df)[which(names(df) == "species_id2")] <- "species_id"

   # aggregate the data, in case more there are more than 1 record per species per day
   # requires the reshape package

   #get a list of all fields, except ObservationCount
   headerfields <- names(df[-which(names(df) %in% c("ObservationCount"))])

   #uses the reshape package functions melt and cast to aggregate the rows, creating unique records for each
   #site, date and species_id. See documentation at http://had.co.nz/reshape/

   df <- melt(df, id=headerfields)
   df <- cast(df,
      paste(bscdata.getDelimitedList(headerfields, delim=" + "), " ~ variable", sep=""),
      sum)

   print(paste("dataframe size after transformation:", length(df[,1]), "records"))

   return(df)
}
