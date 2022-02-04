# Zero-fills a dataframe to create zero values for all species at each observation event (necessary for most population analyses). 
# The number of rows in the resulting dataframe should be the product of the number of unique monitoring events and the number of unique 
# species (eg, 10,000 surveys times 200 species = 2,000,000 records).

# df 			dataframe to zero-fill
# header.fields	a vector of field names that uniquely identify a monitoring event. This could be the SamplingEventIdentifier from BMDE for example, or a combination of fields such as YearCollected, MonthCollected, DayCollected, SiteCode. The list of fields that uniquely identify a monitoring event will depend of the dataset.
# sp.field		field name of the variable used to identify the species in the dataframe.
# sp.list		a vector of species used to zero-fill. If the sp.list argument is not provided, the function will generate a list of all species currently present in the dataframe.

#  Example
#
#  source("i:/r-functions/functions.r")
#
#  mmpfields <- c(c("SurveyAreaIdentifier","YearCollected","MonthCollected",
#    "DayCollected","SpeciesCode","ObservationCount"))
#
#  focalsp <- c("AMBI","KIRA","PBGR","VIRA","AMCO","COMO","LEBI","SORA","YERA")
#  focalid <- bscdata.getSpeciesID(datasource="bmdedata", "BSCDATA", focalsp, aggregate=TRUE)
#
#  df.mmp <- bscdata.readBMDEData(collection="MMPBIRDS", maxrec=0, startyr=2009, endyr=2009, fields=mmpfields)
#
#  df.zerofilled <- bscdata.zerofilling(df.mmp, c("SurveyAreaIdentifier","YearCollected","MonthCollected", "DayCollected"), sp.field="SpeciesCode")
#  df.zerofilled <- bscdata.zerofilling(df.mmp, c("SurveyAreaIdentifier","YearCollected","MonthCollected", "DayCollected"), sp.field="SpeciesCode", sp.list=c("AMGO", "AMRO"))

bscdata.zerofilling <- function(df, header.fields, sp.field, sp.list=c()) {

	require(sqldf)

	df.head <- sqldf(paste("select distinct ", paste(header.fields, collapse=", "), " from [df.mmp]", sep=""))
	if (length(sp.list) == 0) {
	  df.species <- sqldf(paste("select distinct ", sp.field, " from [df]", sep=""))
	} else {
	  df.species <- as.data.frame(sp.list)
      }
      names(df.species) <- sp.field
	df.zf <- sqldf("select * from [df.head], [df.species]")
	return (merge(df.zf, df, all.x=TRUE))

}

