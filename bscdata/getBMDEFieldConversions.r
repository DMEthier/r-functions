# this function returns at dataframe that contains the list of non-standard field used by BSC and their matching equivalent
# in BMDE format. This function is mainly used by the bscdata.prepareBMDEDataframe() function

bscdata.getBMDEFieldConversions <- function() {

   bscfield <- c("collection","project_id","statprov_code","country_code","SiteCode","latitude",
        "longitude","survey_year","survey_month","survey_day","species_code","doy")

   bmdefield <- c("CollectionCode", "ProjectCode", "StateProvince", "Country", "SurveyAreaIdentifier", "DecimalLatitude",
        "DecimalLongitude", "YearCollected", "MonthCollected", "DayCollected", "SpeciesCode", "JulianDay")

   if (length(bscfield) != length(bmdefield)) stop("Vector length do not match")
   else return(data.frame(bscfield, bmdefield))

}
