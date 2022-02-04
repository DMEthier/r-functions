#builds a list of fields to return from the database.
#possible values of v:
#"standard": fields most commonly used
#"short": scientific name and number of individuals only
#"all": all fields (use with caution, not recommended for large datasets)
#"header": only the header fields, such as date, location, but not the species data.

bscdata.getFields <- function(v="standard") {

  headers <- c("project_id","SamplingEventIdentifier","RouteIdentifier","SiteCode","survey_year","survey_month","survey_day","TimeCollected","JulianDay","StateProvince")
  species <- c("ScientificName","CommonName","species_id","ObservationCount")

  if (v == "all") fields <- bscdata.listAllFields()
  else if (substring(v,1,5) == "short") fields <- c("ScientificName","ObservationCount")
  else if (substring(v,1,7) == "headers") fields <- headers
  else if (substring(v,1,8) == "standard") fields <- c(headers, species)
  else if (substring(v,1,3) == "rpi") fields <- c(headers, species,"TimeObservationsStarted","TimeObservationsEnded","DurationInHours","SpeciesCode","ObservationCount")
  else if (substring(v,1,4) == "cmmn") fields <- c("project_id","SamplingEventIdentifier","SiteCode","survey_year","survey_month","survey_day","species_id","SpeciesCode","ObservationCount")
  else if (substring(v,1,5) == "bccws") fields <- c(headers, species, c("SpeciesCode","ObservationCount2","ObservationCount3","ObservationCount4","ObservationCount5"))
  else warning("The pre-defined list of field requested does not exist")

  if (length(fields) > 0 & substring(v,nchar(v)-2) == "ext") fields <- c(fields, c("ObservationCount2","ObservationCount3","ObservationCount4","ObservationCount5"))

  return(fields)
}
