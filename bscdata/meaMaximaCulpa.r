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

# this function takes a BSC-specific BMDE dataframe and makes it compatible with BMDE fields.
# If keepnsf is TRUE, fields that are not part of the BMDE standard (non-standard fields) are removed

bscdata.prepareBMDEDataframe <- function(df, keepnsf=FALSE) {

  fields <- bscdata.getBMDEFieldConversions()

  # rename the BSC index fields to BMDE standard names
  for (i in 1:length(fields$bmdefield)) {
    if (fields$bscfield[i] %in% names(df)) {
      # removes the existing BMDE fields that will be replaced, then rename the BSC field
      df <- subset(df, select=!(names(df) == fields$bmdefield[i]))
      names(df)[which(names(df) == fields$bscfield[i])] <- as.character(fields$bmdefield[i])
    }
  }

  if (!keepnsf) {
    # removes the fields not conforming to BMDE standards
    df <- subset(df, select=!(names(df) %in% c("record_id", "protocol_id", "protocol_type", "species_id", "survey_week", "bcr")))
  }

  return(df)

}



# this function creates a dataframe from the SQL table using the BMDE format

# Parameters:
# datasource: name of ODBC datasource used to create connection to database
# project: vector of NUMERIC id's used to filter the data based on project ID's. default is empty vector (no filter)
# collection: vector of codes used to filter the data based on collection codes. default is empty vector (no filter)
# sitecode: vector of codes used to filter the data based on site codes. default is empty vector (no filter)
# species: vector of species id's used to filter the data based on species ID. default is empty vector (no filter).
#          Species id's are numeric values. To filter based on species codes, you can use the function
#          bscdata.getSpeciesCodes()
# startyr: earliest year (use 0 if no filter needed)
# endyr: final year  (use 0 if no filter needed)
# startday: earliest day of the year (note Jan 1st=1; use 0 if no filter needed)
# endday: final day of the year (note Jan 1st=1; use 0 if no filter needed)
# shapes: vector of shape codes for counties, Important Bird Areas, Bird Conservation Regions, etc.
# maxrec: max. number of records returned in the dataframe. This is mainly to prevent creating very large dataframes
#         by mistake. The default is 5000 records. Use 0 to return all records.
# fields: vector of field names required. You can use the "bscdata.getFields()" function to create a standard list
#         fields or list all fields available.
# zerofill: whether the full matrix with all zeros included should be created. Warning: this may create a very large dataframe.
#           It also hasn't been fully tested yet...
# distinct: whether the dataframe only includes records with distinct values. This is normally used when retrieving
#           only the headers records for instance)
# bmdeformat: if TRUE, non-BMDE index fields (specific to BSC database) are converted and used in place of standard BMDE fields. Values in the
#             BMDE standard fields are replaced by the values of the index fields and a warning is issues. See bscdata.

bscdata.meaMaximaCulpa <- function(datasource="dendroica", maxrec=5000, bmdeformat=TRUE) {

  headers <- c("SamplingEventIdentifier","RouteIdentifier","SiteCode","survey_year","survey_month","survey_day","TimeCollected","JulianDay","StateProvince")
  species <- c("ScientificName","CommonName","species_id","ObservationCount")
  fields <- c(headers, species)

  #build SQL string
  sql <- "SELECT "
  if (maxrec > 0) sql <- paste(sql, "top", maxrec)

  #should validate list of fields first
  if (length(fields) > 0) {
    b <- " "
    for (i in 1:length(fields)) {
      sql <- paste(sql, b, "a.", fields[i], sep="")
      b <- ", "
    }
  }
  sql <- paste(sql, "FROM vwBMDE_cmmn_det_tcbo a left outer join projects b on a.project_id = b.project_id")

  #sql <- paste(sql, bscdata.getBMDESQLFilter(...), sep="")

  channel <- odbcConnect(datasource)
  bmde.df <- sqlQuery(channel, toString(sql))
  odbcClose(channel)
  rm(channel)

  if (bmdeformat) {
     bmde.df <- bscdata.prepareBMDEDataframe(bmde.df, TRUE)
     print("Warning: some interpreted values may have been used in place of values originally stored in BMDE format.")
     print("To prevent this ption, you must use the bmdeformat=FALSE parameter. The see a list of fields that have been modified,")
     print("you should refer to the documentation for the bscdata.prepareBMDEDataframe() function")
  }


  print(paste("Number of records read:", length(bmde.df[,1])))
  return(bmde.df)

}


require(RODBC)
tcbo.df <- bscdata.meaMaximaCulpa()
