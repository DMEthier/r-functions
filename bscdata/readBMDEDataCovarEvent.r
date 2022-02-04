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
# maxrec: max. number of records returned in the dataframe. This is mainly to prevent creating very large dataframes
#         by mistake. The default is 5000 records. Use 0 to return all records.
# fields: vector of field names required. You can use the bscdata.getFields("covar_event") function to create a standard list
#         fields or list all fields available. Use "*" if all fields are required.
# merge: whether the covariate table is merged with the BMDE data. If FALSE, the dataframe created should contain only
#        one record per SamplingEvent. If TRUE, the dataframe should contain as many record as the BMDE data, with a field
#        called GlobalUniqueIdentifier that can be used to merge the dataframes

bscdata.readBMDEDataCovarEvent <- function(datasource="bmdedata", project=c(), collection=c(), sitecode=c(), species=c(),
   prov=c(), startyr=0, endyr=0, startday=0, endday=0, maxrec=0, fields="*", merge=FALSE) {

  if (length(project) == 1 && project == 0) project <- c()
  if (length(collection)  == 1 && collection == 0) collection <- c()
  if (length(sitecode) == 1 && sitecode == "") sitecode <- c()
  if (length(species) == 1 && species == 0) species <- c()
  if (length(prov) == 1 && prov == "") prov <- c()

  #build SQL string
  sql <- "SELECT "
  if (maxrec > 0) sql <- paste(sql, "top", maxrec)

  #should validate list of fields first
  if (length(fields) > 0) {
    b <- " "
    for (i in 1:length(fields)) {
      sql <- paste(sql, b, "b.", fields[i], sep="")
      b <- ", "
    }
  }
  if (merge) sql <- paste (sql, ", GlobalUniqueIdentifier ", sep="")

  sql <- paste(sql, " FROM bmde_data_covar_event b ", sep="")
  a <- " WHERE "
  if (merge) {
    sql <- paste(sql, " INNER JOIN bmde_data a ON a.collection = b.collection and a.SamplingEventIdentifier = b.SamplingEventIdentifier", sep="")
  }
  else {
    sql <- paste(sql, " WHERE EXISTS (select GlobalUniqueIdentifier from bmde_data a where a.collection = b.collection and a.SamplingEventIdentifier = b.SamplingEventIdentifier ", sep="")
    a <- " AND"
  }
  if (length(project) > 0) {
    sql <- paste(sql, a, " a.project_id in (", bscdata.getSQLINList(project), ")", sep="")
    a = " AND"
  }
  if (length(sitecode) > 0) {
    sql <- paste(sql, a, " a.SiteCode in (", bscdata.getSQLINList(sitecode), ")", sep="")
    a = " AND"
  }
  if (length(collection) > 0) {
    sql <- paste(sql, a, " a.collection in (", bscdata.getSQLINList(collection), ")", sep="")
    a = " AND"
  }
  if (startyr > 0) {
    sql <- paste(sql, a, " a.survey_year >= '", startyr, "'", sep="")
    a = " AND"
  }
  if (endyr > 0) {
    sql <- paste(sql, a, " survey_year <= '", endyr, "'", sep="")
    a = " AND"
  }
  if (startday > 0) {
    sql <- paste(sql, a, " DATEPART(dayofyear, CAST(a.[survey_year] AS varchar(4)) + '/' + CAST(a.[survey_month] AS varchar(2)) + '/' + CAST(a.[survey_day] AS varchar(2))) >= ", startday, sep="")
    a = " AND"
  }
  if (endday > 0) {
    sql <- paste(sql, a, " DATEPART(dayofyear, CAST(a.[survey_year] AS varchar(4)) + '/' + CAST(a.[survey_month] AS varchar(2)) + '/' + CAST(a.[survey_day] AS varchar(2))) <= ", endday, sep="")
    a = " AND"
  }
  if (length(prov) > 0) {
    sql <- paste(sql, a, " a.statprov_code in (", bscdata.getSQLINList(prov), ")", sep="")
    a = " AND"
  }
  if (length(species) > 0) {
    sql <- paste(sql, a, " a.species_id in (", bscdata.getSQLINList(species, numeric=T), ")", sep="")
    a = " AND"
  }
  if (!merge) {
    sql <- paste(sql, ")", sep="")
  }

  channel <- odbcConnect(datasource)
  bmde.df <- sqlQuery(channel, toString(sql))
  odbcClose(channel)
  rm(channel)

  print(paste("Number of records read:", length(bmde.df[,1])))
  return(bmde.df)

}

