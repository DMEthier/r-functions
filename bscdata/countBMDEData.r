# this function creates a dataframe from the SQL table using the BMDE format

# Parameters:
# datasource: name of ODBC datasource used to create connection to database
# project: vector of id's used to filter the data based on project ID's. default is empty vector (not filter)
# collection: vector of codes used to filter the data based on collection codes. default is empty vector (not filter)
# sitecode: vector of codes used to filter the data based on site codes. default is empty vector (not filter)
# species: vector of species id's used to filter the data based on species ID. default is empty vector (not filter).
#          Species id's are numeric values. To filter based on species codes, you can use the function
#          bscdata.getSpeciesCodes()
# startyr: earliest year (use 0 if no filter needed)
# endyr: final year  (use 0 if no filter needed)
# shapes: vector of shape codes for counties, Important Bird Areas, Bird Conservation Regions, etc.
# fields: vector of field names used for aggregation. You can use the "bscdata.getFields()" function to list all fields available.

bscdata.countBMDEData <- function(datasource="bmdedata", fields=c(), ...) {

  #build SQL string
  sql <- "SELECT "

  #should validate list of fields first
  b <- " "
  if (length(fields) > 0) {
    for (i in 1:length(fields)) {
      sql <- paste(sql, b, "a.", fields[i], sep="")
      b <- ", "
    }
  }
  sql <- paste(sql, b, " count(*) as n_records, SUM(case when isnumeric(ObservationCount) <> 0 then ISNULL(CAST(ObservationCount AS int), 0) else 0 end) as total_count FROM bmde_data a left outer join projects b on a.project_id = b.project_id")

  sql <- paste(sql, bscdata.getBMDESQLFilter(...), sep="")

  if (length(fields) > 0) {
    sql <- paste(sql, " GROUP BY ")
    b <- " "
    for (i in 1:length(fields)) {
      sql <- paste(sql, b, "a.", fields[i], sep="")
      b <- ", "
    }
  }

  channel <- odbcConnect(datasource)
  bmde.df <- sqlQuery(channel, toString(sql))
  odbcClose(channel)
  rm(channel)

  print(paste("Number of records read:", length(bmde.df[,1])))
  return(bmde.df)
}

