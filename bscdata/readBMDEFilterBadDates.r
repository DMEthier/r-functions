# this function creates a dataframe from the SQL table that contains dates to be excluded
# each record in the filter table corresponds to a series of conditions used to exclude data from a dataframe

# Parameters:
# datasource: name of ODBC datasource used to create connection to database
# project: vector of id's used to filter the data based on Project Code (e.g., CMMN). default is empty vector (not filter)
# sitecode: vector of codes used to filter the data based on Area Code (e.g. LPBO). default is empty vector (not filter)
# species: vector of species id's used to filter the data based on species ID. default is empty vector (not filter).
#          Species id's are numeric values. To filter based on species codes, you can use the function
#          bscdata.getSpeciesCodes()
# maxrec: max. number of records returned in the dataframe. This is mainly to prevent creating very large dataframes
#         by mistake. The default is to return all records (maxrec=0).
# tablename: name of the database table where the filters are stored

bscdata.readBMDEFilterBadDates <- function(datasource="dendroica", project=c(), sitecode=c(), species=c(), maxrec=0, tablename="bmde_filter_bad_dates", verbose=FALSE) {

  #build SQL string
  sql <- paste("SELECT * FROM", tablename)
  a <- " WHERE "

  if (!is.null(project)) {
    sql <- paste(sql, a, "project_id in (", bscdata.getSQLINList(project), ")", sep="")
    a = "AND"
  }
  if (!is.null(sitecode)) {
    sql <- paste(sql, a, " sitecode in (", bscdata.getSQLINList(sitecode), ")", sep="")
    a = "AND"
  }
  if (!is.null(species)) {
    sql <- paste(sql, a, " species_id in (", bscdata.getSQLINList(species), ")", sep="")
    a = "AND"
  }

  channel <- odbcConnect(datasource)
  df <- sqlQuery(channel, toString(sql))
  odbcClose(channel)
  rm(channel)

  if (verbose) print(paste("Number of filters read:", ifelse(is.na(df),0,length(df[,1]))))
  return(df)

  }

