# this function creates a dataframe from the SQL table using the BMDE format
# it extracts the date range filters of specified species from specified sites, period (eg, season) and project (project_code)

# Parameters:
# datasource: name of ODBC datasource used to create connection to database
# project_code: vector of id's used to filter the data based on Project Code (e.g., CMMN). default is empty vector (not filter)
# sitecode: vector of codes used to filter the data based on Area Code (e.g. LPBO). default is empty vector (not filter)
# species_id: vector of species id's used to filter the data based on species ID. default is empty vector (not filter).
#          Species id's are numeric values. To filter based on species codes, you can use the function
#          bscdata.getSpeciesCodes()
# maxrec: max. number of records returned in the dataframe. This is mainly to prevent creating very large dataframes
#         by mistake. The default is 5000 records. Use 0 to return all records.
# tablename: name of the database table where the filters are stored

bscdata.readBMDEFilterDateRanges <- function(datasource="dendroica", project=c(), sitecode=c(), species=c(), maxrec=0, tablename="bmde_filter_date_ranges") {

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
  bmde.df <- sqlQuery(channel, toString(sql))
  odbcClose(channel)
  rm(channel)

  print(paste("Number of filters read:", length(bmde.df[,1])))
  return(bmde.df)
  }

