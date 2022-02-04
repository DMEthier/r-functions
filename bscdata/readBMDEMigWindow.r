# this function creates a dataframe from the SQL table using the BMDE format
# it extracts the Migration Windows of specified species from specified stations and programs (results_code)

# Parameters:
# datasource: name of ODBC datasource used to create connection to database
# results_code: vector of id's used to filter the data based on Project Code (e.g., CMMN). default is empty vector (not filter)
# area_code: vector of codes used to filter the data based on Area Code (e.g. LPBO). default is empty vector (not filter)
# species_id: vector of species id's used to filter the data based on species ID. default is empty vector (not filter).
#          Species id's are numeric values. To filter based on species codes, you can use the function
#          bscdata.getSpeciesCodes()
# maxrec: max. number of records returned in the dataframe. This is mainly to prevent creating very large dataframes
#         by mistake. The default is 5000 records. Use 0 to return all records.

bscdata.readBMDEMigWindow <- function(datasource="dendroica", results_code=c(), area_code=c(), species_id=c(), maxrec=0) {

  if (length(results_code) == 1 && results_code == 0) results_code <- c()
  if (length(area_code)  == 1 && area_code == 0) area_code <- c()
  if (length(species_id) == 1 && species_id == "") species_id <- c()

  #build SQL string
  sql <- "SELECT * FROM results_areas_windows"
  a <- " WHERE "

  if (length(results_code) > 0) {
    sql <- paste(sql, a, "results_code in (", bscdata.getSQLINList(results_code), ")", sep="")
    a = "AND"
  }
  if (length(area_code) > 0) {
    sql <- paste(sql, a, " area_code in (", bscdata.getSQLINList(area_code), ")", sep="")
    a = "AND"
  }
  if (length(species_id) > 0) {
    sql <- paste(sql, a, " species_id in (", bscdata.getSQLINList(species_id), ")", sep="")
    a = "AND"
  }

  print(sql)
  channel <- odbcConnect(datasource)
  bmde.df <- sqlQuery(channel, toString(sql))
  odbcClose(channel)
  rm(channel)

  print(paste("Number of records read:", length(bmde.df[,1])))
  return(bmde.df)
  }

 #df <- bscdata.readBMDEMigWindow()
 #df <- bscdata.readBMDEMigWindow(area_code="LPBO1")
