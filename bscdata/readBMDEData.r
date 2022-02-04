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
# verbose: if TRUE, prints the progress during execution

bscdata.readBMDEData <- function(datasource="bmdedata", maxrec=5000, fields=bscdata.getFields("standard"), zerofill=FALSE, distinct=FALSE, bmdeformat=TRUE, verbose=FALSE, ...) {

  #build SQL string
  sql <- "SELECT "
  if (maxrec > 0) sql <- paste(sql, "top", maxrec)
  if (distinct) sql <- paste(sql, "distinct")

  #should validate list of fields first
  if (length(fields) > 0) {
    b <- " "
    for (i in 1:length(fields)) {
      sql <- paste(sql, b, "a.", fields[i], sep="")
      b <- ", "
    }
  }
  sql <- paste(sql, "FROM bmde_data a left outer join projects b on a.project_id = b.project_id")

  sql <- paste(sql, bscdata.getBMDESQLFilter(...), sep="")

  if (verbose) print(sql)

  channel <- odbcConnect(datasource)
  bmde.df <- sqlQuery(channel, toString(sql))
  odbcClose(channel)
  rm(channel)

  if (bmdeformat) {
     bmde.df <- bscdata.prepareBMDEDataframe(bmde.df, TRUE)
     print("Warning: some interpreted values may have been used in place of values originally stored in BMDE format.")
     print("To prevent this option, you must use the bmdeformat=FALSE parameter. The see a list of fields that have been modified,")
     print("you should refer to the documentation for the bscdata.prepareBMDEDataframe() function")
  }


  # the zero fill section needs to be replaced with the melt and cast function of the reshape package

  if (zerofill) {
    #if data must be zero filled, recurse to get the headers to do a cross-tabulation
    if (maxrec > 0) {
      warning("The option maxrec must =0 in order to use the zero-fill option. Returning unfilled data only.", call.=F)
      return(bmde.df)
    }
    #get a list of species from the data, and add species names
    species.df <- data.frame(species_id=unique(bmde.df$species_id))
    species.df <- merge(species.df, bscdata.specieslist)

    #get the headers
      headers.df <- bscdata.readBMDEData(datasource=datasource, project=project, collection=collection, sitecode=sitecode,
      prov=prov, startyr=startyr, endyr=endyr, bscdata.getFields("headers"), zerofill=FALSE, distinct=TRUE)

    #crosstabulate with species
    headers.df <- merge(headers.df, species.df)
    #adds the data
    bmde.df <- merge(headers.df, bmde.df, all.x=T)
    # replaces missing values of ObservationCount by zero
    bmde.df$ObservationCount[is.na(bmde.df$ObservationCount)] <- 0
    print(paste("Number of records read:", length(bmde.df[,1])))
    return(bmde.df)
  }
  else {
    print(paste("Number of records read:", length(bmde.df[,1])))
    return(bmde.df)
  }

}

