
# this internal function creates a the SQL needed to filter BMDE records. Normally only called by other functions.

# Parameters:
# project: vector of id's used to filter the data based on project ID's. default is empty vector (not filter)
# collection: vector of codes used to filter the data based on collection codes. default is empty vector (not filter)
# resultscode: vector of resultscodes used to filter the data based on results codes. default is empty vector (not filter)
# sitecode: vector of codes used to filter the data based on site codes. default is empty vector (not filter)
# areacode: vector of codes used to filter the data based on area codes. default is empty vector (not filter)
# species: vector of species id's used to filter the data based on species ID. default is empty vector (not filter).
#          Species id's are numeric values. To filter based on species codes, you can use the function
#          bscdata.getSpeciesCodes()
# prov: vector of codes used to filter the data based on province/state codes. default is empty vector (not filter)
# shapes: vector of shape codes for counties, Important Bird Areas, Bird Conservation Regions, etc.
# startyr: earliest year (use 0 if no filter needed)
# endyr: final year  (use 0 if no filter needed)
# startday: earliest day of the year (note Jan 1st=1; use 0 if no filter needed)
# endday: final day of the year (note Jan 1st=1; use 0 if no filter needed)
# prefix: table alias used by the SQL SELECT command for the main table (eg, "a" is the alias used here for bmde_data: select * from bmde_data a).
#         A prefix is required in SQL to differentiate fields in cases where 2 tables joined have the same field name.

bscdata.getBMDESQLFilter <- function(project=c(), collection=c(), resultscode=c(), sitecode=c(), areacode=c(), species=c(),
   prov=c(), shapes=c(), startyr=0, endyr=0, startday=0, endday=0, minlat=NA, maxlat=NA,
   minlon=NA, maxlon=NA, prefix="a") {

  if (length(project) == 1 && project == 0) project <- c()
  if (length(collection)  == 1 && collection == 0) collection <- c()
  if (length(resultscode)  == 1 && resultscode == 0) resultscode <- c()
  if (length(sitecode) == 1 && sitecode == "") sitecode <- c()
  if (length(areacode) == 1 && areacode == "") areacode <- c()
  if (length(species) == 1 && species == 0) species <- c()
  if (length(prov) == 1 && prov == "") prov <- c()
  if (length(shapes) == 1 && shapes == "") shapes <- c()

  if (!is.na(prefix)) prefix <- paste(" ", prefix, ".")
  else prefix <- ""

  sql <- ""
  a <- " WHERE "
  if (length(project) > 0) {
    sql <- paste(sql, a, prefix, "project_id in (", bscdata.getSQLINList(project), ")", sep="")
    a = " AND"
  }
  if (length(sitecode) > 0) {
    # looks whether any of the sitecode values contains a % character for partial match searches (eg, VIVI%)
    if ("%" %in% substr(sitecode, nchar(sitecode), nchar(sitecode))) {
      sql <- paste(sql, a, " (", bscdata.getSQLLike("SiteCode", sitecode), ") ", sep="")
    } else {
      sql <- paste(sql, a, " SiteCode in (", bscdata.getSQLINList(sitecode), ")", sep="")
    }
    a = " AND"
  }
  if (length(areacode) > 0) {
    sql <- paste(sql, a, " area_code in (", bscdata.getSQLINList(areacode), ")", sep="")
    a = " AND"
  }
  if (length(collection) > 0) {
    sql <- paste(sql, a, " collection in (", bscdata.getSQLINList(collection), ")", sep="")
    a = " AND"
  }
  if (length(resultscode) > 0) {
    sql <- paste(sql, a, " results_code in (", bscdata.getSQLINList(resultscode), ")", sep="")
    a = " AND"
  }
  if (startyr > 0) {
    sql <- paste(sql, a, " survey_year >= '", startyr, "'", sep="")
    a = " AND"
  }
  if (endyr > 0) {
    sql <- paste(sql, a, " survey_year <= '", endyr, "'", sep="")
    a = " AND"
  }
  if (startday > 0) {
    sql <- paste(sql, a, " DATEPART(dayofyear, CAST([survey_year] AS varchar(4)) + '/' + CAST([survey_month] AS varchar(2)) + '/' + CAST([survey_day] AS varchar(2))) >= ", startday, sep="")
    a = " AND"
  }
  if (endday > 0) {
    sql <- paste(sql, a, " DATEPART(dayofyear, CAST([survey_year] AS varchar(4)) + '/' + CAST([survey_month] AS varchar(2)) + '/' + CAST([survey_day] AS varchar(2))) <= ", endday, sep="")
    a = " AND"
  }
  if (length(prov) > 0) {
    sql <- paste(sql, a, " statprov_code in (", bscdata.getSQLINList(prov), ")", sep="")
    a = " AND"
  }
  if (length(species) > 0) {
    sql <- paste(sql, a, " species_id in (", bscdata.getSQLINList(species, numeric=T), ")", sep="")
    a = " AND"
  }
  if (length(shapes) > 0) {
    sql <- paste(sql, a, " exists (select record_id from vwBmdeDataShapes shapes with (noexpand) where ",
    prefix, "record_id = shapes.record_id AND shapes.shape_value in (", bscdata.getSQLINList(shapes), "))", sep="")
    a = " AND"
  }
  if (!is.na(minlat)) {
    sql <- paste(sql, a, " latitude >= ", minlat, sep="")
    a = " AND"
  }
  if (!is.na(maxlat)) {
    sql <- paste(sql, a, " latitude <= ", maxlat, sep="")
    a = " AND"
  }
  if (!is.na(minlon)) {
    sql <- paste(sql, a, " longitude >= ", minlon, sep="")
    a = " AND"
  }
  if (!is.na(maxlon)) {
    sql <- paste(sql, a, " longitude <= ", maxlon, sep="")
    a = " AND"
  }

  return (sql)
}
