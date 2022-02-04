# this function filters records from a BMDE dataframe based on a set of exclusion filters stored
# in a data table. Each record filter represents a set of conditions used to remove records from the BMDE
# table. Specific conditions are not used when equal to 0. For example, a filter record where survey_year = 2005
# (with all other filters set to 0) would result in the removal of all records from 2005. Filters can be as specific
# as needed, and are based on the following BMDE fields: project_id, SiteCode, species_id, survey_year, survey_month,
# and survey_day. A field named period can also be used, as long as it is present in the dataframe. Filters based on
# fields that are not present in the dataframe will generate errors.

# Parameters:
# df: dataframe to filter. Must contain at least all fields that have non-zero values in the filters used, among
#     the list of following possible fields: project_id, SiteCode, species_id, survey_year, survey_month,
#     survey_day and period.
# verbose: boolean. Whether details of each step should be printed.
# ...: optional parameters used to restrict which filters are used. Default is to use all date ranges available.
#      See other parameters in bscdata.readBMDEFilterDateRanges.

bscdata.filterBadDates <- function(df, verbose=FALSE, ...) {

  print(paste("dataframe size before transformation:", length(df[,1]), "records"))

  # exclude filtered records (based on date, project, sitecode and species when applicable)
  # any filter with a value > 0 will be used to exclude data from the bmde dataframe

  baddates.df <- bscdata.readBMDEFilterBadDates(...)
  baddates.df$filtered <- 1

  ffields <- c("project_id","SiteCode","species_id","survey_year","survey_month","survey_day","period")
  bfields <- names(df)

  # loops through individual filters to create a merge record, and any record found in bmde is removed
  for (i in 1:length(baddates.df[,1])) {
     byfields <- c()
     suppressWarnings(
       for (j in 1:length(ffields)) {
         if (!is.numeric(baddates.df[i, ffields[j]]) | (is.numeric(baddates.df[i, ffields[j]]) & baddates.df[i, ffields[j]] > 0)) byfields <- c(byfields,ffields[j])
       }
     )
     df <- merge(df, baddates.df[i,], by=byfields, suffixes=c("",".y"), all.x=T)

     if (verbose) {
       print(baddates.df[i,])
       print(paste("step ", as.character(i), ":", sum(!is.na(df[,"filtered"])), "records excluded"))
     }
     df <- subset(df, is.na(filtered), select=bfields)
  }

  print(paste("dataframe size after transformation:", length(df[,1]), "records"))

  return (df)

}

