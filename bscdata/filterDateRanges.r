# this function filters records from a BMDE dataframe based on a set of dates (start-end dates), stored
# in a data table. Unmatched records (records with no date ranges available) or records outside the date
# range are excluded from the resulting dataframe.

# Parameters:
# df: dataframe to filter. Must contain at least the following fields, used to merged with the filter table:
#     project_id, SiteCode and species_id, as well as doy (day of year) and ObservationCount.
# ...: optional parameters used to restrict which filters are used. Default is to use all date ranges available.
#      See other parameters in bscdata.readBMDEFilterDateRanges.

bscdata.filterDateRanges <- function(df, ...) {

  print(paste("dataframe size before transformation:", length(df[,1]), "records"))

  # merge date filters and define in which period each date falls. If there are more than 1 period defined
  # for the range of dates in the dataset, you may have more than 1 row per
  migwin.df <- bscdata.readBMDEFilterDateRanges(...)
  df <- merge(x=df, y=migwin.df, by=c("project_id","SiteCode","species_id"))

  # exclude dates outside of the defined periods and zero counts
  # if start date is after end date, assume it is over the winter season

  df <- subset(df, !is.na(start_date) & !is.na(end_date) &
     ((start_date <= end_date & (doy >= start_date & doy <= end_date)) |
      (start_date > end_date & (doy >= start_date | doy <= end_date))) &
     as.integer(ObservationCount) > 0)

  print(paste("dataframe size after transformation:", length(df[,1]), "records"))

  return(df)

}

