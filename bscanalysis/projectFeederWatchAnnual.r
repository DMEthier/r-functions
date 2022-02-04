# this function generates annual outputs for the PFW analysis.
# Needs to export results to a table, etc.

# parameters:
# maxrec: maximum number of tasks to return at once
# type: type of tasks to return (the table can contain R-PROJECT, SQL-SPROC, DTS, SSIS, etc.).
#     Normally, only R-PROJECT tasks need to be run from R
# alltasks: if TRUE, returns all tasks. Else, returns only tasks that are due to run based on a manual flag or a schedule
# datasource: name of ODBC datasource (normally dendroica, in this case)

# examples:
#   bscanalysis.getTasks()
#   bscanalysis.getTasks(alltasks=TRUE)


bscanalysis.projectFeederWatchAnnual <- function() {

	require(sqldf)

	# creates the unique list of species (by collection and province?)
	df.species <- bscdata.readBMDEData(collection="PFW",fields=c("collection","statprov_code","species_id"), bmdeformat=FALSE, maxrec=0, distinct=TRUE)

	# creates the unique list of sampling events (sites and dates)
	df.events <- bscdata.countBMDEData(collection="PFW",fields=c("collection","project_id","country_code",
		"statprov_code","subnational2_code","bcr","SiteCode","latitude","longitude","iba_site","SamplingEventIdentifier",
		"survey_year","survey_month","survey_week","survey_day"))

	df.events$iba_site[df.events$iba_site == "N/A"] <- NA
	df.events$statprov_code[df.events$statprov_code == "YT"] <- "YK"
	df.events$statprov_code[df.events$statprov_code == "NF"] <- "NL"

	df.events$region[df.events$statprov_code %in% c("YK","BC")] <- "BC"
	df.events$region[df.events$statprov_code %in% c("NT","AB","SK","MB")] <- "PR"
	df.events$region[df.events$statprov_code %in% c("ON")] <- "ON"
	df.events$region[df.events$statprov_code %in% c("QC")] <- "QC"
	df.events$region[df.events$statprov_code %in% c("NB","NS","PE","NL")] <- "AT"

	df.events$pfw_yr <- df.events$survey_year + ifelse(df.events$survey_month > 6, 1, 0)

	df.events <- subset(df.events, !is.na(region))

	df.data <- bscdata.readBMDEData(datasource="bmdedata", maxrec=0, fields=c("SamplingEventIdentifier","species_id","ObservationCount"),
		collection="PFW", bmdeformat=FALSE)

	df.speciestable <- bscdata.listBMDESpecies(showall=FALSE, datasource="bmdedata", collection="PFW")
	df.speciestable <- df.speciestable[with(df.speciestable, order(species_id)),]

	id <- 20390
	for (id in c(20390)) { # df.speciestable$species_id) {

		# 42261 and 20390 = Common Redpoll

		# get the table only for the species
		df.data.sp <- subset(df.data, species_id == id)

		# zero fill to create one row per species and sampling event
		df.data.sp <- merge(df.events, df.data.sp, by="SamplingEventIdentifier", all.x=TRUE)

		# assign missing species id and observation count
		df.data.sp$species_id[is.na(df.data.sp$species_id)] <- id
		df.data.sp <- merge(df.data.sp, df.species, by=c("collection","statprov_code","species_id"))
		df.data.sp$ObservationCount[is.na(df.data.sp$ObservationCount)] <- 0
		df.data.sp$has_bird <- !is.na(df.data.sp$ObservationCount) & df.data.sp$ObservationCount > 0

		# create a summary by site and year
		df.siteyr.sp <- sqldf("select collection, project_id, country_code, region, statprov_code, subnational2_code, bcr, 
		   SiteCode, latitude, longitude, iba_site, pfw_yr, species_id,
		   count(ObservationCount) as n_weeks, sum(has_bird) as n_weeks_obs, 
		   avg(ObservationCount) as avg_count, sum(ObservationCount * has_bird) / sum(has_bird) as group_size,
		   max(ObservationCount) as max_count
		   from [df.data.sp]
		   group by collection, project_id, country_code, region, statprov_code, subnational2_code, bcr, 
		   SiteCode, latitude, longitude, iba_site, pfw_yr, species_id")

		# create a summary by province and year
		df.provyr.sp <- sqldf("select collection, project_id, country_code, statprov_code, 
		   pfw_yr, species_id,
		   avg(n_weeks) as n_weeks, avg(n_weeks_obs) as n_weeks_obs, 
		   avg(avg_count) as avg_count, avg(group_size) as group_size,
		   max(max_count) as max_count, count(group_size) as n_sites_reporting,
		   count(n_weeks) as n_sites_total
		   from [df.siteyr.sp]
		   group by collection, project_id, country_code, statprov_code, 
		   pfw_yr, species_id")

		df.provyr.sp <- df.provyr.sp[with(df.provyr.sp, order(statprov_code, pfw_yr)),]

		df.provyr.sp.melt <- melt(df.provyr.sp, id=c("collection", "project_id", "country_code", "statprov_code","pfw_yr","species_id"), 
		   measure.vars=c("avg_count"))

		df.provyr.sp.cast <- cast(df.provyr.sp.melt, statprov_code ~ pfw_yr ~ variable, mean)
		df.provyr.sp.cast 

		with(subset(df.provyr.sp, statprov_code == "AB"), plot(avg_count ~ pfw_yr))
		with(subset(df.provyr.sp, statprov_code == "AB"), lines(avg_count ~ pfw_yr))


		# create a summary by region and year
		df.regionyr.sp <- sqldf("select collection, project_id, country_code, region, 
		   pfw_yr, species_id,
		   avg(n_weeks) as n_weeks, avg(n_weeks_obs) as n_weeks_obs, 
		   avg(avg_count) as avg_count, avg(group_size) as group_size,
		   max(max_count) as max_count, count(group_size) as n_sites_reporting
		   from [df.siteyr.sp]
		   group by collection, project_id, country_code, region, 
		   pfw_yr, species_id")

		df.regionyr.sp <- df.regionyr.sp[with(df.regionyr.sp, order(region, pfw_yr)),]

      }

}

