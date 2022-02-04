# Performs the trend estimations on a dataframe containing multiple subsets that need to be run individually
# (eg, once per species, area and season). The trends are estimated through Monte-Carlo simulations by calling
# bscanalysis.estimateTrendFromAnnualIndices.

# df          dataframe that contains the annual indices and se from which to calculate trends
# index.name  name of the variable in the dataframe that contains the annual indices (default="index")
# se.name     name of the variable in the dataframe that contains the SE of annual indices (default="se")
# year.name   name of the variable in the dataframe that contains the year variable (default="year")
# by.var      names of the combination of variables for which a trend will be calculated
#             (default=c("species_id", "area_code", "season", "period"))
#             (example, one estimate for each unique combination of species, area and season)
# n.iter      number of iterations when running each Monte-Carlo simulation (default=1000)

bscanalysis.estimateTrends <- function(df, index.name="index", se.name="se",
	year.name="year", by.var=c("species_id", "area_code", "season", "period"),
	n.iter=1000) {

	# gets a list of column index that correspond to the by variables.
	# For instance determines that species and area and the 4 and 5th column.

	which.by <- which(names(df) %in% by.var)

	# makes a dataframe that has only the list of unique combinations for the by variables

	unique.by <- unique(df[,which.by])

	# creates a loop that does something for each subset (doesn't really matter what, but in this case,
	# calculates an estimated population trend

	for (i in 1:NROW(unique.by)) {

		# gets a subset that contains only the values relevant for this loop.
		# this is done by merging the full dataframe with the line of unique.by that contains only
		# the values that we want for this loop.

		df2 <- merge(df, unique.by[i,])
		ttest <- bscanalysis.estimateTrendFromAnnualIndices(df2[[index.name]], df2[[se.name]], df2[[year.name]], n.iter)
		unique.by$intercept[i] <- ttest$intercept
		unique.by$est.trend[i] <- ttest$estimate
		unique.by$p.value[i] <- ttest$p.value
		unique.by$n.years[i] <- NROW(df2)
		unique.by$years[i] <- paste(as.character(min(df2[[year.name]])), "-", as.character(max(df2[[year.name]])), sep="")
	}
	return (unique.by)
}

# source("i:/r-functions/functions.r")
# df <- c()
# df$index <- rnorm(100, 2.5, 2) * (1:10)*.2
# df$se <- runif(100, 0.5, 2)
# df$year <- rep(2001:2010, 10)
# df$species_id <- rep(c(rep(1, 10), rep(2,10)), 5)
# df$stations <- rep(sort(rep(c("A","B","C","D","E"), 10)),2)
# df <- as.data.frame(df)
# by.var <- c("species_id", "stations")
# results <- bscanalysis.estimateTrends(df, by.var=by.var, n.iter=1000)
# results$sign <- bscdata.signLevel(results$p.value)
# results	
