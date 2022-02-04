## calculates ANNUAL INDICES for the provided dataset
## modified by Denis Lepage June 2009
## modified by Phil Taylor from Tara Crewe's scripts
## 1 Jan 2009

# Parameters:
# df: dataframe to analyze. Must contain at least the following fields using BMDE names:
# count.var: name of variable used as count (default: ObservationCount = number of individuals detected at each instance)
# model.covar: vector variable names used as covariables (other than year and doy)
#         To use polynomials, use the form "poly(TimeObservationsStarted,2)"
# by.var: list of variable names used to subset the data (eg, if SpeciesCode is used, the indices are
#         calculated individually for each SpeciesCode value). If not value is provided, the indices are
#         calculated on the entire data frame (which should only be used if the dataframe
#         is already a subset ready for analysis).
# model.dist: model distribution from the value possible values:
#          log.trans (log-transformed), logist (logistic), nb (neg. binomial)
#          pois (poisson)
# drop.zero: boolean. Whether observations with predicted values of zero are removed. This step should only be necessary 
#          for log.trans models.
# weight.var: name of the variable used for weight in the model
# year.order: order of polynomial for year variable in the model (this is only used with the 
#          drop.zero option. The annual indices are always calculated using year as factors.
#          To use year as a factor (discrete values), set year.order = 0.
# doy.order: order of polynomial for day of year (doy) variable in the model(s). 
#          To exclude the doy term, set doy.order = 0.


# Example: 
# ai.df <- bscanalysis.annualIndices(df, year.order=2, by.var=c("reps"), model.dist=c("log.trans", "nb", "pois", "logist"), drop.zero=c(T,F,F,F)) 

  bscanalysis.annualIndices <- function(df, count.var="ObservationCount", doy.order=2, year.order, 
	model.covar=c(), by.var=c(), weight.var=NA, model.dist, drop.zero=F) {

## Because dataframe has observations for every species, every day, 
## in every year (when observations were made) manipulations of ranges 
##(as shown below) can be used to calculate an approximate midpoint. 
## use a named vector (yr.midpoints) here to properly assign different 
## midpoints to each year. The as.numeric below gets rid of the named list 
## component, which seems to cause trouble in the glm function

  df$yearfac <- as.factor(df$YearCollected)
  df$year <- df$YearCollected - trunc(sum(range(df$YearCollected))/2)

  datetime <- bscdata.getPOSIXlt(ifelse(is.na(df$YearCollected),2001,df$YearCollected), df$MonthCollected, df$DayCollected, df$TimeObservationsStarted)
  df$doy <- datetime$yday
  df$TimeObservationsStarted <- as.numeric(df$TimeObservationsStarted)

  yr.midpoints <- with(df, tapply(doy, year, function(x) trunc(sum(range(x))/2)))
  df$doy <- as.numeric(df$doy - yr.midpoints[as.character(df$year)] ) 

  # if only one value of drop zero is provided, assumes the same condition for all models
  if (length(drop.zero) == 1 & length(model.dist) > 1) drop.zero <- rep(drop.zero, length(model.dist))

## Generate a single unique group variable, based on the combinations of values
## from the by.var variables (eg, "SiteCode: LPBO; SpeciesCode: SSHA; "). Indices will be calculated
## independently for each unique values of group.var.
## There may be a better method to do this?

  df$group.var <- ""
  if (length(by.var) > 0) {
    for (i in 1:length(by.var)) {
      df$group.var <- paste(df$group.var, by.var[i], ": ", df[[by.var[i]]], "; ", sep="")
    }
  }

  groups <- unique(df$group.var)

## Adds a column called gr.index to the list of groups that is the index 
## values that corresponds to the windows tables (for subsetting the data later)
## This step doesn't appear necessary yet

  gr.names <- as.data.frame(cbind(groups, 1:length(groups)))
  names(gr.names) <- c("group.var", "gr.index")
  df.copy <- merge(df, gr.names)

## Then add the individual by.var variables, so they can be exported in the final
## table of annual indices. This method doesn't seem very efficient however as it needs to summarize the
## entire dataframe. Oh well.

  gr.names <- unique(subset(df, select=c("group.var", by.var)))

## Original method uses log(n + 1) and computes two regressions.
## The first regression refines the migration window of the species by deleting
## cases at the tails of the distribution.  This will improve the distribution
## of residuals in the final (second) regression. It is unnecessary when using
## a poisson or a negative binomial regression.

## Which species do you want to model?
## Don't eliminate species by minimum of 20 per year; this is unecessary;
## could do the calculations anyway, or simply input a list of species
## later for which you want (or don't want) an index calculated
## If this step is needed, those extra parameters will have to be added to
## the function (eg, min.birds or set.melt)

## For example, can easily calculate the total counts for each species
## by each year.
## uses set.melt, created in previous program, 'rpi correct counts.R'
#  set.sum.year <- cast(set.melt, SpeciesCode + year ~ variable, fun.aggregate = sum, na.rm = TRUE)
#  sp.means <- with(set.sum.year, tapply(ObservationCount, SpeciesCode, mean))
#  sp.list <- names(sp.means)[sp.means > 5]

## first calculate linear models for each species, to eliminate data where the
## predicted values fall below some point (this is also unnecessary
## in the glm framework)

## do it this way (actually passing the data frame back to the list) because
## you can be sure then that the predicted values are properly associated
## with the raw data values.

  tmp.index.all <- c()

# looping through each model type that you want to try
  for (k in 1:length(model.dist)) {

# work from a new copy at each iteration (because of the filtering that may happen later)
     df.copy <- df
	
    if (model.dist[k] == "logist") {
      df.copy[[count.var]] <- ifelse(df.copy[[count.var]] > 0, 1, 0)
    }

    if (drop.zero[k] == T) {
      # creates the formula for the predictive model
      fmla <- bscanalysis.getFormula(count.var, model.dist[k], year.order, doy.order, model.covar)

	first.predict <- list()
	for (x in 1:length(groups)) {
        tmp <- subset(df.copy, group.var == groups[x])
	  tmp.pd <- predict(bscanalysis.runGlm(tmp, fmla, model.dist[k], weight.var))
	  first.predict[[groups[x]]] <- data.frame(tmp, predict.dat = tmp.pd) 
	} #end of first predict loop

      # turn the list back into a vector and subset on the predicted values
	# this was giving me an error until I removed the datetime field

	df.sub <- do.call("rbind", first.predict)
	
	df.copy <- subset(df.sub, predict.dat > 0, select=-predict.dat)

    } # END OF if drop.zero statement (gives you choice to drop values predicted to be zero or below)

    ## and fit the models again (this time with year as a factor)
    ## and return the whole model summary to the list

    # year.order = 0 forces to use yearfac
    fmla <- bscanalysis.getFormula(count.var, model.dist[k], year.order=0, doy.order, model.covar)

    glm.df <- list()
    for (x in 1:length(groups)) {
      tmp <- subset(df.copy, group.var == groups[x])
	glm.df[[groups[x]]] <- bscanalysis.runGlm(tmp, fmla, model.dist[k], weight.var) 
    } #end of glm.df loop

## The index is the predicted value from the regression added to half the mean
## square error (deviance(glm)/df.residual(glm)). The predicted values are
## calculated by summing the predicted values from every day of observation,
## in every year

## Although I think that the SAS code says to add the mean square error,
## I think it really should be the root mean square error -- since that
## is in units of individual birds, whereas the
## mean square error is in units of birds^2.

    tmp.index <- list()
    for (x in 1:length(glm.df)) {

## create a new data frame with a value for every day (even days without observations)
## in every year (only years with observations) and predict the log(number) of individuals
## of a given species (species x) observed on that day.

      glm.x <- glm.df[[x]] #subset out the model for easy manipulation below
      if (model.dist[k] == "nb" ) {yearfac <- levels(glm.x$model$yearfac)}
      else {yearfac <- sort(unique(glm.x$data$yearfac))}

## calculate the number of days
## I don't understand why the 'by' step is only 1/100 in your code

      doy <- seq(min(tmp$doy), max(tmp$doy), by = 1)

## create the new dataframe, and generate predicted values
      new.data <- expand.grid(doy = doy, yearfac = yearfac)
      new.data$predict.dat <- predict(glm.x, newdata = new.data)

## Compute the geometric mean of the predicted values for each year.
## The code returns a data frame with year and the geometric mean (x)
## The value is the equivalent of avhpd (average hawks per day in each year,
## for each species) in the SAS programs. To me (pdt) the estimated number
## seems to be higher than it should be.  Possibly (tlc) because of the way
## numbers of hawks are corrected???

      tmp.tot <- with(new.data, aggregate(predict.dat, list(yearfac = yearfac),
        function(x2) {
          tmp <- exp(x2) - 1
          tmp <- log(mean(tmp) + 1)
          #  tmp <- exp(tmp + sqrt(deviance(glm.x) / df.residual(glm.x)) / 2) - 1
          tmp <- exp(tmp) - 1
          return(tmp)
      })) # end of tmp.tot

## add extra info to the data frame for later reference

      tmp.tot$group.var <- groups[x]
	tmp.tot <- merge(tmp.tot, gr.names)
      tmp.tot$model <- model.dist[k]
      tmp.index[[groups[x]]] <- tmp.tot
    } # end of tmp.index loop 

## convert list output of indices into dataframe
    tmp.index <- do.call("rbind", tmp.index)
    tmp.index.all <- rbind(tmp.index.all, tmp.index)

  } # end of if model str statement

  row.names(tmp.index.all) <- c()
  return(tmp.index.all)

} #end of annual indices function
