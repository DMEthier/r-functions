# creates an inner function that can be called to generate the formula
# this can be used by other functions such as annualIndices, etc.

# count.var: name of variable used as count (default: ObservationCount = number of individuals detected at each instance)
# model.dist: model distribution from the value possible values:
#          log.trans (log-transformed), logist (logistic), nb (neg. binomial)
#          pois (poisson)
# year.order: order of polynomial for year variable in the model (this is only used with the 
#          drop.zero option. The annual indices are always calculated using year as factors.
#          To use year as a factor (discrete values), set year.order = 0.
# doy.order: order of polynomial for day of year (doy) variable in the model(s). 
#          To exclude the doy term, set doy.order = 0.
# model.covar: vector of variable names used as covariables (other than year and doy)
#         To use polynomials, use the form "poly(TimeObservationsStarted,2)"

bscanalysis.getFormula <- function(count.var, model.dist, year.order, doy.order, model.covar) {
  return(as.formula(paste(ifelse(model.dist=="log.trans", paste("log(", count.var, " + 1)", sep=""), count.var),
    ifelse(year.order > 0," ~ poly(year, year.order) "," ~ yearfac "),   
    ifelse(doy.order > 0, " + poly(doy, doy.order)", ""),
    ifelse(length(model.covar)>0,paste(" + ", paste(model.covar, collapse=" + ")), ""), 
    sep="")))
} # end of fn.getFormula

