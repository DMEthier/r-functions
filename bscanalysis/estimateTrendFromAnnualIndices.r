# calculates a population trend using a Monte-Carlo simulation, based 
# on annual indices and standard errors. 

# index.var    vector of annual indices
# se.var       vector of standard errors of the annual indices
# year.var     vector of years
# n.iter       number of iterations (1000 by default)

# Returns a dataframe of one row with the mean trend (estimate) and the estimated p value
# (number of iterations that are smaller or larger than zero)

bscanalysis.estimateTrendFromAnnualIndices <- 
	function(index.var, se.var, year.var, n.iter=1000) {

	year <- year.var - median(year.var)
	n.years <- length(year.var)
	if (n.years != length(unique(year.var))) 
		warning("The year variable is not unique")

	# for each year, creates an vector of random values with 
	# mean = index[year] and std = se[year]

	sim.data <- array(NA, c(n.iter, n.years))
	for (i in 1:n.years)  sim.data[,i] <- rnorm(n.iter, 
		index.var[year==year[i]], se.var[year==year[i]])

	# for each iteration, calculates the linear trend

	sim.intercept <- c()
	sim.trend <- c()
	for (i in 1:n.iter) {
		l1 <- lm(sim.data[i,] ~ year)
		sim.intercept[i] <- l1$coefficients[["(Intercept)"]]
		sim.trend[i] <- l1$coefficients[["year"]]
	}

	# this is equivalent to a two-tailed t-test. 
	intercept <- mean(sim.intercept)
	estimate <- mean(sim.trend)
	p.value <- length(sim.trend[sim.trend <= (estimate - abs(estimate)) |
		sim.trend >= (estimate + abs(estimate))]) / n.iter 
	# hist(sim.trend)
	return(as.data.frame(cbind(intercept, estimate, p.value)))
	
}

# Example

# index.var <- c(5, 12, 13, 11, 13, 12, 11, 10, 16)
# year.var <- 2001:2009
# se.var <- runif(9, 1, 3)
# plot(index.var ~ year.var, ylim=c(0,20))
# bscresults.addErrorBars(year.var, index.var, se.var)
# bscanalysis.estimateTrendFromAnnualIndices(index.var, se.var, year.var, n.iter=1000) 
