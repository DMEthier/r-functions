# Creates a dataframe of x and y coordinates that ensures that all points contained are separated by a minimum
# buffer distance from each other. This function can be used for instance when displaying points on a graphic
# or a map to ensure that the symbols used do not overlap each other. This is an iterative process because the
# comparisons are always only done between pairs of points. Once the points are moved, they may fall within the
# buffer of another point, so the process gradually expand the points.

# x            x coordinates of reference points
# y            y coordinates of reference points
# buffer       buffer distance to use
# bounds.x     vector of minimum and maximum x values to prevent points from falling off a grid. Eg, c(0, 100)
# bounds.y     vector of minimum and maximum y values to prevent points from falling off a grid. Eg, c(0, 100)
# show.out     whether the points that are already outside the boundaries are included or not. If so, they are
#              moved to the edge of the area. If not, they are removed from the x and y vectors, which may result
#              in a dataframe with fewer points than the original vectors
# show.plot    whether the results are displayed on a plot at each iteration
# max.iter     maximum number of iteration

# Note that this can take a relatively long time and may not be well suited for datasets with more than a
# few hundred points (for instance a dataset of 1,000 points will need to perform 499,500 distance calculations
# at each iteration, and this number will increase exponentially as per this formula: (x^2 / 2) - (x/2). A dataset
# of 10,000 points will necessitate about 50 million operations at each iteration. Also, the number of iterations
# will depend on the degree of clumpiness of your points, and may fail to converge to a solution. If that happens,
# you can always continue the process by sending your corrected set of coordinates to the function, rather than
# start again from your original values with a higher number of max.iter.

bscdata.spreadPoints <- function (x, y, buffer, bounds.x=NA, bounds.y=NA, show.out=TRUE, show.plot=FALSE, max.iter=250) {

   # defines a temporary function for distance. This could also be a geographic distance, but I see no use for it
   # at the moment, and it would complicate the spreading formula.
   # dist.fn <- function(x1, y1, x2, y2) {return(sqrt((x1 - x2) ^2 + (y1 - y2) ^2)) }

   # returns the delta distance (x and y directions) needed for each point, so they are
   # at least a buffer distance away from each other
   # points are moved away from each other along their existing axis.
   # If the points overlap exactly, one of them is moved randomly a short distance away before
   # x1, y1, x2, y2 are the x and y coordinates of point1 and point2, respectively.

   delta.fn <- function(x1, y1, x2, y2, buffer) {
      # special case if the points overlap exactly
      if (x1 == x2 & y1 == y2) {
         x1 <- jitter(x1, 0.001)
         y1 <- jitter(y1, 0.001)
      }
      # d is the distance between point1 and point2
      d <- sqrt((x1 - x2) ^2 + (y1 - y2) ^2)
      # if the points are separated by at least the buffer, the do not need to move.
      # Adds a 1% error to account for a minor loss of precision. This ensures that points moved
      # at a fixed distance do not appear to need moving again later by a very small distance (eg, 1E-6).

      if (d * 1.01 > buffer) return (c(0, 0, 0, 0))

      # dx and dy are the distance between point1 and point2 on the x and y axis

      dx <- x1 - x2
      dy <- y1 - y2

      # theta are the angles in a cartesian grid between point1 and point2 for the x and y axis
      # the sum of both is always equal to pi/2

      theta.x <- acos(dx/d)
      theta.y <- asin(dy/d)

      #d2 is the distance by which each point needs to be moved
      #dx1, dy1, dx2 and dy2 are the deltas, or the value that needs to be added to each of the coordinates

      d2 <- (buffer - d) / 2
      dx1 <- d2 * cos(theta.x)
      dx2 <- d2 * cos(theta.x + pi)
      dy1 <- d2 * sin(theta.y)
      dy2 <- d2 * sin(theta.y + pi)
      return (c(dx1, dy1, dx2, dy2))
   }


# Compare all pairs of points and spread those that fall within the buffer distance.
# Create a temporary set of coordinates, so the calculations for each point are done on
# the starting set (otherwise, can change the relative position). The deltas cumulated and only added to the previous
# set of coordinates at the end of the loop. The loop is repeated until no point needs to be moved,
# or until the maximum number of iterations has been reached

df1 <- as.data.frame(cbind(x, y))

if (length(bounds.x) > 1) {
   if (show.out) {
      df1$x[df1$x < bounds.x[1]] <- bounds.x[1]
      df1$x[df1$x > bounds.x[2]] <- bounds.x[2]
   }
   else {
      df1 <- subset(df1, x >= bounds.x[1] & x <= bounds.x[2])
   }
}
if (length(bounds.y) > 1) {
   if (show.out) {
      df1$y[df1$y < bounds.y[1]] <- bounds.y[1]
      df1$y[df1$y > bounds.y[2]] <- bounds.y[2]
   }
   else {
      df1 <- subset(df1, y >= bounds.y[1] & y <= bounds.y[2])
   }
}

if (length(x) < length(df1$x) | length(y) < length(df1$y))
      warning("Some points that were outside of the defined boundaries have been removed")

# keep the original values in the dataframe
df1$x.orig <- df1$x
df1$y.orig <- df1$y

# plot the points, mostly for debugging purposes
if (show.plot) {
	if (length(bounds.x) <= 1) b.x <- c(min(df1$x),max(df1$x))
	else b.x <- bounds.x
	if (length(bounds.y) <= 1) b.y <- c(min(df1$y),max(df1$y))
	else b.y <- bounds.y
	plot(df1$y ~ df1$x, xlim=b.x, ylim=b.y, col=1)
}

# k is the index of the iterations
k <- 1
repeat {
   df1$dx <- 0
   df1$dy <- 0

   for (i in 1:NROW(df1)) {
      if (i < NROW(df1)) for (j in (i+1):NROW(df1)) {
         delta <- delta.fn(df1$x[i], df1$y[i], df1$x[j], df1$y[j], buffer)
         df1$dx[i] <- df1$dx[i] + delta[1]
         df1$dy[i] <- df1$dy[i] + delta[2]
         df1$dx[j] <- df1$dx[j] + delta[3]
         df1$dy[j] <- df1$dy[j] + delta[4]
      }
    }

    # add the deltas
    df1$x <- df1$x + df1$dx
    df1$y <- df1$y + df1$dy

    # truncate to outer boundaries
    if (length(bounds.x) > 1) {
       df1$x[df1$x < bounds.x[1]] <- bounds.x[1]
       df1$x[df1$x > bounds.x[2]] <- bounds.x[2]
    }
    if (length(bounds.y) > 1) {
       df1$y[df1$y < bounds.y[1]] <- bounds.y[1]
       df1$y[df1$y > bounds.y[2]] <- bounds.y[2]
    }

    if (show.plot) points(df1$y ~ df1$x , col=(k %% 16)+1)
    k <- k + 1
    if (k > max.iter | (sum(abs(df1$dx)) < 0.0001 & sum(abs(df1$dy)) < 0.0001)) break
}

if (k > max.iter & (sum(abs(df1$dx)) >= 0.0001 | sum(abs(df1$dy)) >= 0.0001)) warning("Failed to converge. Some points are still within a buffer distance. You may need to increase the maximum iterations.")
if (k <= max.iter & sum(abs(df1$dx)) < 0.0001 & sum(abs(df1$dy)) < 0.0001) warning(paste("Convergence reached in ", as.character(k-1), " iterations.", sep=""))

return(df1)

}

# An example
#x <- runif(40, 1, 100)
#y <- runif(40, 1, 100)
#df2 <- bscdata.spreadPoints (x, y, 16, bounds.x=c(1,100), bounds.y=c(1,100), 
#	show.plot=T, max.iter=100) 
