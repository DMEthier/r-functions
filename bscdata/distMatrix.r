# this function returns a 2-dimensional matrix of geographic distances (in meters) between pairs of points.
# The main diagonal (the distance of a point to itself) are all set to NA's. Each distance are provided twice
# because the distance from point A to B is the same as the distance from B to A. Because of this, you may need
# to use only the entries on either side of the main diagonal. Note that lat and lon should have the same length.
# the index of the matrix corresponds to the index of the lat and lon vectors. Eg, the distance between point #1 and
# point #5 in your lat and lon vectors is found at position [1, 5] in the resulting matrix (as well as at [5, 1]).

# lat  Vector of Latitude values (degrees north). Longitude in the southern hemisphere should be negative.
# lon  Vector of Longitude values (degrees east). Longitude in the western hemisphere should be negative.

bscdata.distMatrix <- function (lat, lon) {
   dist <- matrix(nrow=length(lat), ncol=length(lat) )

   # calculate a matrix of distance between each pairs of points
   for (i in 1:length(lat)) {
      for (j in i:length(lat)) {
         if (i == j) dist[i, j] <- NA
         else {
            dist[i, j] = bscdata.distAB(lat[i], lon[i], lat[j], lon[j])
            dist[j, i] = dist[i, j]
         }
      }
   }
   return (dist)
}


