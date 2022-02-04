# returns the Great Circle Distance in meters between a reference point and a series of coordinates.
# formula taken from http://www.meridianworlddata.com/Distance-Calculation.asp

# lat1  Latitude of reference point (degrees north)
# lon1  Longitude of reference point (degrees east)
# lat2  Latitude of points of interest (vector) (degrees north).
# lon2  Longitude of points of interest (vector) (degrees east).
# a     Radius of the Earth. The following alternative values can be used if a different return unit is required:
#       a=3437.74677 (nautical miles), a=6378.7 (kilometers), a=3963.0 (statute miles)

bscdata.distAB <- function (lat1, lon1, lat2, lon2, a=6378700) {

   # converts decimal degrees to radiants
   lat1 <- lat1 * pi / 180
   lon1 <- lon1 * pi / 180
   lat2 <- lat2 * pi / 180
   lon2 <- lon2 * pi / 180

   v <- c()
   for (i in 1:length(lat2)) {
     v[i] <- a * acos(sin(lat1) *  sin(lat2[i]) + cos(lat1) * cos(lat2[i]) * cos(lon2[i] - lon1))
   }

   return(v)

}


