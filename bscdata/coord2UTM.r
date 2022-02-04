# converts latitude/longitude to UTM coordinates
# example: bscdata.coord2UTM (c(-80, -78), c(45, 46))
# an older version was using zone as a parameter, but all coordinates had to be in the same UTM zone
# this new version makes the calculation for each zone independently using a loop
# you should make sure that all longitudes are negative in the Western Hemisphere.

library(rgdal)

bscdata.coord2UTM <- function(lon, lat) {

if (max(lon) > 0) warning("Some longitudes are greater than 0. Check values to make sure you do have points outside of the Western Hemisphere")

zone <- as.integer((lon + 180)/6) + 1

df <- as.data.frame(cbind(lon=lon, lat=lat, zone=zone))
df$row <- 1:nrow(df)

zones <- sort(unique(df$zone))

df.utm <- c()

for (i in 1:length(zones)) {

   df.1 <- subset(df, zone == zones[i])

   SP <- SpatialPoints(cbind(df.1$lon, df.1$lat),
     proj4string=CRS("+proj=longlat"))

   utm.1 <- spTransform(SP, CRS(paste("+proj=utm +zone=", as.character(zones[i]), " +",
      ifelse(df.1$lat < 0, "south", "north"), sep="")))

   df.1 <- cbind(df.1, utm.1)
   names(df.1)[5:6] <- c("easting","northing")

   df.utm <- rbind(df.utm, df.1)

}
return(df.utm[order(df.utm$row),])

}

#bscdata.coord2UTM (c(-68, -78), c(45, 46))

