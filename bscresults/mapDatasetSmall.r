
obfs.latlon <- bscdata.countBMDEData(collection="OBFS", fields=c("DecimalLatitude","DecimalLongitude"))

xcoord <- obfs.latlon$DecimalLongitude
ycoord <- obfs.latlon$DecimalLatitude

mapDatasetSmall <- function(xcoord, ycoord) {

    require(rgdal)

    uscan.shp <- readShapeLines(paste(functionDir, "us-canada2.shp", sep=""),
       proj4string=CRS("+proj=lcc +lat_0=45 +lon_0=-101 +lat_1=35 +lat_2=85"))
    plot(uscan.shp)

    points(xcoord, ycoord, cex=.4, col="red", pch=15)

    #proj4string(uscan.shp)

}

