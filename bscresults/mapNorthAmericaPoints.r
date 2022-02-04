
bscresults.mapNorthAmericaPoints <- function(df.x, df.y, par.col="red", par.cex=.15, par.pch=15, par.width=325, par.height=325,
	shapefile="i:/r-functions/maps/us-canada2.shp", pngname=NA, ...) {

require(rgdal)
require(maps)
require(maptools)     # loads sp library too

# the projection parameters still need some tweakings

uscan.shp <- readShapeLines(shapefile,
   proj4string=CRS("+proj=lcc +lat_0=45 +lon_0=-101 +lat_1=35 +lat_2=85"))

if (!is.na(pngname)) png(filename = pngname, width=par.width, height=par.height, 
	pointsize=8, bg="white", res=NA, restoreConsole = FALSE)

plot(uscan.shp)
try(points(-abs(df.x), df.y, cex=par.cex, col=par.col, pch=par.pch, ...))

if (!is.na(pngname)) dev.off()

}