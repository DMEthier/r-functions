# df.col <- bscdata.listBMDECollections(showall=F)
# pngprefix <- "c:/r-files/maps/map_"
# bscresults.mapNorthAmericaSmallMaps(df.col$collection_code, pngprefix)

bscresults.mapNorthAmericaSmallMaps <- function(
	collections=bscdata.listBMDECollections(showall=F)$collection_code,
	pngprefix=paste(getwd(), "/map_", sep="")
) {

for (i in 1:length(collections)) {
	#gets unique lat-lon for collection
	df <- bscdata.countBMDEData(collection=collections[i], fields=c("latitude","longitude"))
	df <- subset(df, !is.na(latitude) & !is.na(longitude))
	fn <- paste(pngprefix, collections[i], ".png", sep="")
	if (length(df[,1]) < 10) c <- 1.2
	else c <- 0.3
	
	dfx <- as.numeric(df$longitude)
	dfy <- as.numeric(df$latitude)

      try(bscresults.mapNorthAmericaPoints(
		dfx, dfy, par.cex=c, pngname=fn))
}

}
