# created by Rob Rankin
# example
# atlasNames <- c("16CU15","17PM50")
# t(sapply(atlasNames,  bscdata.atlas2latlong))


bscdata.atlas2latlong <- function(sqq){
   library(rgdal)
   require(rgdal)
   # sqq is a 6 digit character such as "16CU15" "17PM50"
   # function returns the Coordinates (Decimal degrees) at the south-west corner of the square

   east1 <- rbind("ABCDEFGH", "JKLMNPQR", "STUVWXYZ")
      block1 <- rbind("CEDFGHJKLM","NPQRSTUVWX")
      north1 <- "ABCDEFGHJKLMNPQRSTUVABCDE"

   zone <- as.numeric(substr(sqq,1,2))
   e1 <- (zone-1) %% 3
   ee <- substr(sqq,3,3)   nn <- substr(sqq,4,4)
   e3 <- as.numeric(substr(sqq,5,5))
   n3 <- as.numeric(substr(sqq,6,6))

   easting <- regexpr(ee,east1[e1+1])*100000
   northing <- 100000 * (regexpr(nn,north1) -1 -((zone-1) %% 2) *5)

   easting2 <- e3*10000
   efin <- easting + easting2

      # final digit is northing 10k block
   northing2 <- n3*10000
   nfin <- northing + northing2 + 4000000

   # test: bscdata.utm2AtlasSquare(zone, efin, nfin)
   # now convert UTM's to lat and long

   SP <- SpatialPoints(cbind(c(efin), c(nfin)), proj4string=CRS(paste("+proj=utm +zone=",as.character(zone),sep="")))
   SP2 <- spTransform(SP, CRS("+proj=longlat"))
   res <- attributes(SP2)$coords
   row.names(res) <- sqq
   return(res)
   }

