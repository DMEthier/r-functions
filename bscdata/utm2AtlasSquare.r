
# get atlas square numbers in NAD83 datum, from UTM coordinates
# you can use bscdata.coord2UTM to convert decimal coordinates to UTM first if required.
# converted from Java by Denis Lepage (10 Dec 2009)

bscdata.utm2AtlasSquare <- function(zone, easting, northing, northHem=TRUE, showBlock=FALSE) {

   if (min(zone) < 1 | max(zone) > 60) warning("Some zone values are out of range (1-60)")
   if (min(easting) < 100000 | max(easting) > 899999) warning("Some easting values are out of range")
   if (min(northing) < 0 | max(northing) > 9999999) warning("Some northing values are out of range")

   east1 <- rbind("ABCDEFGH", "JKLMNPQR", "STUVWXYZ")
   block1 <- rbind("CEDFGHJKLM","NPQRSTUVWX")
   north1 <- "ABCDEFGHJKLMNPQRSTUVABCDE"

   # calculate UTM block from northing. Blocks are exactly 8 degrees latitude, or
   # approximately 884431 meters, except the northernmost block.
   # this is only an approximation, and may not work well in the southern hemisphere

   b1 <- as.integer(northing / 884431)
   b1[b1 > 9] <- 9

   block <- substr(block1[ifelse(northHem, 2, 1)], b1+1, b1+1)

   # first 2 digits are for the zone
   square <- paste(ifelse(zone < 10, "0", ""), as.character(zone), sep="")

   # if utm block is requested for 7-digit square numbers
   if (showBlock) square <- paste(square, as.character(block), sep="")

   # next digit is for the easting block.
   # Repeating sequence: A-H, J-R, S-Z

   e1 <- (zone-1) %% 3
   e2 <- as.integer(easting / 100000) -1

   square <- paste(square, substr(east1[e1+1], e2+1, e2+1), sep="")
   #square <- paste(square, lapply(e2+1, function(x) return(substr(east1[e1+1], x, x))), sep="")

   # next digit is for the northing block.
   # About 5 repeating sequence per hemisphere: A-V
   # Uneven zones: start at A, Even zones: start at F

   n1 <- as.integer(northing) %% 2000000
   n2 <- as.integer(n1 / 100000)
   n2i <- n2 + ((zone-1) %% 2) *5

   # something is wrong here (but not above for east1, although it is apparently the same!). Using lapply instead
   # square <- paste(square, substr(north1, n2i+1, n2i+1), sep="")
   square <- paste(square, lapply(n2i+1, function(x) return(substr(north1, x, x))), sep="")

   # next digit is easting 10k block
   e3 <- as.integer(easting / 10000) %% 10

   # final digit is northing 10k block
   n3 <- as.integer(northing / 10000) %% 10

   square <- paste(square, e3, n3, sep="")

   return(square)

}

# bscdata.utm2AtlasSquare(c(17,18,17), c(412001,430001,530001), c(4730000,4720000,4730001))
