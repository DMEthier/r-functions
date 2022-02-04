# finds the closest weather stations with data to a given point

bscdata.findECWeatherStations <- function(lat=NA, lon=NA, prov=NA, startyr=NA, endyr=NA,
   type=NA, max=NA, maxdist=NA, stationID=NA, datasource="dendroica") {

 if (!is.na(lat) & !is.na(lon)) {
   dist <- paste("dbo.distAB(lat, lon, ", as.character(lat), ", ", as.character(lon), ")", sep="")
 } else {
   dist <- NA
 }

 a <- " WHERE "
 wh <- ""
 if (!is.na(prov)) {
    wh <- paste(wh, a, " prov = '", prov, "'", sep="")
    a <- " AND "
 }
 if (!is.na(startyr)) {
    wh <- paste(wh, a, startyr, " BETWEEN year_start and year_end", sep="")
    a <- " AND "
 }
 if (!is.na(endyr)) {
    # be careful with end yr, as this relies on outdated information (July 2009). More recent data may be available.
    wh <- paste(wh, a, endyr, " BETWEEN year_start and year_end", sep="")
    a <- " AND "
 }
 if (!is.na(type)) {
    # type can be one of hrl, dly or mly (hourly, daily or monthly)
    wh <- paste(wh, a, " type = '", type, "'", sep="")
    a <- " AND "
 }
 if (!is.na(maxdist) & !is.na(dist)) {
    wh <- paste(wh, a, dist, " <= ", maxdist, sep="")
    a <- " AND "
 }
 if (!is.na(stationID)) {
    wh <- paste(wh, a, " station_id = '", stationID, "'", sep="")
    a <- " AND "
 }
 # months not yet implemented. I don't think the data is currently available to allow this.

sql <- paste("select ", ifelse(is.na(max), "", paste(" top ", as.character(max), sep="")),
   " *, ", ifelse(is.na(dist),"NULL",dist), " as dist from vwWeatherECStations ", wh, ifelse(is.na(dist),"",paste(" order by ", dist, sep="")), sep="")

#print(sql)

 channel <- odbcConnect(datasource)
 table1 <- sqlQuery(channel, sql)
 odbcClose(channel)
 rm(channel)
 return(table1)


}

#bscdata.findECWeatherStations (42.6, -80.47, max=10, startyr=1960, endyr=2000, type="dly")

