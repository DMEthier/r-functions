# reads a single CSV file from Environment Canada servers

readECWeatherSingle <- function(station, year, month, day=0, save=F, dir="T:/cmmn/climatedata/", type="dly") {

host <- "www.climate.weatheroffice.ec.gc.ca"
path <- paste("/climateData/bulkdata_e.html?timeframe=1&Prov=XX&StationID=", station, "&Year=", year,
        "&Month=", month, "&Day=", day, "&format=csv&type=", type, sep="")
myurl <- paste("http://", host, path, sep="")
con <- url(myurl)
csvfile <- readLines(con)
close(con)
dir <- paste(dir, "station_", station, sep="")

if (save) {
  dir.create(dir, showWarnings=F, recursive=T)
  filename <- paste(dir, "/bulkdata_", station, "-", year, "-", month, "-", day, "-", type, ".csv", sep="")
} else {
  filename <- ""
}

f <- file(filename, open="w+")
writeLines(csvfile, f)
if (type == "hly") skiplines <- 16
else if (type == "dly") skiplines <- 24
else if (type == "mly") skiplines <- 19

df <- read.csv(f, skip=skiplines, header=FALSE)

# be aware that the list of variables names COULD change if EC updates their standards. In which case, the following
# section will likely fail based on the number of variables expected

if (type == "hly") {
names(df) <- c("DateTime","Year","Month","Day","Time","Temp","TempFlag","DewPointTemp","DewPointTempFlag","RelHum","RelHumFlag",
   "WindDir","WindDirFlag","WindSpd","WindSpdFlag","Visibility","VisibilityFlag","StnPress","StnPressFlag","Hmdx",
   "HmdxFlag","WindChill","WindChillFlag","Weather")
} else if (type == "dly") {
names(df) <- c("DateTime","Year","Month","Day","DataQuality","MaxTemp","MaxTempFlag","MinTemp","MinTempFlag","MeanTemp",
  "MeanTempFlag","HeatDegDays","HeatDegDaysFlag","CoolDegDays","CoolDegDaysFlag","TotalRain","TotalRainFlag",
  "TotalSnow","TotalSnowFlag","TotalPrecip","TotalPrecipFlag","SnowonGrnd","SnowonGrndFlag","DirofMaxGust",
  "DirofMaxGustFlag","SpdofMaxGust","SpdofMaxGustFlag")
} else if (type == "mly") {
names(df) <- c("DateTime","Year","Month","MeanMaxTemp","MeanMaxTempFlag","MeanTemp","MeanTempFlag","MeanMinTemp",
  "MeanMinTempFlag","ExtrMaxTemp","ExtrMaxTempFlag","ExtrMinTemp","ExtrMinTempFlag","TotalRain","TotalRainFlag",
  "TotalSnow","TotalSnowFlag","TotalPrecip","TotalPrecipFlag","SnowGrndLastDay","SnowGrndLastDayFlag","DirofMaxGust",
  "DirofMaxGustFlag","SpdofMaxGust","SpdofMaxGustFlag")

}

close(f)
return(df)
}
