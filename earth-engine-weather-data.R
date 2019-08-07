# NOTE: Encong of this file is UTF-8


# Rough guide to installation of dependencies for earthEngineGrabR on a Mac:
# 
# https://github.com/JesJehle/earthEngineGrabR
# 
# Check python version to make sure it is 2.7 (but I guess Python 3 would work too? ):
# In terminal:
# python --version
# 
# Then install via Anacondavia GUI installer here (for Python 2.7):
# https://www.anaconda.com/distribution/
# Chose "install for me only" in the install wizard
# 
# So:
# Terminal:
# First, it threw an error and I may need to do:
# xcode-select --install
# Then : 
# brew install pkg-config
# (may need : brew link --overwrite pkg-config ; and then again : brew install pkg-config )
# Ok now try this since the above didnt work either :
# https://stackoverflow.com/a/47228630
# which links to https://github.com/Homebrew/brew/issues/3228
# So :
# sudo chown -R $(whoami) $(brew --prefix)/*
# [The above takes a bit of time to complete)
# Then:
# brew install gdal
# Then do it again since it throws an error partway through:
# brew link --overwrite pkg-config
# sudo chown -R $(whoami) $(brew --prefix)/*
# Then again finally:
# brew install gdal
# 
# 
# Then in R:
# install.packages("sf")
# I chose _NOT_ to install from source
# 
# 
# Finally, install earthEngineGrabR
# library(devtools)
# install_github("JesJehle/earthEngineGrabR")
# library(earthEngineGrabR)
# 
# And must set API credentials (only need to do this once):
# ee_grab_install()
# Then will go through some Google account authentication steps in the browser




# install.packages("sf")
# Choose to install from binary

# library(devtools)
# install_github("JesJehle/earthEngineGrabR")

library(earthEngineGrabR)

# Set API credentials:
# ee_grab_install()


# TEST:
srtm_data <- ee_grab(data = ee_data_image(datasetID = "CGIAR/SRTM90_V4", 
                                          spatialReducer = "mean", 
                                          resolution = 100, 
                                          bandSelection = "elevation"
                                          ),
                    targetArea = system.file("data/territories.shp", package = "earthEngineGrabR")
                    )

str(srtm_data)
plot(srtm_data)

require(rgdal)
shape.test <- readOGR(dsn = system.file("data/territories.shp", package = "earthEngineGrabR"))
plot(shape.test )
axis(1)
axis(2)

# var dataset = ee.ImageCollection('UCSB-CHG/CHIRPS/DAILY')
#                   .filter(ee.Filter.date('2018-05-01', '2018-05-03'));
# var precipitation = dataset.select('precipitation');
# var precipitationVis = {
#   min: 1.0,
#   max: 17.0,
#   palette: ['001137', '0aab1e', 'e7eb05', 'ff4a2d', 'e90000'],
# };
# Map.setCenter(17.93, 7.71, 2);
# Map.addLayer(precipitation, precipitationVis, 'Precipitation');


CHIRPS.test.example.in.package <- ee_grab(data = ee_data_collection(
  datasetID = "UCSB-CHG/CHIRPS/DAILY",
  spatialReducer = "mean", temporalReducer = "mean",
  timeStart = "2017-06-01", timeEnd = "2017-07-01",
  resolution = NULL, bandSelection = NULL),
  targetArea = system.file("data/territories.shp", package = "earthEngineGrabR"))
# NOTE: Maybe don't request a re-upload of territories.shp

str(CHIRPS.test.example.in.package)
# ?plot.sf
plot(CHIRPS.test.example.in.package, max.plot = 13)
plot(CHIRPS.test.example.in.package[, "precipitation_s.mean_t.mean_2017.06.01_to_2017.07.01", drop = FALSE])



CHIRPS.test.example.in.package.EDITED <- ee_grab(data = ee_data_collection(
  datasetID = "UCSB-CHG/CHIRPS/DAILY",
  spatialReducer = "mean", # temporalReducer = "mean",
  timeStart = "2017-06-01", timeEnd = "2017-07-01",
  resolution = NULL, bandSelection = NULL),
  targetArea = system.file("data/territories.shp", package = "earthEngineGrabR"))
# Ok so, no matter what, spatialReducer and temporalReducer are in effect (can be
# mean, median, min, max, or mode), so
# cannot get multiple values at various dates (or various pixels
# within a given polygon) with the same request
# NOTE: Maybe don't request a re-upload of territories.shp

str(CHIRPS.test.example.in.package.EDITED)
# ?plot.sf
plot(CHIRPS.test.example.in.package.EDITED, max.plot = 13)
plot(CHIRPS.test.example.in.package.EDITED[, "precipitation_s.mean_t.mean_2017.06.01_to_2017.07.01", drop = FALSE])


guate.chickens.gps.df.CSV <- read.csv("/Users/travismcarthur/Google Drive/Guatemala ronda 2 2017/stata files/edited data/guatemala fomin ronda 2.csv", stringsAsFactors = FALSE)

str(guate.chickens.gps.df.CSV[, grepl("GPS", names(guate.chickens.gps.df.CSV))])


# https://gis.stackexchange.com/questions/206903/convert-csv-file-to-shapefile-in-r
library(sp)
library(rgdal)
# Mydata <- read.csv("londonburglary201402.csv", header = T, sep = ",")
# coordinates(Mydata) <- ~Longitude + Latitude
# proj4string(Mydata) = CRS("+init=epsg:4326")
# writeOGR(Mydata, ".","Mydata", "ESRI Shapefile")

guate.chickens.gps.df.CSV <- guate.chickens.gps.df.CSV[, c("SbjNum", "GPS.Latitude", "GPS.Longitude")]
colnames(guate.chickens.gps.df.CSV[, 2:3]) <- c("Latitude", "Longitude")
guate.chickens.gps.df.CSV <- guate.chickens.gps.df.CSV[complete.cases(guate.chickens.gps.df.CSV), ]

coordinates(guate.chickens.gps.df.CSV) <- ~Longitude + Latitude
# proj4string(guate.chickens.gps.df.CSV) = CRS("+init=epsg:4326")
# I don't think that the projection matters for the purposes of pulling data
# from Google Earth Engine
# writeOGR(Mydata, ".","Mydata", "ESRI Shapefile")


library(readstata13)
library(data.table)

guate.chickens.gps.df.DTA <- read.dta13("/Users/travismcarthur/Google Drive/Guatemala ronda 2 2017/stata files/edited data/stata data files/hhroster.dta")

guate.chickens.gps.df.DTA <- guate.chickens.gps.df.DTA[, c("sbjnum", "gpslatitude", "gpslongitude")]
colnames(guate.chickens.gps.df.DTA)[2:3] <- c("Latitude", "Longitude")
guate.chickens.gps.df.DTA <- guate.chickens.gps.df.DTA[complete.cases(guate.chickens.gps.df.DTA), ]
guate.chickens.gps.df.DTA <- unique(guate.chickens.gps.df.DTA)

coordinates(guate.chickens.gps.df.DTA) <- ~Longitude + Latitude
# proj4string(guate.chickens.gps.df.DTA) = CRS("+init=epsg:4326")
# I don't think that the projection matters for the purposes of pulling data
# from Google Earth Engine. Actually, I think I can skip assignming

writeOGR(guate.chickens.gps.df.DTA, ".","Guate-HH-points", "ESRI Shapefile",  overwrite_layer = TRUE)



CHIRPS.guate.test <- ee_grab(data = ee_data_collection(
  datasetID = "UCSB-CHG/CHIRPS/DAILY",
  spatialReducer = "mean", # temporalReducer = "mean",
  timeStart = "2017-06-01", timeEnd = "2017-07-01",
  resolution = NULL, bandSelection = NULL),
  targetArea = "/Users/travismcarthur/Guate-HH-points.shp")
# If you say "Y" to re-upload the shapefile then it has crashed every time.
# But once you say "Y" and it crashes, you can re-run it unmodified and it works.

colnames(CHIRPS.guate.test)[colnames(CHIRPS.guate.test) == 
  "precipitation_s.mean_t.mean_2017.06.01_to_2017.07.01"] <- 
  "precipitation_s.mean_t.mean_2017.06"

str(CHIRPS.guate.test)
uniqueN(CHIRPS.guate.test$precipitation_s.mean_t.mean_2017.06)
# [1] 15
sd(CHIRPS.guate.test$precipitation_s.mean_t.mean_2017.06)
# 0.5265971
sd(CHIRPS.guate.test$precipitation_s.mean_t.mean_2017.06) / mean(CHIRPS.guate.test$precipitation_s.mean_t.mean_2017.06)
# Coef of variation : [1] 0.04196773
# ?plot.sf
plot(CHIRPS.guate.test)
plot(CHIRPS.guate.test[, "precipitation_s.mean_t.mean_2017.06", drop = FALSE], axes = TRUE,
     main = "June 2017 mean daily precip in mm/day (CHIRPS)")


guate.bbox <- guate.chickens.gps.df.DTA@bbox
grid.resolution.test.df <- expand.grid(Longitude = seq(guate.bbox[1, 1], guate.bbox[1, 2], length.out = 20),
                                       Latitude = seq(guate.bbox[2, 1], guate.bbox[2, 2], length.out = 20))
plot(grid.resolution.test.df[, 1:2])

grid.resolution.test.df$id <- 1:nrow(grid.resolution.test.df)

coordinates(grid.resolution.test.df) <- ~Longitude + Latitude
writeOGR(grid.resolution.test.df, ".","Guate-resolution-test-points", "ESRI Shapefile",  overwrite_layer = TRUE)


CHIRPS.guate.resolution.test <- ee_grab(data = ee_data_collection(
  datasetID = "UCSB-CHG/CHIRPS/DAILY",
  spatialReducer = "mean", # temporalReducer = "mean",
  timeStart = "2017-06-01", timeEnd = "2017-07-01",
  resolution = NULL, bandSelection = NULL),
  targetArea = "/Users/travismcarthur/Guate-resolution-test-points.shp")

plot(CHIRPS.guate.resolution.test[, 2], axes = TRUE, pch = 15, cex = 1.9)
plot(CHIRPS.guate.test[, "precipitation_s.mean_t.mean_2017.06", drop = FALSE], axes = TRUE)


TerraClimate.guate.test <- ee_grab(data = ee_data_collection(
  datasetID = "IDAHO_EPSCOR/TERRACLIMATE",
  spatialReducer = "mean", # temporalReducer = "mean",
  timeStart = "2017-06-01", timeEnd = "2017-07-01",
  resolution = NULL, bandSelection = NULL),
  targetArea = "/Users/travismcarthur/Guate-HH-points.shp")
# If you say "Y" to re-upload the shapefile then it has crashed every time.
# But once you say "Y" and it crashes, you can re-run it unmodified and it works.

# NOTE:
# Error: timeStart and timeEnd have the identical date: 2017-01-01
# To select only the single day 2017-01-01 use the date range of timeStart: 2017-01-01 and timeEnd: 2017-01-02.
# The date selection is inclusive for the dateStart date and exclusive for the timeEnd date. 
#   Therefore, to select a single day use the date of the day as time start and the day after as timeEnd date.

colnames(TerraClimate.guate.test)[colnames(TerraClimate.guate.test) == 
                                    "tmmx_s.mean_t.mean_2017.06.01_to_2017.07.01"] <- "tmmx_s.mean_t.mean_2017.06"

TerraClimate.guate.test$tmmx_s.mean_t.mean_2017.06 <- TerraClimate.guate.test$tmmx_s.mean_t.mean_2017.01 * 0.1

str(TerraClimate.guate.test)
uniqueN(TerraClimate.guate.test$tmmx_s.mean_t.mean_2017.06)# [1] 11
sd(TerraClimate.guate.test$tmmx_s.mean_t.mean_2017.06)
# [1] 0.1119042
# ?plot.sf
plot(TerraClimate.guate.test)
plot(TerraClimate.guate.test[, "tmmx_s.mean_t.mean_2017.06", drop = FALSE], axes = TRUE,
     main = "June 2017 mean daily max temp in Celsius (TerraClimate)")


TerraClimate.guate.resolution.test <- ee_grab(data = ee_data_collection(
  datasetID = "IDAHO_EPSCOR/TERRACLIMATE",
  spatialReducer = "mean", # temporalReducer = "mean",
  timeStart = "2017-06-01", timeEnd = "2017-07-01",
  resolution = NULL, bandSelection = NULL),
  targetArea = "/Users/travismcarthur/Guate-resolution-test-points.shp")

TerraClimate.guate.resolution.test$tmmx_s.mean_t.mean_2017.06.01_to_2017.07.01 <-
  TerraClimate.guate.resolution.test$tmmx_s.mean_t.mean_2017.06.01_to_2017.07.01 * 0.1

plot(TerraClimate.guate.resolution.test[, "tmmx_s.mean_t.mean_2017.06.01_to_2017.07.01", drop = FALSE], 
     axes = TRUE, pch = 15, cex = 1.9)
plot(TerraClimate.guate.test[, "tmmx_s.mean_t.mean_2017.06", drop = FALSE], axes = TRUE)







MODIS.temp.guate.test <- ee_grab(data = ee_data_collection(
  datasetID = "MODIS/006/MOD11A1",
  spatialReducer = "mean", # temporalReducer = "mean",
  timeStart = "2017-06-01", timeEnd = "2017-07-01",
  resolution = NULL, bandSelection = NULL),
  targetArea = "/Users/travismcarthur/Guate-HH-points.shp")

#colnames(MODIS.temp.guate.test)[colnames(MODIS.temp.guate.test) == 
#                                    "tmmx_s.mean_t.mean_2017.06.01_to_2017.07.01"] <- "tmmx_s.mean_t.mean_2017.06"

#MODIS.temp.guate.test$tmmx_s.mean_t.mean_2017.06 <- MODIS.temp.guate.test$tmmx_s.mean_t.mean_2017.01 * 0.1

MODIS.temp.guate.test$LST_Day_1km_s.mean_t.mean_2017.06.01_to_2017.07.01 <-
  MODIS.temp.guate.test$LST_Day_1km_s.mean_t.mean_2017.06.01_to_2017.07.01 * 0.02 

# convert from Kelvin to Celsius
MODIS.temp.guate.test$LST_Day_1km_s.mean_t.mean_2017.06.01_to_2017.07.01 <- 
  MODIS.temp.guate.test$LST_Day_1km_s.mean_t.mean_2017.06.01_to_2017.07.01 - 273.15

str(MODIS.temp.guate.test)
# QC_Day
uniqueN(MODIS.temp.guate.test$LST_Day_1km_s.mean_t.mean_2017.06.01_to_2017.07.01)
# 91 unique values
sd(MODIS.temp.guate.test$LST_Day_1km_s.mean_t.mean_2017.06.01_to_2017.07.01)
# [1] 1.671745
# ?plot.sf
plot(MODIS.temp.guate.test)
plot(MODIS.temp.guate.test[, "LST_Day_1km_s.mean_t.mean_2017.06.01_to_2017.07.01", drop = FALSE], axes = TRUE,
     main = "June 2017 mean daily surface temp in Celsius (MODIS)")






GPP.guate.test <- ee_grab(data = ee_data_collection(
  datasetID = "MODIS/006/MOD17A2H",
  spatialReducer = "mean", # temporalReducer = "mean",
  timeStart = "2017-06-01", timeEnd = "2017-07-01",
  resolution = NULL, bandSelection = NULL),
  targetArea = "/Users/travismcarthur/Guate-HH-points.shp")


str(GPP.guate.test)
# QC_Day
uniqueN(GPP.guate.test$Gpp_s.mean_t.mean_2017.06.01_to_2017.07.01)
# 170 unique values
sd(GPP.guate.test$Gpp_s.mean_t.mean_2017.06.01_to_2017.07.01)
# [1] 57.68621
sd(GPP.guate.test$Gpp_s.mean_t.mean_2017.06.01_to_2017.07.01) / mean(GPP.guate.test$Gpp_s.mean_t.mean_2017.06.01_to_2017.07.01)
# [1] 0.1747028  coefficient of variation
# ?plot.sf
plot(GPP.guate.test)
plot(GPP.guate.test[, "Gpp_s.mean_t.mean_2017.06.01_to_2017.07.01", drop = FALSE], axes = TRUE,
     main = "June 2017 mean Gross Primary Productvity (MODIS)")

















# system.file("data/territories.shp", package = "earthEngineGrabR")
# 2013 - 2017



CHIRPS.guate.test.2 <- ee_grab(data = ee_data_collection(
  datasetID = "UCSB-CHG/CHIRPS/DAILY",
  spatialReducer = "mean", # temporalReducer = "mean",
  timeStart = "2013-01-01", timeEnd = "2017-12-31",
  resolution = NULL, bandSelection = NULL),
  targetArea = "/Users/travismcarthur/Guate-HH-points.shp")
# If you say "Y" to re-upload the shapefile then it has crashed every time.
# But once you say "Y" and it crashes, you can re-run it unmodified and it works.


str(CHIRPS.guate.test.2)
# ?plot.sf
plot(CHIRPS.guate.test.2)
plot(CHIRPS.guate.test.2[, "precipitation_s.mean_t.mean_2013.01.01_to_2017.12.31", drop = FALSE], axes = TRUE)









weather.station.data.dir <- "/Users/travismcarthur/Google Drive/Guatemala ronda 2 2017/Información MCC-CLIMA-PINPEP/Información Clima/Datos de Estaciones Climáticas/Datos de Estaciones Climáticas"

weather.station.names <- list.dirs(weather.station.data.dir, full.names = FALSE)
weather.station.names <- weather.station.names[weather.station.names != ""]



meses.excel <- c("ENE", "FEB", "MAR", "ABR", "MAY", "JUN", "JUL", "AGO", "SEP", "OCT", "NOV", "DIC")

weather.combined.ls <- list()

for (targ.station in weather.station.names) {
  
  weather.station.files <- list.files(paste0(weather.station.data.dir, "/", targ.station))
  weather.station.files <- weather.station.files[grepl("xlsx", weather.station.files)]
  weather.station.files <- gsub("[.]xlsx", "", weather.station.files)
  
  for (targ.year in weather.station.files) {
    
    for (targ.month in meses.excel) {
      
      collate.data.temp.df <- tryCatch(
        openxlsx::read.xlsx(paste0(weather.station.data.dir, "/", targ.station, "/", targ.year, ".xlsx"),
                            startRow = 1, sheet = targ.month), error = function(e) "threw error" )
      
      if (length(collate.data.temp.df) == 1) {
        next
      }
      collate.data.temp.df <- collate.data.temp.df[-1, ]
      
      stopifnot( all(colnames(collate.data.temp.df) == c("DÍA", "TEMPERATURA", "X3",                                                          "PRECIPITACIÓN", "VELOCIDAD.DEL.VIENTO", "X6") ) )
      
      colnames(collate.data.temp.df) <- c("day", "tempmax", "tempmin", "precip", "windmean", "windmax")
      
      collate.data.temp.df$station.name <- gsub("[0-9]*[.] ", "", targ.station)
      collate.data.temp.df$year <- as.numeric(targ.year)
      collate.data.temp.df$month <- as.numeric(which(targ.month == meses.excel)) # Want as float, not integer
      # Bit of a hack above
      
      weather.combined.ls[[length(weather.combined.ls) + 1]] <- collate.data.temp.df
    }
    
  }
   # stop()
}

# options(warn=2)
# options(warn=0)

weather.combined.dt <- rbindlist(weather.combined.ls)
# NOTEL this is a data.table, not a data.frame

weather.combined.dt <- weather.combined.dt[ day != "TEMP MAX", ]
# Seem there were a few manual formula calculations in the spreadsheets

weather.combined.dt[, day := as.numeric(day)]
weather.combined.dt[, tempmax := as.numeric(tempmax)]
weather.combined.dt[, tempmin := as.numeric(tempmin)]
weather.combined.dt[, windmean := as.numeric(windmean )]
# One observation has windmean as "1.832.9", so this "convert to N"A warning is OK
weather.combined.dt[, windmax := as.numeric(windmax)]

weather.combined.dt

maxtemp.monthly.mean.dt <- weather.combined.dt[, .(tempmax = mean(tempmax)), by = .(station.name, month, year)]


# Cannot use openxlsx::read.xlsx since xls, not xlsx
# install.packages("readxl")
# library(xlsx)

station.coords.dt <- as.data.table(readxl::read_excel("/Users/travismcarthur/Google Drive/Guatemala ronda 2 2017/Mapa de Ubicación de Estaciones Climáticas/Capas/coordenadas.xls"))

sort(station.coords.dt$STATION)
sort(unique(weather.combined.dt$station.name))
station.coords.dt[STATION == "Mancomunidad", STATION := "Mancomunidad Copanchorti"] 
station.coords.dt[STATION == "Oquen", STATION := "Lomas Oquen"] 
stopifnot(identical(sort(station.coords.dt$STATION), sort(unique(weather.combined.dt$station.name)) ))

colnames(station.coords.dt)[2:3] <- c("Longitude", "Latitude")
coordinates(station.coords.dt) <- ~Longitude + Latitude

writeOGR(station.coords.dt, ".","Guate-station-points", "ESRI Shapefile",  overwrite_layer = TRUE)



MODIS.temp.guate.test.2 <- ee_grab(data = ee_data_collection(
  datasetID = "MODIS/006/MOD11A1",
  spatialReducer = "mean", # temporalReducer = "mean",
  timeStart = "2017-06-01", timeEnd = "2017-07-01",
  resolution = NULL, bandSelection = NULL),
  targetArea = "/Users/travismcarthur/Guate-station-points.shp")
N
# the "N" answers the prompt: Should the file be deleted and uploaded again? [Y/N]: Y

plot(MODIS.temp.guate.test.2[, "LST_Day_1km_s.mean_t.mean_2017.06.01_to_2017.07.01", drop = FALSE], axes = TRUE)
# weird limits ??

maxtemp.monthly.mean.dt

MODIS.temp.guate.test.2.subset <- as.data.table(MODIS.temp.guate.test.2[, c("STATION", "LST_Day_1km_s.mean_t.mean_2017.06.01_to_2017.07.01"), drop = FALSE])

colnames(MODIS.temp.guate.test.2.subset)[1:2] <- c("station.name", "modis.surface.temp")

MODIS.temp.guate.test.2.subset[, modis.surface.temp := modis.surface.temp * 0.02 - 273.15 ]
test.3 <- merge(maxtemp.monthly.mean.dt[year == 2017 & month == 7, ], MODIS.temp.guate.test.2.subset)

cor(test.3[, .(modis.surface.temp, tempmax)])




