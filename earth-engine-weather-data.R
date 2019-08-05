


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
  timeStart = "2017-01-01", timeEnd = "2017-02-01",
  resolution = NULL, bandSelection = NULL),
  targetArea = system.file("data/territories.shp", package = "earthEngineGrabR"))
# NOTE: Maybe don't request a re-upload of territories.shp

str(CHIRPS.test.example.in.package)
# ?plot.sf
plot(CHIRPS.test.example.in.package, max.plot = 13)
plot(CHIRPS.test.example.in.package[, "precipitation_s.mean_t.mean_2017.01.01_to_2017.02.01", drop = FALSE])



CHIRPS.test.example.in.package.EDITED <- ee_grab(data = ee_data_collection(
  datasetID = "UCSB-CHG/CHIRPS/DAILY",
  spatialReducer = "mean", # temporalReducer = "mean",
  timeStart = "2017-01-01", timeEnd = "2017-02-01",
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
plot(CHIRPS.test.example.in.package.EDITED[, "precipitation_s.mean_t.mean_2017.01.01_to_2017.02.01", drop = FALSE])


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
proj4string(guate.chickens.gps.df.CSV) = CRS("+init=epsg:4326")
# I don't think that the projection matters for the purposes of pulling data
# from Google Earth Engine
# writeOGR(Mydata, ".","Mydata", "ESRI Shapefile")


library(readstata13)

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
  timeStart = "2017-01-01", timeEnd = "2017-02-01",
  resolution = NULL, bandSelection = NULL),
  targetArea = "/Users/travismcarthur/Guate-HH-points.shp")
# If you say "Y" to re-upload the shapefile then it has crashed every time.
# But once you say "Y" and it crashes, you can re-run it unmodified and it works.

colnames(CHIRPS.guate.test)[colnames(CHIRPS.guate.test) == 
  "precipitation_s.mean_t.mean_2017.01.01_to_2017.02.01"] <- 
  "precipitation_s.mean_t.mean_2017.01"

str(CHIRPS.guate.test)
# ?plot.sf
plot(CHIRPS.guate.test)
plot(CHIRPS.guate.test[, "precipitation_s.mean_t.mean_2017.01", drop = FALSE], axes = TRUE,
     main = "Jan 2017 mean daily precip in mm/day")





TerraClimate.guate.test <- ee_grab(data = ee_data_collection(
  datasetID = "IDAHO_EPSCOR/TERRACLIMATE",
  spatialReducer = "mean", # temporalReducer = "mean",
  timeStart = "2017-01-01", timeEnd = "2017-01-02",
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
                                    "tmmx_s.mean_t.mean_2017.01.01_to_2017.01.02"] <- "tmmx_s.mean_t.mean_2017.01"

TerraClimate.guate.test$tmmx_s.mean_t.mean_2017.01 <- TerraClimate.guate.test$tmmx_s.mean_t.mean_2017.01 * 0.1

str(TerraClimate.guate.test)
# ?plot.sf
plot(TerraClimate.guate.test)
plot(TerraClimate.guate.test[, "tmmx_s.mean_t.mean_2017.01", drop = FALSE], axes = TRUE,
     main = "Jan 2017 mean daily max temp in Celsius")




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






