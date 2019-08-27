
# 2 bugs to fix in earthEngineGrabR:
# 1. Chokes with new version of googledrive package
# 2. Chokes when the ee_data_collection function returns a list of length 1

# NOTE: I edited earthEngineGrabR::upload() to not ask for a prompt about uploading the stuffs
# Fixed another bug too in auth()
# and then re-installed the modified version.
# See
# https://stackoverflow.com/questions/37934694/how-to-re-install-a-modified-r-package
# https://stackoverflow.com/questions/1474081/how-do-i-install-an-r-package-from-source
# https://superuser.com/questions/46512/create-a-tar-file-for-compressing-files-and-directories-on-mac-os-x
# (BTW use cd with tar on terminal, not absolute paths)
# Note: I have to re-run ee_grab_install() after re-building from source

library(earthEngineGrabR)

# NOTE: Encoding of this file is UTF-8

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
# NOTE: must set API credentials (only need to do this once):
# ee_grab_install()
# Then will go through some Google account authentication steps in the browser




# install.packages("sf")
# Choose to install from binary

# library(devtools)
# install_github("JesJehle/earthEngineGrabR")

library(earthEngineGrabR)
library(sp)
library(rgdal)
library(readstata13)
library(data.table)
library(lfe)
library(ggplot2)
require(quantmod)
require(reshape2)
require(plyr)
require(scales)

source("~/git/guatemala-chickens/earth-engine-util-functions.R")


# Set API credentials. Just need to do this once:
# ee_grab_install()

guate.chickens.gps.df.DTA <- read.dta13("~/Google Drive/Guatemala ronda 2 2017/stata files/edited data/stata data files/hhroster.dta")

guate.chickens.gps.df.DTA <- guate.chickens.gps.df.DTA[, c("sbjnum", "gpslatitude", "gpslongitude")]
colnames(guate.chickens.gps.df.DTA)[2:3] <- c("Latitude", "Longitude")
guate.chickens.gps.df.DTA <- guate.chickens.gps.df.DTA[complete.cases(guate.chickens.gps.df.DTA), ]
guate.chickens.gps.df.DTA <- unique(guate.chickens.gps.df.DTA)

guate.chickens.gps.df.DTA.for.merger <- guate.chickens.gps.df.DTA

coordinates(guate.chickens.gps.df.DTA) <- ~Longitude + Latitude
# proj4string(guate.chickens.gps.df.DTA) = CRS("+init=epsg:4326")
# I don't think that the projection matters for the purposes of pulling data
# from Google Earth Engine. Actually, I think I can skip assigning

writeOGR(guate.chickens.gps.df.DTA, ".","Guate-HH-points", "ESRI Shapefile",  overwrite_layer = TRUE)




weather.station.data.dir <- "~/Google Drive/Guatemala ronda 2 2017/Información MCC-CLIMA-PINPEP/Información Clima/Datos de Estaciones Climáticas/Datos de Estaciones Climáticas"

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

# TODO: What is going on with these warnings? If I set warn = 2, it doesnt show warnings
# so maybe it's embedded within a tryCatch() within the code itself
# ALSO! Could be beacsue I've updated R and all packages to latest version
# In unzip(xlsxFile, exdir = xmlDir) : error 1 in extracting from zip file

weather.combined.dt <- rbindlist(weather.combined.ls)
# NOTE: this is a data.table, not a data.frame

uniqueN(weather.combined.dt[, .(station.name, year)])
# This matches the number of files in the folders,
# so the warning above is probably not concerning


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
precip.monthly.mean.dt <- weather.combined.dt[, .(precip = mean(precip)), by = .(station.name, month, year)]


# Cannot use openxlsx::read.xlsx since xls, not xlsx
# install.packages("readxl")
# library(xlsx)

station.coords.dt <- as.data.table(readxl::read_excel("~/Google Drive/Guatemala ronda 2 2017/Mapa de Ubicación de Estaciones Climáticas/Capas/coordenadas.xls"))

sort(station.coords.dt$STATION)
sort(unique(weather.combined.dt$station.name))
station.coords.dt[STATION == "Mancomunidad", STATION := "Mancomunidad Copanchorti"] 
station.coords.dt[STATION == "Oquen", STATION := "Lomas Oquen"] 
stopifnot(identical(sort(station.coords.dt$STATION), sort(unique(weather.combined.dt$station.name)) ))

colnames(station.coords.dt)[2:3] <- c("Longitude", "Latitude")
station.coords.dt.for.merger <- station.coords.dt

coordinates(station.coords.dt) <- ~Longitude + Latitude

writeOGR(station.coords.dt, ".","Guate-station-points", "ESRI Shapefile",  overwrite_layer = TRUE)

station.coords.dt.for.merger$type <- "station"

guate.chickens.gps.df.DTA.for.merger$type <- "hh"

stations.hhs.combined.df <- rbind.fill(station.coords.dt.for.merger, guate.chickens.gps.df.DTA.for.merger)


coordinates(stations.hhs.combined.df) <- ~Longitude + Latitude

writeOGR(stations.hhs.combined.df, ".","Guate-stations-and-hhs-combined", "ESRI Shapefile",  overwrite_layer = TRUE)








surface.temperature.ls <- list()


year.sequence <- seq.Date(as.Date("2015-01-01"), as.Date("2018-01-01"), by = "year")
# 2013 to 2017

for (targ.interval in seq_along(year.sequence[-1])) {
  # targ.interval <- 1
  
  surface.temperature.df <- 
    ee_grab(data = construct.ee_data_collection(
      datasetID = "MODIS/006/MOD11A1",
     # timeStart = "2013-01-01", timeEnd = "2014-01-01",
      timeStart = year.sequence[targ.interval], 
      timeEnd = year.sequence[targ.interval + 1],
      specify.frequency = TRUE, temporal.frequency = "month",
      resolution = NULL, bandSelection = "LST_Day_1km"),
      targetArea = "~/Guate-station-points.shp",
      testCase = "N") 
  # If you say "Y" to re-upload the shapefile then it has crashed every time.
  # But once you say "Y" and it crashes, you can re-run it unmodified and it works.
  # N
  # To answer the "interactive" prompt
  # Ok it doesn't work with a loop

  colnames(surface.temperature.df) <- gsub(".*t[.]mean_", "", colnames(surface.temperature.df))
  colnames(surface.temperature.df) <- gsub("_to_.*$", "", colnames(surface.temperature.df))
  surface.temperature.df <- melt(surface.temperature.df, id.vars = c("id", "STATION", "geometry"))
  surface.temperature.df$variable <- as.Date(gsub("[.]", "-", as.character(surface.temperature.df$variable)))
  colnames(surface.temperature.df)[colnames(surface.temperature.df) == "variable"] <- "date"
  colnames(surface.temperature.df)[colnames(surface.temperature.df) == "value"] <- "MODIS.temperature"
  surface.temperature.df$day <- formatC(as.numeric(data.table::mday(surface.temperature.df$date)), 
                                        width = 2, flag = "0")
  surface.temperature.df$month <- formatC(as.numeric(data.table::month(surface.temperature.df$date)), 
                                          width = 2, flag = "0")
  surface.temperature.df$year <- formatC(as.numeric(data.table::year(surface.temperature.df$date)), 
                                         width = 4, flag = "0")
  surface.temperature.df$STATION <- as.character(surface.temperature.df$STATION)
  surface.temperature.df$id <- as.character(surface.temperature.df$id)
  stop()
  surface.temperature.ls[[length(surface.temperature.ls) + 1]] <- surface.temperature.df
  
}



CHIRPS.guate.test.again <- ee_grab(data = ee_data_collection(
  datasetID = "UCSB-CHG/CHIRPS/DAILY",
  spatialReducer = "mean", # temporalReducer = "mean",
  timeStart = "2017-06-01", timeEnd = "2017-07-01",
  resolution = NULL, bandSelection = NULL),
  targetArea = "~/Guate-station-points.shp")
# If you say "Y" to re-upload the shapefile then it has crashed every time.
# But once you say "Y" and it crashes, you can re-run it unmodified and it works.



surface.temperature.combined.dt <- rbindlist(surface.temperature.ls)

surface.temperature.combined.dt$day <- NULL
# maxtemp.monthly.mean.dt$day <- NULL
maxtemp.monthly.mean.dt$month <- formatC(maxtemp.monthly.mean.dt$month, 
                                          width = 2, flag = "0")
maxtemp.monthly.mean.dt$year <- formatC(maxtemp.monthly.mean.dt$year, 
                                         width = 4, flag = "0")

setnames(surface.temperature.combined.dt, "STATION", "station.name")

surface.temperature.combined.dt[, MODIS.temperature := MODIS.temperature * 0.02 - 273.15 ]
surface.temperature.validation.df <- merge(maxtemp.monthly.mean.dt, surface.temperature.combined.dt)

# save.image("~/Desktop/Collaborations/Mullally/guatemala-chickens/GIS/temperature-validation-temporary.Rdata")
# load("~/Desktop/Collaborations/Mullally/guatemala-chickens/GIS/temperature-validation-temporary.Rdata")





