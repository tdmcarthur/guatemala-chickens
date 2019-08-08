
# install.packages("dplyr")
# need new version of dplyr for group_map() and group_split()
# https://www.johnmackintosh.com/2019-02-28-first-look-at-mapping-and-splitting-in-dplyr/
library(earthEngineGrabR)
library(sp)
library(rgdal)
library(readstata13)
library(data.table)

# ee_grab() [data] list of ee_data_image() or ee_data_collection() functions which define the requested data. Multiple functions are passed inside a list, while a single function can be passed directly.

# for now, only one datasetID at a time
construct.ee_data_collection <- function(datasetID, spatialReducer = "mean", temporalReducer = "mean", 
         timeStart, timeEnd, specify.frequency = FALSE,
         temporal.frequency = c("day", "week", "month", "quarter", "year"),
         resolution = NULL,
         bandSelection = NULL) {
  
  if (specify.frequency) {
    
    date.endpoints <- as.character(seq.Date(as.Date(timeStart), as.Date(timeEnd), by = temporal.frequency))
    ee_data_collection.ls <- list()
    
    for (targ.date in seq_along(date.endpoints[-1])) {
      timeStart.first <- date.endpoints[targ.date]
      timeStart.last <- date.endpoints[targ.date + 1]
      ee_data_collection.ls[[length(ee_data_collection.ls) + 1]] <- 
        ee_data_collection(
        datasetID = datasetID,
        spatialReducer = spatialReducer, temporalReducer = temporalReducer,
        timeStart = timeStart.first, timeEnd = timeStart.last,
        resolution = resolution, bandSelection = bandSelection)
    }
  } else {
    timeStart.first <- substitute(a, list(a = timeStart))
    print(timeStart.first)
    timeStart.last <- timeEnd
    ee_data_collection.ls <- ee_data_collection(
        datasetID = datasetID,
        spatialReducer = spatialReducer, temporalReducer = temporalReducer,
        timeStart = timeStart.first, timeEnd = timeStart.last,
        resolution = resolution, bandSelection = bandSelection)
  }
  
  ee_data_collection.ls

}





########################################
########################################
### BELOW IS SCRATCH DEVELOPMENT CODE ##
########################################
########################################









construct.ee_data_collection(
  datasetID = "UCSB-CHG/CHIRPS/DAILY",
  spatialReducer = "mean", # temporalReducer = "mean",
  timeStart = "2017-06-01", timeEnd = "2017-07-01",
  resolution = NULL, bandSelection = NULL)
  

9:38

system.time(
test.multiple.days <- ee_grab(data = construct.ee_data_collection(
  datasetID = "UCSB-CHG/CHIRPS/DAILY",
  timeStart = "2017-06-01", timeEnd = "2017-06-06",
  specify.frequency = TRUE, temporal.frequency = "day",
  resolution = NULL, bandSelection = NULL),
  targetArea = "/Users/travismcarthur/Guate-HH-points.shp")
)

# , targetArea = "/Users/travismcarthur/Guate-HH-points.shp"




CHIRPS.guate.test.multiple <- ee_grab(data = list(ee_data_collection(
  datasetID = "UCSB-CHG/CHIRPS/DAILY",
  spatialReducer = "mean", # temporalReducer = "mean",
  timeStart = "2017-06-01", timeEnd = "2017-07-01",
  resolution = NULL, bandSelection = NULL),
  ee_data_collection(
  datasetID = "UCSB-CHG/CHIRPS/DAILY",
  spatialReducer = "mean", # temporalReducer = "mean",
  timeStart = "2017-07-01", timeEnd = "2017-08-01",
  resolution = NULL, bandSelection = NULL) ),
  targetArea = "/Users/travismcarthur/Guate-HH-points.shp")




  ee_data_collection(
  datasetID = "UCSB-CHG/CHIRPS/DAILY",
  spatialReducer = "mean", # temporalReducer = "mean",
  timeStart = "2017-07-01", timeEnd = "2017-08-01",
  resolution = NULL, bandSelection = NULL) ),
  ))

        timeStart = "2017-06-01", timeEnd = "2017-07-01",

         
          
         
          temporal.reolution, )




# for now, only one datasetID at a time
test.fn <- function(datasetID, spatialReducer = "mean", temporalReducer = "mean", 
         timeStart, timeEnd, specify.frequency = FALSE,
         temporal.frequency = c("day", "week", "month", "quarter", "year"),
         resolution = NULL,
         bandSelection = NULL,
         targetArea) {
  
  construct.ee_data_collection <- function(timeStart, timeEnd) {
    function(timeStart, timeEnd) {
      ee_data_collection(
        datasetID = datasetID,
        spatialReducer = spatialReducer, temporalReducer = temporalReducer,
        timeStart = timeStart, timeEnd = timeEnd,
        resolution = resolution, bandSelection = bandSelection)
    }
  }
  # a bit advanced -- creating a function within a function

  if (specify.frequency) {
    
    date.endpoints <- seq.Date(timeStart, timeEnd, by = temporal.frequency)
    ee_data_collection.ls <- list()
    
    for (targ.date in seq.along(date.endpoints[-1])) {
      timeStart.first <- date.endpoints[targ.date]
      timeStart.last <- date.endpoints[targ.date + 1]
      ee_data_collection[[length(ee_data_collection) + 1]] <- 
        construct.ee_data_collection(timeStart = timeStart.first, 
                                     timeEnd = timeStart.last)
    }
  } else {
    timeStart.first <- substitute(a, list(a = timeStart))
    print(timeStart.first)
    timeStart.last <- timeEnd
    ee_data_collection.ls <- construct.ee_data_collection(timeStart = timeStart.first, 
                                     timeEnd = timeStart.last)
    cat("byhiu \n")
  }
  
  ee_data_collection.ls

}





  
  