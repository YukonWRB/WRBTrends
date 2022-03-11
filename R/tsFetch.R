#This script takes the data.table output  from the stationMeta script (truncated if you desire fewer stations) and fetches time-series information for the stations contained in the "Site name", "Site identifier", and "Data location" columns.
#
#If you are fetching information from the EQWin or Snow Survey Access databases you MUST have access to the X drive on your machine. Aquarius logon can be from anywhere.

#' Title
#'
#' @param TSlist The data.table output of WRBTrends::stationMeta, containing at minimum the default columns specified by that function.
#' @param sources The list of sources you wish to download data for. "all" means all the sources listed in the "Data location" column of the input data.table. Can also specify from "Aquarius", "EQWin", "Snow Survey", "ECCC", or "Workbook" if you want to exclude any sources. If specifying "Workbook" all time-series must be in individual workbook tabs with columns "datetime" and "value".
#' @param AQlogin The login parameters for Aquarius in format c("username", "password"). Leave NULL if you are not fetching from Aquarius, in which case you should either ensure it is not specified in the input data.table or is excluded under the source parameter.
#' @param HYlogin The login parameters for HYDAT in format c("username", "password"). Leave NULL if you are not fetching from Aquarius, in which case you should either ensure it is not specified in the input data.table or is excluded under the source parameter.
#'
#' @return A list containing one element (tibble) per time-series, with processing performed to standardize the time-series to one common format. Designed to act as input to the WRBTrends:: ???? and ???? functions.
#' 
#' @export
#'
#' @examples
#' 


tsFetch <- function(TSlist, sources="all", AQlogin=c("gtdelapl","WQ*2021!"), HYlogin=NULL){
  
  #Find all the data sources
  if (sources=="all"){
    sources <- unique(TS$`Data location`)
  }
  
  #Split into a list by source
  TSlist <- split(TS, by="Data location")
  
  # AQTSServerID="https://yukon.aquaticinformatics.net/AQUARIUS"
  # AQTSUsername="gtdelapl"
  # AQTSPassword="WQ*2021!"
  # dateRange="all"
  # timeRange=c("00:00:00", "23:59:59")
  # AQID="YOWN-1907"
  # timeSeriesID="Wlevel_btoc.Calculated"
  # chartXInterval="1 year"
  # chartType="Level"
  # saveTo="C:/Users/g_del/Desktop"
  # specName=NULL
  
  #Dowload the data from each source
  if("Aquarius" %in% sources == TRUE){
    Aquarius <- list()
    for (i in unique(TSlist$Aquarius$`Site identifier`)){
      fetch <- subset(TSlist$Aquarius, `Site identifier` %in% i)
      for (j in 1:nrow(fetch)){
        line <- fetch[j] %>% dplyr::mutate_all(as.character) #made as.character to work with timeseries_client.R
        #Make the Aquarius configuration
        config = list(
          # Aquarius server credentials
          server="https://yukon.aquaticinformatics.net/AQUARIUS", username=AQlogin[1], password=AQlogin[2],
          # time series name@location EX: Wlevel_btoc.Calculated@YOWN-XXXX
          timeSeriesName=paste0(line$`TS name`,"@",line$`Site identifier`),
          # Analysis time period
          if(is.na(line$`Start year`)==TRUE){
            eventPeriodStartDay = "1950-01-01"
          }
          if(is.na(line$`Start year`)==FALSE){
            eventPeriodStartDay = paste0(lines$`Start year`,"-01-01")
          }
          if(is.na(line$`End year`)==TRUE){
            eventPeriodEndDay = as.character(Sys.Date())
          }
          if(is.na(line$`End year`)==FALSE){
            eventPeriodEndDay = paste0(lines$`End year`,"-12-31")
          }
          # # Report title
          # uploadedReportTitle = "Test Plot",
          # # Remove pre-existing reports with the same name from Aquarius
          # removeDuplicateReports = TRUE)
         )
        
        # Load supporting code
        source("R/timeseries_client.R")
        # Connect to Aquarius server
        timeseries$connect(config$server, config$username, config$password)
        # Get the location metadata
        locationData = timeseries$getLocationData(timeseries$getLocationIdentifier(config$timeSeriesName))
        utcOffset = timeseries$getUtcOffsetText(locationData$UtcOffset)
        startOfDay = "T00:00:00"
        endOfDay = "T23:59:59.9999999"
        # Prepare for downloading data points based on specified period start and end or for all data points
        fromPeriodStart = paste0(config$eventPeriodStartDay, startOfDay, utcOffset)
        toPeriodEnd = paste0(config$eventPeriodEndDay, endOfDay, utcOffset)
        periodLabel = sprintf("%s - %s", config$eventPeriodStartDay, config$eventPeriodEndDay)
        # Read corrected time-series data from Aquarius
        RawDL <- timeseries$getTimeSeriesCorrectedData(c(config$timeSeriesName),
                                                       queryFrom = fromPeriodStart,
                                                       queryTo = toPeriodEnd)
        #Fix the start time and end time to match either that specified or that in the time-series, whichever is shorter.
        #TODO: FIX THIS!!!!!
        trueStart <- RawDL$Points$Timestamp[1]
        trueStart <- substr(trueStart, 1, 10)
        trueEnd <- RawDL$Points$Timestamp[nrow(RawDL$Points)]
        trueEnd <- substr(trueEnd, 1, 10)
        
        if (is.na(line$`Start year`)==TRUE){
          line$`Start year` <- trueStart
        }
        if (is.na(line$`Start year`)==FALSE){
          if (line$`Start year` < trueStart){
            line$`Start year` <- trueStart
          }
          if (line$`End year` > trueEnd){
            line$`End year` <- trueEnd
          }
        }
      }
      
      
      Aquarius[[i]] <- 
      
    }
      
  }
  
  if("EQWin" %in% sources == TRUE){
    TSlist$EQWin
  }
  
  if("Snow Survey" %in% sources == TRUE){
    
  }
  
    
}