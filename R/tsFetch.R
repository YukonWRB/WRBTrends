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
  
  
  #Dowload the data from each source
  if("Aquarius" %in% sources == TRUE){
    Aquarius <- list()
    for (i in unique(TSlist$Aquarius$`Location identifier`)){
      fetch <- subset(TSlist$Aquarius, `Location identifier` %in% i)
      for (j in unique(fetch$`TS name`)){
        line <- subset(fetch, `TS name`%in% j) %>% dplyr::mutate_all(as.character) #made as.character to work with timeseries_client.R
        #Make the Aquarius configuration
        config = list(
          # Aquarius server credentials
          server="https://yukon.aquaticinformatics.net/AQUARIUS", username=AQlogin[1], password=AQlogin[2],
          # time series name@location EX: Wlevel_btoc.Calculated@YOWN-XXXX
          timeSeriesName=paste0(line$`TS name`,".Logger@",line$`Location identifier`),
          # Analysis time period
          eventPeriodStartDay =
            if(is.na(line$`Start year`)==TRUE){
            "1950-01-01"
            } else{
              paste0(line$`Start year`,"-01-01")
            },
          eventPeriodEndDay = 
            if(is.na(line$`End year`)==TRUE){
            as.character(Sys.Date())
            } else {
              paste0(line$`End year`,"-12-31")
            },
          # Report title
          uploadedReportTitle = "Test Plot",
          # Remove pre-existing reports with the same name from Aquarius
          removeDuplicateReports = TRUE)
        
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
        #Fix the start time and end time to match either that specified or that in the time-series, whichever is shorter; if nothing is specified then the entire Aquarius TS is fetched.
        trueStart <- RawDL$Points$Timestamp[1]
        trueStart <- substr(trueStart, 1, 10)
        trueEnd <- RawDL$Points$Timestamp[nrow(RawDL$Points)]
        trueEnd <- substr(trueEnd, 1, 10)
        
        if (is.na(line$`Start year`)==TRUE){ #if not specified, default to full range
          line$`Start year` <- trueStart
        }
        if (is.na(line$`End year`)==TRUE){ #if not specified, default to full range
          line$`End year` <- trueEnd
        }
        if (is.na(line$`Start year`)==FALSE){
          if (line$`Start year` < trueStart){
            line$`Start year` <- trueStart
          }
          if (line$`Start year` > trueStart){
            #add in month and day 01-01
          }
        }
        if (is.na(line$`End year`)==FALSE){
          if (line$`End year` > trueEnd){
            line$`End year` <- trueEnd
          }
          if(line$`End year` < trueEnd){
            #add in month and day 12-31
          }
        }
        
        # Create full timestamp series spanning specified (or automatically selected) time range, 1hr intervals
        fullTS <- data.table::as.data.table(seq.POSIXt(strptime(paste(line$`Start year`, "00:00:00"), format = "%Y-%m-%d %T"), strptime(paste(line$`End year`, "23:59:59"), format = "%Y-%m-%d %T"), by="hour"))
        data.table::setnames(fullTS, old = c("x"), new = c("timestamp"))
        
        # format base Aquarius time series
        timestamp <- data.table::setDT(data.table::as.data.table(strptime(substr(RawDL$Points$Timestamp,0,19), "%FT%T")))
        value <- data.table::setDT(data.table::as.data.table(RawDL$Points$Value))
        #TODO: make the line below a data.table operation for time savings
        rawdata <- as.data.frame(cbind(timestamp, value))
        data.table::setnames(rawdata, old = c("x", "Numeric"), new = c("timestamp", "value"))
        
        # Join full timestamp series to native data time series
        Aquarius[[paste0(i,"_",j)]] <- dplyr::full_join(fullTS, rawdata)
      }
    }
#TODO: use code in Lana's xle to csv script to round rawdata measurements to the nearest hour, and make it discard measurements occurring more than every hour. For measurements every few hours or daily, discard rows where there is no value to reduce data size.
  }
  
  
  if("EQWin" %in% sources == TRUE){
    TSlist$EQWin
  }
  
  if("Snow Survey" %in% sources == TRUE){
    
  }
    
    
} # End of function