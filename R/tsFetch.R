#' Utility for automatically fetching time-series data
#' 
#' This script takes the data.table output  from the stationMeta function (truncated if you desire fewer stations) and fetches time-series information for the stations contained in the "Site name", "Location identifier" columns.
#' If you are fetching information from the EQWin or Snow Survey Access databases you MUST have access to the X drive on your machine. Aquarius data can be fetched from anywhere with an internet connection.
#' 
#' BE AWARE that this function may take a long time to execute, perform work on a multi-core machine with lots of memory and a good internet connection!
#'
#' @param TS The data.table output of WRBTrends::stationMeta, containing at minimum the default columns specified by that function.
#' @param sources The list of sources you wish to download data for. "all" means all the sources listed in the "Data location" column of the input data.table. Can also specify from "Aquarius", "EQWin", "Snow Survey", "ECCC", or "Workbook" if you want to exclude any sources. If specifying "Workbook" all time-series must be in individual workbook tabs with columns "datetime" and "value".
#' @param AQlogin The login parameters for Aquarius in format c("username", "password"). Leave NULL if you are not fetching from Aquarius, in which case you should either ensure it is not specified in the input data.table or is excluded under the source parameter.
#' @param HYlogin The login parameters for HYDAT in format c("username", "password"). Leave NULL if you are not fetching from Aquarius, in which case you should either ensure it is not specified in the input data.table or is excluded under the source parameter.
#' @return A list containing one element (tibble) per time-series, with processing performed to standardize the time-series to one common format. Designed to act as input to the WRBTrends:: ???? and ???? functions.
#' 
#' @import data.table
#' @export
#' @examples

tsFetch <- function(TS, sources="all", AQlogin=c("gtdelapl","WQ*2021!"), HYlogin=NULL){
  
  #Find all the data sources
  if (sources=="all"){
    sources <- unique(TS$`Data location`)}
  #Split into a list by source
  TS <- split(TS, by="Data location")
  
  
  #Dowload the data from each source
  #####Aquarius fetch#####
  if("Aquarius" %in% sources == TRUE){
    Aquarius <- list()
    for (i in unique(TS$Aquarius$`Location identifier`)){
      fetch <- subset(TS$Aquarius, `Location identifier` %in% i)
      for (j in unique(fetch$`TS name`)){
        tryCatch( {
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
                "1950-01-01"} else {
                paste0(line$`Start year`,"-01-01")},
            eventPeriodEndDay = 
              if(is.na(line$`End year`)==TRUE){
                as.character(Sys.Date())} else {
                paste0(line$`End year`,"-12-31")})
          
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
          RawDL <- timeseries$getTimeSeriesCorrectedData(c(config$timeSeriesName), queryFrom = fromPeriodStart, queryTo = toPeriodEnd)
          # format base Aquarius time series and combine with values
          timestamp <- data.table::as.data.table(strptime(substr(RawDL$Points$Timestamp,0,19), "%FT%T"))
          value <- data.table::as.data.table(RawDL$Points$Value)
          rawdata <- cbind(timestamp, value)
          data.table::setnames(rawdata, old = c("x", "Numeric"), new = c("timestamp", "value"))
          #Fix the start time and end time to match either that specified or that in the time-series, whichever is shorter; if nothing is specified then the entire Aquarius TS is fetched.
          trueStart <- RawDL$Points$Timestamp[1]
          trueStart <- substr(trueStart, 1, 10)
          trueEnd <- RawDL$Points$Timestamp[nrow(RawDL$Points)]
          trueEnd <- substr(trueEnd, 1, 10)
          
          if (is.na(line$`Start year`)==TRUE){ #if not specified, default to full range
            line$`Start year` <- trueStart}
          if (is.na(line$`End year`)==TRUE){ #if not specified, default to full range
            line$`End year` <- trueEnd}
          if (is.na(line$`Start year`)==FALSE){
            if (line$`Start year` < trueStart){
              line$`Start year` <- trueStart} #if the specified start is before the real start
            if (line$`Start year` > trueStart){
              line$`Start year` <- paste0(line$`Start year`, "-01-01")} #if the specified start is valid.
          }
          if (is.na(line$`End year`)==FALSE){
            if (line$`End year` > trueEnd){
              line$`End year` <- trueEnd} #if the specified end is after the real end
            if(line$`End year` < trueEnd){
              line$`End year` <- paste0(line$`End year`, "-12-31")} #if the specified end is valid.
          }
          
          #truncate according to set dates, if needed
          rawdata <- rawdata[ rawdata$timestamp >= line$`Start year`& rawdata$timestamp <= line$`End year`]
          # Remove NA values and order, and make a list element
          Aquarius[[paste0(i,"_",j)]] <- na.omit(rawdata, cols=c("value","timestamp")) %>% data.table::setorder(cols="timestamp")
          
         },  error = function(e) {paste0("The time-series ", j, "for location ", i, "could not be found on the Aquarius server or could not be processed. Check the exact naming of the location and time-series.")})
        }
    }
  } #End of Aquarius if loop
  
  #####Snow Survey fetch#####
  if("Snow Survey" %in% sources == TRUE){
    #Download the data and select necessary columns
    #TODO: remove the extra path here
    db <- "X:/Snow/DB/SnowDB.mdb"
    #db <- "C:/Users/g_del/OneDrive/Desktop/temp/SnowDB.mdb"
    snowCon <- odbc::dbConnect(drv = odbc::odbc(), .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",db))
    meas <- DBI::dbReadTable(snowCon, "SNOW_SAMPLE")
    odbc::dbDisconnect(snowCon)

    #Manipulate things a bit
    meas <- subset(meas, select=c("SNOW_COURSE_ID","DEPTH","SNOW_WATER_EQUIV","SAMPLE_DATE","EXCLUDE_FLG"))
    meas$month <- lubridate::month(meas$SAMPLE_DATE)
    meas$year <- lubridate::year(meas$SAMPLE_DATE)
    meas$day <- lubridate::day(meas$SAMPLE_DATE)
    meas <- meas[which(meas$EXCLUDE_FLG==0),] # OMIT VALUES OF EXCLUDEFLG=1, aka TRUE
    meas$SAMPLE_DATE = as.character(meas$SAMPLE_DATE) # Change date to character format

    # Special case (i) Twin Creeks - 09BA-SC02B
    #Step 1: Remove 09BA-SC02A values in 2016 (the year of overlap):
    #TODO: Ask Jonathan why snow course still sampled in 21, 22. Also confirm the correction factor.
    #TODO: apply the same correction to depth?
    meas<-meas[!(meas$SNOW_COURSE_ID=="09BA-SC02A" & meas$yr==2016),]
    meas<-meas[!(meas$SNOW_COURSE_ID=="09BA-SC02A" & meas$yr==2021),]
    meas<-meas[!(meas$SNOW_COURSE_ID=="09BA-SC02A" & meas$yr==2022),]
    # Step 2: Multiply all 09BA-SC02A values by 0.79 to estimate the historic 09BA-SC02B values:
    meas$SNOW_WATER_EQUIV[meas$SNOW_COURSE_ID=="09BA-SC02A"] <- 0.79*(meas$SNOW_WATER_EQUIV[meas$SNOW_COURSE_ID=="09BA-SC02A"]) 
    # Step 3: Rename these locations as 09BA-SC02B:
    meas$SNOW_COURSE_ID[meas$SNOW_COURSE_ID=="09BA-SC02A"] <- "09BA-SC02B" 
    
    #TODO: confirm correction factor with Jonathan, check into overlap between the two
    #TODO: apply the same correction to depth?
    # Special case (ii) Hyland 10AD-SC01 [(not A, just blank) vs 10AD-SC01B]
    # Step 1: Select the pre 2018 data
    Target <- meas[which(meas$SNOW_COURSE_ID=="10AD-SC01" & meas$yr<2018),] 
    # Step 2: Rename the snow course ID to the B series
    Target$SNOW_COURSE_ID <- "10AD-SC01B" 
    # Step 3: Multiply SNOW_WATER_EQUIVALENT by the appropriate ratio 
    Target$SNOW_WATER_EQUIV <- 1.17*(Target$SNOW_WATER_EQUIV) 
    # Step 4: Add this data back on to meas, to use for calculating values at 10AD-SC01B:
    meas <- rbind(meas,Target)
    
    #Pull the data out and truncate based on specified start/end dates
    #TODO: add an if loop to deal with SWE and/or depth, specified in the `TS name` column of TS
    SnowSurvey <- list()
    for (i in unique(TS$`Snow Survey`$`Location identifier`)){
      tryCatch( {
        line <- subset(TS$`Snow Survey`, `TS name`%in% i) %>% dplyr::mutate_all(as.character)
        
        #Find the real start/end of the TS; dat becomes the data for i
        dat <- meas[(meas$SNOW_COURSE_ID==i),]
        dat <- dat[order(dat$SAMPLE_DATE),]
        trueStart <- dat$SAMPLE_DATE[1]
        trueEnd <- dat$SAMPLE_DATE[nrow(dat)]
        
        #Fix the Start year/end year
        if (is.na(line$`Start year`)==TRUE){ #if not specified, default to full range
          line$`Start year` <- trueStart}
        if (is.na(line$`End year`)==TRUE){ #if not specified, default to full range
          line$`End year` <- trueEnd}
        if (is.na(line$`Start year`)==FALSE){
          if (line$`Start year` < trueStart){
            line$`Start year` <- trueStart} #if the specified start is before the real start
          if (line$`Start year` > trueStart){
            line$`Start year` <- paste0(line$`Start year`, "-01-01")} #if the specified start year is valid.
        }
        if (is.na(line$`End year`)==FALSE){
          if (line$`End year` > trueEnd){
            line$`End year` <- trueEnd} #if the specified end is after the real end
          if(line$`End year` < trueEnd){
            line$`End year` <- paste0(line$`End year`, "-12-31")} #if the specified end year is valid.
        }
        
        #truncate according to set dates, if needed
        dat <- dat[which(dat$SAMPLE_DATE >= line$`Start year`& dat$SAMPLE_DATE <= line$`End year`),]
        # Remove NA values if depth AND SWE missing, order, and make a list element for i
        SnowSurvey[[i]] <- dat[!with(dat,is.na("DEPTH")& is.na("SNOW_WATER_EQUIV")),] %>% data.table::setorder(cols="SAMPLE_DATE")
        
      },  error = function(e) {paste0("The time-series for location ", i, "could not be found in the Snow Survey database or could not be processed. Check the exact naming of the location and time-series.")})
    }
  } #End of Snow Survey if loop
  
  #####EQWin fetch#####
  if("EQWin" %in% sources == TRUE){
    #Download the data, select necessary columns, combine everything together
    #TODO: remove the extra path here
    #db <- "X:/WR/DB/Water Resources.mdb"
    db <- "C:/Users/g_del/OneDrive/Desktop/temp/Water Resources.mdb"
    EQCon <- odbc::dbConnect(drv = odbc::odbc(), .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",db)) #open the connection
    params <- data.table::as.data.table(DBI::dbReadTable(EQCon, "eqparams") %>% subset(select=c("ParamId", "ParamCode", "ParamName", "Units", "udf_diss_tot_extract", "udf_symbol", "ParamDesc", "ParamClass"))) #get the parameter detail sheet
    samps <- data.table::as.data.table(DBI::dbReadTable(EQCon, "eqsampls") %>% subset(select=c("SampleId", "StnId", "CollectDateTime"))) #get the metadata for each sample
    stns <- data.table::as.data.table(DBI::dbReadTable(EQCon, "eqstns") %>% subset(select=c("StnId", "StnCode", "StnName", "StnDesc", "UTMZone", "Easting", "Northing", "udf_Stn_Status")))
    meas <- data.table::as.data.table(DBI::dbReadTable(EQCon, "eqdetail") %>% subset(select=c("SampleId", "ParamId","Result"))) #get the measurements, contains one line for each analysis
    odbc::dbDisconnect(EQCon) #close the connection
    
    meas <- merge(x=samps, y=meas, by="SampleId") %>% merge(., y=params, by="ParamId") %>% merge(., y=stns, by="StnId") #put it all together into one giant table 
    meas$CollectDateTime <- substr(meas$CollectDateTime, start=1, stop=10) #keep only the date
    
            
    EQWin <- list()
    for (i in unique(TS$`EQWin`$`Location identifier`)){
      fetch <- subset(TS$EQWin, `Location identifier` %in% i)
      for (j in unique(fetch$`TS name`)){
        tryCatch( {
          line <- subset(fetch, `TS name`%in% j) %>% dplyr::mutate_all(as.character)
          
          #Find the real start/end of the TS; dat becomes the data for i
          dat <- meas[(meas$StnName==i),]
          dat <- dat[order(dat$CollectDateTime),]
          trueStart <- dat$CollectDateTime[1]
          trueEnd <- dat$CollectDateTime[nrow(dat)]
          
          #Fix the Start year/end year
          if (is.na(line$`Start year`)==TRUE){ #if not specified, default to full range
            line$`Start year` <- trueStart}
          if (is.na(line$`End year`)==TRUE){ #if not specified, default to full range
            line$`End year` <- trueEnd}
          if (is.na(line$`Start year`)==FALSE){
            if (line$`Start year` < trueStart){
              line$`Start year` <- trueStart} #if the specified start is before the real start
            if (line$`Start year` > trueStart){
              line$`Start year` <- paste0(line$`Start year`, "-01-01")} #if the specified start year is valid.
          }
          if (is.na(line$`End year`)==FALSE){
            if (line$`End year` > trueEnd){
              line$`End year` <- trueEnd} #if the specified end is after the real end
            if(line$`End year` < trueEnd){
              line$`End year` <- paste0(line$`End year`, "-12-31")} #if the specified end year is valid.
          }
          
          #truncate according to set dates, if needed
          dat <- dat[which(dat$CollectDateTime >= line$`Start year`& dat$CollectDateTime <= line$`End year`),]
          # make a list element for i
          EQWin[[paste0(i,"_",j)]] <- dat
        },  
        error = function(e) {paste0("The time-series for location ", i, "and parameter ",j, "could not be found in the EQWin database or could not be processed. Check the exact naming of the location and time-series.")})
      }
    }
  } #End of EQWin if loop

    
  #####Workbook fetch#####
  if("Workbook" %in% sources == TRUE){
    Workbook <- list()
    for (i in unique(TS$Workbook$`Location identifier`)){
      fetch <- subset(TS$Workbook, `Location identifier` %in% i)
      for (j in unique(fetch$`TS name`)){
        tryCatch( { 
          Workbook[[paste0(i,"_",j)]] <- na.omit(rawdata)
        },
        error = function(e) {paste0("The time-series ", j, "for location ", i, "could not be found or could not be processed. Check the exact naming of the location and time-series.")})
      }
    }
  } #End of Workbook if loop
  
  #####WSC fetch#####
  if("WSC" %in% sources == TRUE){
    WSC <- list()
    for (i in unique(TS$WSC$`Location identifier`)){
      fetch <- subset(TS$WSC, `Location identifier` %in% i)
      for (j in unique(fetch$`TS name`)){
        tryCatch( { 
          WSC[[paste0(i,"_",j)]] <- na.omit(rawdata)
        },
        error = function(e) {paste0("The time-series ", j, "for location ", i, "could not be found or could not be processed. Check the exact naming of the location and time-series.")})
      }
    }
  } #End of WSC if loop
  
  
  #####ECCC fetch#####
  if("ECCC" %in% sources == TRUE){
    ECCC <- list()
    for (i in unique(TS$ECCC$`Location identifier`)){
      fetch <- subset(TS$ECCC, `Location identifier` %in% i)
      for (j in unique(fetch$`TS name`)){
        tryCatch( { 
          ECCC[[paste0(i,"_",j)]] <- na.omit(rawdata)
        },
        error = function(e) {paste0("The time-series ", j, "for location ", i, "could not be found or could not be processed. Check the exact naming of the location and time-series.")})
      }
    }
  } #End of ECCC if loop
  
  
  
} # End of function