#' Utility for automatically fetching time-series data
#' 
#' This script takes the data.table output  from the stationMeta function (truncated if you desire fewer stations) and fetches time-series information for the stations contained in the "Site name", "Location identifier" columns present in the Excel workbook fed to stationMeta. The Data location column informs where this script looks for data, and entries there must correspond exactly to one of EQWin, Aquarius, Snow Survey Access, Workbook, WSC, or ECCC.
#' 
#' BE AWARE that this function may take a long time to execute, perform work on with lots of memory and a good internet connection!
#' 
#' If you are fetching information from the EQWin or Snow Survey Access databases you MUST have access to the X drive on your machine. Aquarius, WSC, and ECCC data can be fetched from anywhere with an internet connection.
#' 
#' If you are using Access databases you will need to download and install the Microsoft Access Database Engine Redistributable on your machine. As of spring 2022 the latest version is here: https://www.microsoft.com/en-us/download/details.aspx?id=54920. 
#' 
#' This will work fine if you have R and Access in 32 OR 64 bit versions. If that's not the case then the best way forward is to upgrade your R and Office installations to 64 bit versions. If that's not possible (you have 64 bit Windows and R but 32 bit Access for example) you will still need the 64-bit version of the redistributable. Windows won't easily let you do it, and this method is a hack, but if you insist: download and save the 64-bit version somewhere. Then, install it from the command prompt with warnings suppressed. Open the command prompt, then type the path to the .exe followed by quiet like this: C:\Users\gtdelapl>Downloads\accessdatabaseengine_X64.exe/quiet. WARNING: this might damage your Office installation. If it did, you can try uninstalling the redistributable from Add/Remove Programs, if that fails just remove the Office install and start again with the proper 64-bit version.
#' 
#' If you are specifying an Excel workbook with time-series, each tab in the workbook must contain a single time-series. The tab must be named as Location identifier TS name, with Location identifier and TS name EXACTLY matching the relevant entries in the Excel metadata workbook, separated by a space. The first column of each tab must contain a time/date information, the second tab must contain the corresponding value. The header must be on line 1 (column names are not important) with no empty lines.
#'
#'The Snow Survey processing includes applying a correction factor to Hyland and Twin Creeks stations to harmonize the new stations with the old stations.
#'
#' @param TS The data.table output of WRBTrends::stationMeta, containing at minimum the default columns specified by that function.
#' @param sources The list of sources you wish to download data for. "all" means all the sources listed in the "Data location" column of the input data.table. Can also specify from "Aquarius", "EQWin", "Snow Survey", "ECCC", or "Workbook" if you want to exclude any sources. If specifying "Workbook" all time-series must be in individual workbook tabs with columns "datetime" and "value".
#' @param AQlogin The login parameters for Aquarius in format c("username", "password"). Leave NULL if you are not fetching from Aquarius, in which case you should either ensure it is not specified in the input data.table or is excluded under the source parameter.
#' @param HYlogin The login parameters for HYDAT in format c("username", "password"). Leave NULL if you are not fetching from Aquarius, in which case you should either ensure it is not specified in the input data.table or is excluded under the source parameter.
#' @param WorkbookPath The exact path to the Excel workbook containing time-series information that you wish to analyze, if applicable. Each tab in the workbook (from 1 to n) should contain a single time-series, named as   Location identifier TS name. Location identifier and TS name must EXACTLY match the entries in the Excel metadata workbook, separated by a space.
#' @param SnowSurveyPath The exact path to the Snow Survey Access database. If specifying an Access database see the note below.
#' @param EQWinPath The exact path to the EQWin access database. If specifying an Access database see the note below.
#' 
#' @return A list containing one element (tibble) per time-series, with processing performed to standardize the time-series to one common format. Designed to act as input to the WRBTrends:: ???? and ???? functions.
#' 
#' @import data.table
#' @export
#' @examples

tsFetch <- function(TS, sources="all", AQlogin=c("gtdelapl","WQ*2021!"), HYlogin=NULL, WorkbookPath=NULL, SnowSurveyPath="X:/Snow/DB/SnowDB.mdb", EQWinPath="X:/EQWin/WR/DB/Water Resources.mdb"){
  
  #db <- "C:/Users/g_del/OneDrive/Desktop/temp/SnowDB.mdb"
  
  #Find all the data sources
  if (sources=="all"){
    sources <- unique(TS$`Data location`)}
  #Split into a list by source
  TS <- split(TS, by="Data location")
  
  
  #Download the data from each source
  #####Aquarius fetch#####
  if("Aquarius" %in% sources == TRUE){
    # Load supporting code
    source("R/timeseries_client.R")
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
              if(is.na(line$`Start date`)==TRUE){
                "1950-01-01"},
            eventPeriodEndDay = 
              if(is.na(line$`End date`)==TRUE){
                as.character(Sys.Date())})
          
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
          
          if (is.na(line$`Start date`)==TRUE){ #if not specified, default to full range
            line$`Start date` <- trueStart}
          if (is.na(line$`End date`)==TRUE){ #if not specified, default to full range
            line$`End date` <- trueEnd}
          if (is.na(line$`Start date`)==FALSE){
            if (line$`Start date` < trueStart){
              line$`Start date` <- trueStart} #if the specified start is before the real start
          }
          if (is.na(line$`End date`)==FALSE){
            if (line$`End date` > trueEnd){
              line$`End date` <- trueEnd} #if the specified end is after the real end
          }
          
          #truncate according to set dates, if needed
          rawdata <- rawdata[ rawdata$timestamp >= line$`Start date`& rawdata$timestamp <= line$`End date`]
          # Remove NA values and order, and make a list element
          Aquarius[[paste0(i,"_",j)]] <- na.omit(rawdata, cols=c("value","timestamp")) %>% data.table::setorder(cols="timestamp")
          
         },  error = function(e) {paste0("The time-series ", j, "for location ", i, "could not be found on the Aquarius server or could not be processed. Check the exact naming of the location and time-series.")})
        }
    }
  } #End of Aquarius if loop
  
  #####Snow Survey fetch#####
  if("Snow Survey" %in% sources == TRUE){
    #Download the data and select necessary columns
    snowCon <- odbc::dbConnect(drv = odbc::odbc(), .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",SnowSurveyPath))
    meas <- DBI::dbReadTable(snowCon, "SNOW_SAMPLE")
    odbc::dbDisconnect(snowCon)

    #Manipulate things a bit
    meas <- subset(meas, select=c("SNOW_COURSE_ID","DEPTH","SNOW_WATER_EQUIV","SAMPLE_DATE","EXCLUDE_FLG"))
    meas$month <- lubridate::month(meas$SAMPLE_DATE)
    meas$year <- lubridate::year(meas$SAMPLE_DATE)
    meas$day <- lubridate::day(meas$SAMPLE_DATE)
    meas <- meas[which(meas$EXCLUDE_FLG==0),] # OMIT VALUES OF EXCLUDEFLG=1, aka TRUE
    meas$SAMPLE_DATE = as.character(meas$SAMPLE_DATE) # Change date to character format

    # Special case (i) Twin Creeks - 09BA-SC02B: 09BA-SC02B will take precedence over A from 2016 onwards. A correction factor can be calculated from the overlapping years of data for SWE and depth.
    #Step 1: Remove 09BA-SC02A values in 2016 and later. 
    meas <- meas[!(meas$SNOW_COURSE_ID=="09BA-SC02A" & meas$year>=2016),]
    # Step 2: Multiply all 09BA-SC02A values by 0.825 (DEPTH) and 0.853 (SWE):
    meas$SNOW_WATER_EQUIV[meas$SNOW_COURSE_ID=="09BA-SC02A"] <- 0.825*(meas$SNOW_WATER_EQUIV[meas$SNOW_COURSE_ID=="09BA-SC02A"])
    meas$DEPTH[meas$SNOW_COURSE_ID=="09BA-SC02A"] <- 0.853*(meas$DEPTH[meas$SNOW_COURSE_ID=="09BA-SC02A"])
    # Step 3: Rename these locations as 09BA-SC02B (A will no longer exist here)
    meas$SNOW_COURSE_ID[meas$SNOW_COURSE_ID=="09BA-SC02A"] <- "09BA-SC02B" 
    
    # Special case (ii) Hyland 10AD-SC01 and 10AD-SC01B. B will take precedence over A from 2018 onwards. A correction factor can be calculated from the overlapping years of data for SWE and depth.
    #Step 1: Remove SC01 blank values in 2018 and later.
    meas <- meas[!(meas$SNOW_COURSE_ID=="10AD-SC01" & meas$year>=2018),]
    #Step 2: Multiply values later than 2018 to match B series: SWE by 1.127, depth by 1.115
    meas$SNOW_WATER_EQUIV[meas$SNOW_COURSE_ID=="10AD-SC01"] <- 1.127*(meas$SNOW_WATER_EQUIV[meas$SNOW_COURSE_ID=="10AD-SC01"])
    meas$DEPTH[meas$SNOW_COURSE_ID=="10AD-SC01"] <- 1.115*(meas$DEPTH[meas$SNOW_COURSE_ID=="10AD-SC01"])
    # Step 3: Rename these locations as 010AD-SC01B (blank will no longer exist)
    meas$SNOW_COURSE_ID[meas$SNOW_COURSE_ID=="10AD-SC01"] <- "10AD-SC01B" 
    
    #Pull the data out and truncate based on specified start/end dates
    SnowSurvey <- list()
    for (i in unique(TS$`Snow Survey`$`Location identifier`)){
      fetch <- subset(TS$`Snow Survey`, `Location identifier` %in% i)
      for (j in unique(fetch$`TS name`)){
      tryCatch( {
        line <- subset(fetch, `TS name` %in% j) %>% dplyr::mutate_all(as.character)
        
        #Keep only j, find the real start/end of the TS; dat becomes the data for i
        dat <- meas[(meas$SNOW_COURSE_ID==i),]
        #TODO: keep only SWE or Depth column
        if (line$`TS name` == SWE){
          
        }
        if(line$`TS name` == Depth){
          
        }
        dat <- dat[order(dat$SAMPLE_DATE),]
        trueStart <- dat$SAMPLE_DATE[1]
        trueEnd <- dat$SAMPLE_DATE[nrow(dat)]
        
        #Fix the Start date/End date
        if (is.na(line$`Start date`)==TRUE){ #if not specified, default to full range
          line$`Start date` <- trueStart}
        if (is.na(line$`End date`)==TRUE){ #if not specified, default to full range
          line$`End date` <- trueEnd}
        if (is.na(line$`Start date`)==FALSE){
          if (line$`Start date` < trueStart){
            line$`Start date` <- trueStart} #if the specified start is before the real start
        }
        if (is.na(line$`End date`)==FALSE){
          if (line$`End date` > trueEnd){
            line$`End date` <- trueEnd} #if the specified end is after the real end
        }
        
        # Truncate according to set dates, if needed
        dat <- dat[which(dat$SAMPLE_DATE >= line$`Start date`& dat$SAMPLE_DATE <= line$`End date`),]
        
        # Order and make a list element for i,j
        SnowSurvey[[paste0(i,"_",j)]] <- dat %>% data.table::setorder(cols="SAMPLE_DATE")
        
      },  error = function(e) {paste0("The time-series ", j," for location ", i, " could not be found in the Snow Survey database or could not be processed. Check the exact naming of the location and time-series.")})
    }
  } #End of Snow Survey if loop
  
  #####EQWin fetch#####
  if("EQWin" %in% sources == TRUE){
    #Download the data, select necessary columns, combine everything together
    EQCon <- odbc::dbConnect(drv = odbc::odbc(), .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",EQWinPath)) #open the connection
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
          
          #Fix the Start date/End date
          if (is.na(line$`Start date`)==TRUE){ #if not specified, default to full range
            line$`Start date` <- trueStart}
          if (is.na(line$`End date`)==TRUE){ #if not specified, default to full range
            line$`End date` <- trueEnd}
          if (is.na(line$`Start date`)==FALSE){
            if (line$`Start date` < trueStart){
              line$`Start date` <- trueStart} #if the specified start is before the real start
          }
          if (is.na(line$`End date`)==FALSE){
            if (line$`End date` > trueEnd){
              line$`End date` <- trueEnd} #if the specified end is after the real end
          }
          
          #truncate according to set dates, if needed
          dat <- dat[which(dat$CollectDateTime >= line$`Start date`& dat$CollectDateTime <= line$`End date`),]
          # make a list element for i
          EQWin[[paste0(i,"_",j)]] <- dat
        },  
        error = function(e) {paste0("The time-series for location ", i, "and parameter ",j, "could not be found in the EQWin database or could not be processed. Check the exact naming of the location and time-series.")})
      }
    }
  } #End of EQWin if loop

    
  #####Workbook fetch#####
  WorkbookPath <- "C:/Users/gtdelapl/Desktop/Book1.xlsx"
  if("Workbook" %in% sources == TRUE){
    sheets <- readxl::excel_sheets(WorkbookPath) #get the names of each sheet in the workbook
    data <- list()
    for (i in sheets){
      data[[i]] <- readxl::read_xlsx(WorkbookPath, sheet=i) #bring the sheets into R as list elements
    }
      
    Workbook <- list()
    for (i in unique(TS$Workbook$`Location identifier`)){
      fetch <- subset(TS$Workbook, `Location identifier` %in% i)
      for (j in unique(fetch$`TS name`)){
        tryCatch( { 
          line <- subset(fetch, `TS name`%in% j) %>% dplyr::mutate_all(as.character) #pull out a single line from the metadata
          dat <- data[[paste0(i," ",j)]] #Pull the data to a new list, if it exists
          colnames(dat) <- c("time", "vaue") #standardize column names
          Workbook[[paste0(i,"_",j)]] <- dat #make the new Workbook/list
        },
        error = function(e) {paste0("The time-series ", j, "for location ", i, "could not be found in the specified Excel workbook or could not be processed. Check the exact naming of the location and time-series.")})
      }
    }
  } #End of Workbook if loop
  
  #####WSC fetch#####
  if("WSC" %in% sources == TRUE){
    WSC <- list()
    tidyhydat::download_hydat() #download the latest and greatest hydat database
    
    for (i in unique(TS$WSC$`Location identifier`)){
      fetch <- subset(TS$WSC, `Location identifier` %in% i)
      for (j in unique(fetch$`TS name`)){
        tryCatch( {
          line <- subset(fetch, `TS name`%in% j) %>% dplyr::mutate_all(as.character) #pull out a single line from the metadata
          if (j == "Level"){ #Bit below determines if start and end dates are specified, if not defaults to all dates
            if(is.na(line$`Start date`)==TRUE & is.na(line$`End date`)==TRUE){
              dat <- tidyhydat::hy_daily_levels(station_number=i)}
            if(is.na(line$`Start date`)==FALSE & is.na(line$`End date`)==TRUE){
              dat <- tidyhydat::hy_daily_levels(station_number=i, start_date=line$`Start date`)}
            if(is.na(line$`Start date`)==FALSE & is.na(line$`End date`)==FALSE){
              dat <- tidyhydat::hy_daily_levels(station_number=i, start_date=line$`Start date`, end_date=line$`End date`)}
          }
          if (j == "Flow") { #Bit below determines if start and end dates are specified, if not defaults to all dates
            if(is.na(line$`Start date`)==TRUE & is.na(line$`End date`)==TRUE){
              dat <- tidyhydat::hy_daily_flows(station_number=i)}
            if(is.na(line$`Start date`)==FALSE & is.na(line$`End date`)==TRUE){
              dat <- tidyhydat::hy_daily_flows(station_number=i, start_date=line$`Start date`)}
            if(is.na(line$`Start date`)==FALSE & is.na(line$`End date`)==FALSE){
              dat <- tidyhydat::hy_daily_flows(station_number=i, start_date=line$`Start date`, end_date=line$`End date`)}
          }
          
          WSC[[paste0(i,"_",j)]] <- dat
        },
        error = function(e) {paste0("The time-series ", j, "for location ", i, "could not be found in the Water Survey of Canada Hydat database or could not be processed. Check the exact naming of the location and time-series.")})
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
          line <- subset(line, `TS name`%in% j) %>% dplyr::mutate_all(as.character) #pull out a single line from the metadata
          #if date is not specified, default to 1900 and to Sys.Date() to capture all
          if (is.na(line$`Start date`)==TRUE){
            line$`Start date` <- "19000101"}
          if (is.na(line$`End date`)==TRUE){
            line$`End date` <- gsub("-", "", Sys.Date())}
         dat <-  read.csv(paste0("https://aquatic.pyr.ec.gc.ca/webdataonlinenational/en/Measurements/LastDataResultsDownloadCSV/Sites/", i ,"/Projects/PYLTM/Regions/0/vars/", j, "/from/", line$`Start date`,"/to/", line$`End date`, "/utcToLocal/0/page/1?"))
         
            ECCC[[paste0(i,"_",j)]] <- dat
        },
        error = function(e) {paste0("The time-series ", j, "for location ", i, "could not be found at https://aquatic.pyr.ec.gc.ca/webdataonlinenational/en/ or could not be processed. Check the exact naming of the location and time-series, and that it does indeed exist.")})
      }
    }
  } #End of ECCC if loop
  
  
  
} # End of function
