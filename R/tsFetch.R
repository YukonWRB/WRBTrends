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


tsFetch <- function(TSlist, sources="all", AQlogin=NULL, HYlogin=NULL){
  
  #Find all the data sources
  if (sources=="all"){
    sources <- unique(TS$`Data location`)
  }
  
  #Split into a list by source
  TSlist <- split(TS, by="Data location")
  
  #Dowload the data from each source
  if("Aquarius" %in% sources == TRUE){
    
    Aquarius <- list()
    for (i in TSlist$Aquarius$`Site identifier`){
      fetch <- subset(TSlist$Aquarius, `Site identifier` %in% i)
      for (i in 1:nrow(fetch)){
        fetch[i]
      }
      list[[i]] <- 
      
    }
      
  }
  
  if("EQWin" %in% sources == TRUE){
    TSlist$EQWin
  }
  
  if("Snow Survey" %in% sources == TRUE){
    
  }
  
    
}