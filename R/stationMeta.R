#' Utility for ingesting and filtering station/timeseries metadata
#'
#'This script takes a .xlsx document containing metadata and grading information for all WRB time-series, brings it into R as a list of tibbles, and extracts grading information to facilitate the reporting of trend analyses. It can prepare a data.frame containing information to fetch data from Aquarius, the Snow Survey and EQWin Access databases, ECCC, and a user-specified workbook or prepare a list of tibbles containing metadata for the time-series meeting the minGrade specification.
#'
#' @param path The path to the Excel workbook containing station/time-series metadata, as a character vector.
#' @param colList The list of columns you wish to include in the final output, as a character vector. Defaults to all columns yielding useful station metadata.
#' @param gradeColumn The column containing grade information to use as a filter with gradeRange.
#' @param gradeRange NULL or a numeric vector of length 2, as in c(4,6). Default NULL returns all regardless of grade, specifying a range returns only the time-series with a GRADE final score value within the (inclusive) range.
#' @param product Specify "data.table"  or "list", depending on if you want a single data.frame or a list of tibbles. The single data.frame will be formatted for input to the tsFetch and other package functions for trend analysis, while the list of tibbles will contain a list element for each tab in the Excel workbook. In either case the tabs specified in ignore.tabs will not be included.
#' @param ignore.tabs The name(s) of tabs to ignore in the Excel workbook, as a character vector.
#'
#' @return A data.table ready for input to the tsFetch function (can be used for other purposes), or a list of tibbles. Both outputs contain the same information but in a different form.
#' @export
#'
#' @examples
#' 

stationMeta <- function(path, colList = c("Site name", "Location identifier", "TS name", "Start date", "End date","Data location", "Latitude", "Longitude", "GRADE length", "GRADE gaps", "GRADE disturbance", "GRADE data quality", "GRADE final score"), gradeColumn="GRADE final score", gradeRange=NULL, product="data.table", ignore.tabs=c("README", "Parameter codes for WQN", "Filter and Grade all sites")){
  
  #bring the sheets into R
  sheets <- readxl::excel_sheets(path) #get the names of each sheet in the workbook
  TS <- list()
  for (i in sheets){
    TS[[i]] <- readxl::read_xlsx(path, sheet=i, skip=1)
  }

  #extract location IDs, names, and grades or other specified columns from the tabs not excluded
  TS <- TS[!names(TS) %in% ignore.tabs]
  TS <- lapply(TS, function(x) x %>% dplyr::select(all_of(colList)))
  
  #filter by minGrade
  if (is.null(gradeRange)==FALSE) {
    TS <- lapply(TS, function(x) x %>% dplyr::filter(get(gradeColumn)>=gradeRange[1] & get(gradeColumn)<=gradeRange[2]))
  }
  
  #create a data.table or a list of tibbles
  if(product=="data.table"){
    TS <- data.table::rbindlist(TS, use.names=TRUE, idcol="Network")
  }
  if (product!="list" & product !="data.table") {
    print("Your input for the product parameter is not valid.")
  }
  return(TS)
}
