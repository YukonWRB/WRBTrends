#' Regional or Seasonal Kendall Test Helper Function
#'
#' @description Takes a list of data.frames and pre-processes them for use with the rkt package to return a regional/seasonal Kendall p-value and estimated slope. Adds rows to data.frames in the list to make each the same length and creates a blocking variable for each data.frame. Can factor covariables and correlation between blocks as well as deal with multiple data points in the same time period, but the underlying function rkt::rkt does not currently handle covariables.
#'
#' @param data A list of data.frames to test, with each data.frame representing one station (for regional Kendall) or season (for seasonal Kendall). Data.frames must contain a column of type numeric with the data to be analyzed (valuesColumn) and a matching integer column representing regular interval (timeColumn). Data frames in a list can be of different lengths but must have identical column names.
#' @param valuesColumn The column name of the values to be tested.
#' @param timeColumn The column name of the date/time representative integer values.
#' @param covariable (optional) A column in the data.frames representing a covariable. Refer to ?rkt::rkt for more information.
#' @param correlate.correct Boolean T/F. If T, correction for correlation between blocks is performed. Refer to ?rkt::rkt for more information.
#' @param rep What to do with data sharing the same date. Set to "a" to average, "m" for median. Any other value results in error if two or more data points share a date. As per rkt::rkt.
#'
#' @return An object of type "rkt" with the Kendall Tau, S, and variance of S, a 2-sided p-value, and a seasonal/regional Kendall slope.
#' @export
#'

easyRKT <- function(data, valuesColumn, timeColumn, covariable="none", correlate.correct=F, rep="e" ){

  #find the first and last time point/date on record and make a data.frame with that range
  dat <- do.call(rbind.data.frame, data)
  first <- min(dat[[timeColumn]])
  last <- max(dat[[timeColumn]])
  range <- data.frame(year=first:last)
  range <- stats::setNames(range, timeColumn)
  siteCode <- data.frame(siteCode = rep(1:length(data), each=nrow(range)))

  #Use full_join to add missing time points to each station/season, then bind them together to one data.frame
  data.all <- lapply(data, function(x) dplyr::full_join(x, range))
  data.all <- lapply(data.all, function(x) dplyr::arrange(x, get(timeColumn)))
  data.all <- as.data.frame(do.call(rbind.data.frame, data.all))
  data.all <- bind_cols(data.all, siteCode) #Add in a site code to act as blocks for each region

  #Finally... run the Regional Kendall test!
  if (covariable == "none") {
    RegionalKendallResult <- rkt(data.all[[timeColumn]], data.all[[valuesColumn]], block=data.all$siteCode, correct=correlate.correct, rep=rep)
  return(RegionalKendallResult)
  }

 #Uncomment the code below when/if rkt package works with a covariable.
  if (covariable != "none") {
    print("Factoring a covariable was not functional in the rkt::rkt function when this function was written. Test rkt::rkt and update this function if it is now supported.")
    # RegionalKendallResult <- rkt(data.all[[timeColumn]], data.all[[valuesColumn]], block=data.all$siteCode, cv=data.all[[covariable]], correct=correlate.correct, rep=rep )
  }

}
