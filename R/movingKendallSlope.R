#' Moving Kendall Slope on Single or Regional Time-Series
#'
#' @description
#' Calculates a Mann-Kendall significance level and a Sen's slope (if single data.frame) or regional/seasonal Kendall slope (if list of data.frames) for a given time window within a larger time interval, with the time window moving incrementally to cover the specified time interval.
#'
#' @param data A data.frame or a list containing the data.frames (one per site or season) to test. Must contain a column of type numeric with the data to be analyzed, and a matching integer column in sequence representing years, months, days, or other regular interval. Data frames in a list can be of different lengths but must have identical column names. Use as.data.frame if selecting a list element.
#' @param valuesColumn The column name of the values to be tested.
#' @param timeColumn The column name of the date/time representative integer values.
#' @param start The first data point (year, month, day, etc) that will be considered in the rolling trend calculations (first calculated slope is from start:(end + period)). This year must be present in at least one of the data.frames in the supplied list.
#' @param end The last data point (year, month, day, etc) considered in the rolling trend calculations.
#' @param valuesUnits A character vector describing the data you are analyzing.
#' @param timeUnits A character vector describing the time units ("days", "years", "months").
#' @param period The length of the moving window.
#'
#' @return A data.frame with three columns: one column of p-values, one column of regional Kendall slopes (for lists) or Sen's slopes (data.frame), and one column showing the last data point (year, month, day, etc) used in the calculations.
#' @export
#'

movingKendallSlope <- function(data, valuesColumn, timeColumn, start, end, valuesUnits="mm", timeUnits="years", period=10) {

  if (class(data)=="list"){
   #find the first and last year on record and make a data.frame with that range, plus numeric code for each region (site)
   dat <- do.call(rbind.data.frame, data)
   first <- min(dat[[timeColumn]])
   last <- max(dat[[timeColumn]])
   range <- data.frame(range=first:last)
   range <- stats::setNames(range, timeColumn)
   siteCode <- data.frame(siteCode = rep(1:length(data), each=nrow(range)))

   #Use full_join to add missing data points to each station, then bind them together to one data.frame.
   data.all <- lapply(data, function(x) dplyr::full_join(x, range))
   data.all <- lapply(data.all, function(x) dplyr::arrange(x, get(timeColumn)))
   data.all <- as.data.frame(do.call(rbind.data.frame, data.all))
   data.all <- dplyr::bind_cols(data.all, siteCode) #Add in a site code to act as blocks for each region


   #Run the regional Kendall: subset the data.frame into 10-year periods and run each period. Combine it all in one data.frame.

   listNames <- (start+(period-1)):end
   rollingKendall <- list()
   for (i in listNames){
     eachRange <- seq(from=(i-(period-1)), to=i, by=1)
     dat <- dplyr::filter(data.all, get(timeColumn) %in% eachRange)
     result <- rkt::rkt(dat[[timeColumn]], dat[[valuesColumn]], block=dat$siteCode)
     result <- unname(unlist(result[c(1,3)]))
     rollingKendall[[i]] <- result
   }
   rollingKendall <- rollingKendall[listNames]
   names(rollingKendall) <- listNames
   rollingKendall <- plyr::ldply(rollingKendall)
   colnames(rollingKendall) <- c(paste0("Period end (",timeUnits,")"), "SL of period", paste0("Slope of period (", valuesUnits, ") per ", timeUnits))

   return(rollingKendall)
  }

  if (class(data)=="data.frame"){
#Run the MK and the Sen's Slope: subset the data.frame into 10-year periods and run each period. Combine it all in one data.fram.
    listNames <- (start+(period-1)):end
    rollingKendall <- list()
    for (i in listNames){
      eachRange <- seq(from=(i-(period-1)), to=i, by=1)
      dat <- dplyr::filter(data, get(timeColumn) %in% eachRange)
      KT <- Kendall::MannKendall(dat[[valuesColumn]])
      Sen <- trend::sens.slope(dat[[valuesColumn]])
      KT <- unname(unlist(KT[2]))
      Sen <- unname(unlist(Sen[1]))
      res <- cbind(KT, Sen)
      rollingKendall[[i]] <- res
    }
    rollingKendall <- rollingKendall[listNames]
    names(rollingKendall) <- listNames
    rollingKendall <- plyr::ldply(rollingKendall)
    colnames(rollingKendall) <- c(paste0("Period end (",timeUnits,")"), "SL of period", paste0("Slope of period (", valuesUnits, ") per ", timeUnits))

    return(rollingKendall)
  }

}
