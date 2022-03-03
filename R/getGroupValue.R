#' Extract Max Record from Date-based Groups
#'
#' Identify and retain only max or min value from grouped data
#'
#' @description
#' This function uses the group_by() functions from the dplyr package and applies the with_ties = FALSE option to retain only one record per group.  The group column can be a date object, an integer, or character variable to group by.
#'
#' @param df A data.frame or tibble data object including a date and value column
#' @param groupColumn The column name, character or integer, of the group column. Can be a blocking variable representing sites, dates, or other groups. If using complete dates (year, month, day present) ensure as.date format yyyy-mm-dd. Incomplete dates (i.e. only years) should remain as integers.
#' @param valueColumn The column name, character or integer, of the data values to be tested.
#' @param value The value to be returned, either "min", "max", "mean", or "median.
#' @param dateGroup If using a date formatted groupColumn, specify whether grouping by "year", "month", or "day".
#'
#' @return A data.frame of test results: number of points and high and low confidence intervals. One row per dataframe in the provided list.
#' @export
#'

#TODO: make this work with columns that are not dates but can also be stations or sites. Requires different workflow to work with non-date objects that will take the place of groupColumn.
#TODO: make a version of this for lists.

getGroupValue <- function(df, groupColumn, valueColumn, value = "max", dateGroup = "noDate"){


  #Group by specified date component
  if (dateGroup == "noDate"){
    out <- df %>%
      dplyr::group_by(get(groupColumn))
  }
  if (dateGroup == "year"){
    out <- df %>%
      dplyr::mutate(Year = lubridate::year(get(groupColumn))) %>%
      dplyr::group_by(Year)
  }
  if (dateGroup == "month"){
    out <- df %>%
      dplyr::mutate(Month = lubridate::month(get(groupColumn))) %>%
      dplyr::group_by(Month)
  }
  if (dateGroup =="day"){
    out <- df %>%
      dplyr::mutate(Day = lubridate::day(get(groupColumn))) %>%
      dplyr::group_by(day)
  }

  #Get the requested value
	if (value == "max"){
		out <- out %>%
			dplyr::slice_max(get(valueColumn), n=1, with_ties = FALSE)
	}
	if (value == "min"){
		out <- out %>%
			dplyr::slice_min(get(valueColumn), n=1, with_ties = FALSE)
	}
  if (value == "mean"){
    out <- mean(out[[valueColumn]])
  }
  if (value == "median"){
    out <- stats::median(out[[valueColumn]])
  }
  dplyr::ungroup(out)
	return(out)
}
