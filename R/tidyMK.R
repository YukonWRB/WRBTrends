#' Tidy Mann Kendal Result Table
#'
#' Calculate and return tidy data.frame of Mann Kendall results
#'
#' @description
#' This function uses the `MannKendall()` function from the Kendall package and applies the default two-sided test.  The values tested must be either a vector of numeric or time series values.
#'
#' @param ygDataList A list of data.frame or tibble data objects providing station data.
#' @param valueColumn The column name, character or integer, of the data values to be tested.
#' @param idLabel A character string to label the id column.
#'
#' @return A data.frame of test results: Mann-Kendall tau and 2-sided p-value. One row per dataframe in the provided list.
#' @export
#'
#'
#TODO: explore whether this should be combined with tidySens to yield one output only.
#TODO: should also work with data.frames, not just lists.


tidyMK <- function(ygDataList, valueColumn, idLabel="Site") {

	# Run the test statistics and grab the tau and p-value results
	out <- lapply(ygDataList, function(x) Kendall::MannKendall(x[[valueColumn]]))
	out <- do.call(rbind.data.frame, out)
	# these function generate row names based on list element names
	# Pull these out into a data column as the unique identifiers
	out[[idLabel]] <- row.names(out)
	# Keep just the desired stats columns
	out <-  out %>%
		dplyr::select(idLabel,
									`Mann Kendall tau` = tau,
									`2-sided p-value` = sl)

	# Tidy by rounding numeric values to third decimal
	out[["Mann Kendall tau"]] <- round(out[["Mann Kendall tau"]], 3)
	out[["2-sided p-value"]] <- round(out[["2-sided p-value"]], 3)

	# Bring row names into the data as Station and then reorder columns
	out <- out %>%
		dplyr::select(idLabel, `Mann Kendall tau`, `2-sided p-value`)

	return(out)

}
