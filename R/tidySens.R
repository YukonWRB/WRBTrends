#' Tidy Sens Slope Result Table
#'
#' Calculate and return a tidy data.frame of Sen's slope results
#'
#' @description
#' Uses the `sens.slope()` function from the trend package to calculate a slope estimate with 95% confidence intervals and a significance level (p-value) of the slope estimate. The values tested must be either a numeric vector or time series object, present as a column in a data.frame or list of data.frames.
#'
#' @param data A data.frame or list of data.frame objects providing data. Data points must be ordered from oldest to youngest in each data.frame. Use as.data.frame if selecting an item from a list.
#' @param Column The column name, character or integer, of the data values to be tested.
#' @param idLabel A character string to label the id column (eg. "Station", "Site", "Creek").
#'
#' @return A data.frame of test results: number of points and high and low confidence intervals. One row per dataframe in the provided list.
#' @export
#'
#'

tidySens <- function(data, Column, idLabel="Site") {

  if (class(data)=="list") {
	 # First run the sens slope test and output N and conf intervals
	 out <- lapply(data, function(x) trend::sens.slope(x[[Column]]))
	 out <- lapply(out, function(x) broom::tidy(x))
	 out <- as.data.frame(do.call(rbind.data.frame, out))
	 # these function generate row names based on list element names
	 # Pull these out into a data column as the unique identifiers
	 out[[idLabel]] <- row.names(out)
	 # Keep just the desired stats columns
	 out <-  out %>%
	 	dplyr::select(idLabel,
						 			`Number of data points` = parameter,
					 				`95% CI low` = conf.low,
									`95% CI high` = conf.high,
									`P-value` = p.value)

	 # Tidy by removing name attributes of the Number column
	 # And round numeric values to third decimal
	 # And bring rownames into dataframe as column values
	 out[["Number of data points"]] <- as.integer(out[["Number of data points"]])
	 out[["95% CI low"]] <- round(out[["95% CI low"]], 3)
	 out[["95% CI high"]] <- round(out[["95% CI high"]], 3)


	 # Then calculate and append the slope estimates
	 slope <- as.data.frame(data %>%
												 	purrr::map_df(.,~{
												 		test = trend::sens.slope(.x %>% dplyr::pull(get(Column)))
												 		test = tibble(`Slope estimate` = test["estimates"] %>%
												 										unlist)
												 	}))
	 # Assign station names in same order
	 slope[[idLabel]] <- out[[idLabel]]

	 # Tidy by removing name attributes of the Slope estimate column
	 # And round numeric values to third decimal
	 slope[["Slope estimate"]] <- round(as.numeric(slope[["Slope estimate"]]), 3)

	 # Merge the data and reorder columns
	 out <- merge(slope, out, by=idLabel) %>%
	 	 dplyr::select(idLabel,
									`Number of data points`,
									`Slope estimate`,
									`P-value`,
									`95% CI low`,
									`95% CI high`)
	 return(out)
  }

  if (class(data)=="data.frame") {
    # First run the sens slope test and output N and conf intervals
    sens <- trend::sens.slope(data[[Column]])
    out <- broom::tidy(sens)
    out <- as.data.frame(out)
    # these function generate row names based on list element names
    # Pull these out into a data column as the unique identifiers
    out[[idLabel]] <- row.names(out)
    # Keep just the desired stats columns
    out <-  out %>%
      dplyr::select(idLabel,
                    `Number of data points` = parameter,
                    `95% CI low` = conf.low,
                    `95% CI high` = conf.high,
                    `P-value` = p.value)

    # Tidy by removing name attributes of the Number column
    # And round numeric values to third decimal
    # And bring rownames into dataframe as column values
    out[["Number of data points"]] <- as.integer(out[["Number of data points"]])
    out[["95% CI low"]] <- round(out[["95% CI low"]], 3)
    out[["95% CI high"]] <- round(out[["95% CI high"]], 3)


    # Then append the slope estimates
    slope <- tibble(`Slope estimate` = sens["estimates"] %>%
                                               unlist)

    # Assign station names in same order
    slope[[idLabel]] <- out[[idLabel]]

    # Tidy by removing name attributes of the Slope estimate column
    # And round numeric values to third decimal
    slope[["Slope estimate"]] <- round(as.numeric(slope[["Slope estimate"]]), 3)

    # Merge the data and reorder columns
    out <- merge(slope, out, by=idLabel) %>%
      dplyr::select(idLabel,
                    `Number of data points`,
                    `Slope estimate`,
                    `P-value`,
                    `95% CI low`,
                    `95% CI high`)
    return(out)
  }

}
