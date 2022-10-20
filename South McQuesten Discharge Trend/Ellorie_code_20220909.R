# Ellorie Mann-Kendall

# Load packages ----

library(readxl) # for readinf excel
library(openxlsx) # for writing excel
library(magrittr) #for the pipes %>% and %<>%
library(dplyr) # used to tidy, restructure,and summarize data
library(rkt) #https://cran.r-project.org/web/packages/rkt/index.html not as well developed or supported?
library(ggplot2)
library(wql) #https://cran.r-project.org/web/packages/wql/index.html and https://cran.r-project.org/web/packages/wql/vignettes/wql-package.html  looks quite useful for this and other analyses
library(Kendall) #https://cran.r-project.org/web/packages/Kendall/index.html looks current - but very narrow focus, just K-rank and MK tests
library(broom) #used to extract from stats from the Kendall test result into a dataframe
library(lubridate) #used to manage date data

# Load & tidy data ---

raw <- readxl::read_xlsx("SMcQuesten_flow_ECCCdata.xlsx") %>%
	dplyr::rename("Discharge" = "...4")

#table is a nice quick way to inspect data
table(raw$YEAR)
# 372 observations per year?!
# 2015 has complete daily discharge data, but the YEAR value is blank (you can see if you look in Excel)

tidy <- raw %>%
	# missing year values are all 2015, so apply simple correction
	dplyr::mutate(YEAR = if_else(is.na(YEAR), 2015, YEAR)) %>%
	# then remove lines with no discharge (these are not real dates)
	dplyr::filter(!is.na(Discharge))

table(tidy$YEAR, tidy$MM)
# Looks like Dec 2011 has a couple missing values, but nothing too serious
# you could interpolate these if you wanted

# Add a date column for use in visuals (if want to show full data series in background)
tidy %<>%
	dplyr::mutate(Date = lubridate::mdy(paste0(MM,"/",DD,"/",YEAR)))

# Explore the data and various model tools----

# Generate the summary stats for each Year-Month period calculate the summary stats
summary <- tidy %>%
	dplyr::group_by(YEAR, MM) %>%
	dplyr::summarize(AVE = mean(Discharge),
									 MIN = min(Discharge),
									 MAX = max(Discharge),
									 Q90 = quantile(Discharge, 0.9),
									 Q10 = quantile(Discharge, 0.1),
									 .groups = "drop") %>%
	# add a mid-month date for use if want to plot summary points on
	# top of the full (daily) data
	dplyr::mutate(Date = lubridate::mdy(paste0(MM, "-15-", YEAR)))

# test seasonal models with rkt
# seasonal test calculates for each month independently and then sums (or averages?) results to return one value summarizing the overall annual trend
# but rkt never shows the intermediate results 

aveMM_Model <- rkt::rkt(date = summary$YEAR, y = summary$AVE, block = summary$MM, correct = TRUE)
minMM_Model <- rkt::rkt(date = summary$YEAR, y = summary$MIN, block = summary$MM, correct = TRUE)
maxMM_Model <- rkt::rkt(date = summary$YEAR, y = summary$MAX, block = summary$MM, correct = TRUE)

#see Figure 12.11 here: https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=&ved=2ahUKEwiLzKezzej5AhWnlYkEHVmUDM8QFnoECEoQAQ&url=https%3A%2F%2Fwww.flrules.org%2Fgateway%2FreadRefFile.asp%3FrefId%3D2957%26filename%3DSeasonal%2520Kendall%2520trend%2520test.pdf&usg=AOvVaw0woa4fSP7WUDH7N1aBQ3TX

# The rkt model method could be done with a simple numeric vectors of values, but the wql methods require a time series vector of values.  Convert the data in the summary table to time-series, then rerun models with second method
summary_ts <- ts(dplyr::select(summary, AVE:Q10), frequency = 12, start = c("2008", 1))
wql::seaKen(summary_ts)
wql::seaKen(summary_ts, plot=TRUE, order=TRUE)
# numbers agree with B from rkt - increases confidence that we are giving the correct instructions for a seasonal test

# messing around with graphs ----

# To create a panel plot with one month per plot - but all data in background
# Need to remove MM from the full annual data
allFlow <- dplyr::select(tidy, -MM)

# quick visualization by month
ggplot() +
	geom_line(data = allFlow, aes(x=Date, y=Discharge), color = "darkblue") +
	geom_point(data = summary, aes(x=Date, y = AVE), color = "cyan", shape = 15) +
	geom_point(data = summary, aes(x=Date, y = MIN), color = "goldenrod", shape = 16) +
	geom_point(data = summary, aes(x=Date, y = MAX), color = "darkred", shape = 17) +
	facet_wrap(~MM) +
	theme_bw()
# not a super useful plot

# annual data (each summary stat per year)
annual <- tidy %>%
	dplyr::group_by(YEAR) %>%
	dplyr::mutate(AVE = mean(Discharge),
								MIN = min(Discharge),
								MAX = max(Discharge),
								.groups = "drop") %>%
	dplyr::mutate(Date = lubridate::mdy(paste0("06-30-", YEAR))) # center of year

# simple slope: y = xa + b
# x: number years, a: sens slope coefficient, b: initial discharge
nYears <- dplyr::n_distinct(annual$YEAR)
firstDischarge <- mean(annual$Discharge[annual$YEAR == min(annual$YEAR)])


# Manually create data series for sens slope
# MK sens slope for the year
slopeLine <- data.frame(SEQYEAR = 1:(nYears*12),
												YEAR = rep(min(annual$YEAR):max(annual$YEAR), each = 12)) %>%
	dplyr::mutate(YEARPNT = (SEQYEAR * aveMM_Model$B) + firstDischarge,
								Date = lubridate::mdy(paste0(1:12, "-15-", YEAR)))
	
ggplot() +
	geom_line(data = summary, aes(x=Date, y=AVE), color = "grey") +
	geom_point(data = annual, aes(x=Date, y=AVE), color = "red") +
  geom_line(data = slopeLine, aes(x = Date, y = YEARPNT))

# The seasonal man kendall seems to remove effects of season.  If you actually want to socus on trends within a given season (month) - I think you should just do a regular Mann Kendall on individual months across years.

# This loop creates a monthy stats summary table (MIN, AVE, etc per year), converts the data to time series and then calculates the set of slopes with pvalues for each summary stat
# Basically checking  that the code is working as expected
for (i in 1:12){
	print(i)
	dat <- dplyr::filter(summary, MM == i)
	dat_ts <- ts(dplyr::select(dat, AVE:Q10), frequency = 1, start = c("2008"))
	out <- wql::mannKen(dat_ts)
	print(out)
}
# that test shows some months have pos slope, some neg, and some no slope.
# but doesn't also give the tau stats - so also do Kendall::MannKendall

#### This is what you wanted #####

# This does the same as above, but uses the Kendall package to get Tau, then pits all the data together in a single table

# run for requestedState stat- you can say which one you want.

requestedStat <- "AVE" # choose AVE, MIN, MAX, Q90, or Q10

for (i in 1:12){
	# get months data and convert to ts object
	dat <- dplyr::filter(summary, MM == i)
	dat_ts <- ts(dplyr::select(dat, contains(requestedStat)), frequency = 1, start = c("2008"))
	# get tau onfo
	tau_result <- broom::tidy(Kendall::MannKendall(dat_ts)) %>%
		dplyr::mutate(Month = i)
	# get slope info
	slope_result <- wql::mannKen(dat_ts)
	# bind results with reach other and previous months
	result <- cbind(tau_result, slope_result)
	if (i == 1){
		out <- result
	} else {
		out <- rbind(out, result)
	}
	print(out)
	#raw slope line plot
	slopePoints = (1:nYears * out$sen.slope[i]) + summary[[requestedStat]][summary$YEAR == 2008 & summary$MM == i]
	dat <- cbind(dat, slopePoints)
	plot <- ggplot() +
		geom_point(data = dat, aes(x=YEAR, y=get(requestedStat)), color = "red") +
		geom_line(data = dat, aes(x=YEAR, y=slopePoints)) +
		labs(title = lubridate::month(i, label=T, abbr=F))
	print(plot)
	
}

# Send results to Excel tables in this project folder
openxlsx::write.xlsx(dat, here::here(paste0("Monthy_", requestedStat, "_SummaryStats.xlsx")), overwrite=T)
openxlsx::write.xlsx(out, here::here(paste0("Monthy_", requestedStat, "_MK_TauSens_Stats.xlsx")), overwrite=T)

###### End of code that does what you want ########

## Other data visualizations ----

maxDatePlotData <- tidy %>%
	dplyr::mutate(falseAxisDate = lubridate::ymd(paste0("2000-", MM, "-", DD))) %>%
	dplyr::group_by(YEAR, MM) %>%
	dplyr::mutate(MaxDischarge = max(Discharge, na.rm = TRUE)) %>%
	dplyr::ungroup() %>%
	dplyr::mutate(MaxMatch = as.character(ifelse(Discharge == MaxDischarge, 1, 0)),
								MM = lubridate::month(MM, label=TRUE, abbr=FALSE))

checkData <- dplyr::filter(maxDatePlotData, MaxMatch == "1")
table(checkData$DD) # this strikes me as a suspicious distribution... I don't trust these data...

# Monthly max plot -it's very busy...
ggplot() +
	geom_point(data = maxDatePlotData, aes(x=falseAxisDate, y=YEAR, color = MaxMatch), shape = 4) +
	scale_color_manual(values = c("1" = "blue", "0" = NA), na.value = NA) +
	scale_y_continuous(limits = c(min(maxDatePlotData$YEAR), max(maxDatePlotData$YEAR)),
										 breaks = min(maxDatePlotData$YEAR):max(maxDatePlotData$YEAR)) +
	scale_x_date(limits = c(min(maxDatePlotData$falseAxisDate), max(maxDatePlotData$falseAxisDate)), date_breaks = "1 week")+
	guides(color = "none") +
	theme(axis.text.x = element_text(angle =90))

#Facet by month for a clearer view of patterns (and suspicious first/last day max)
ggplot() +
	geom_point(data = maxDatePlotData, aes(x=falseAxisDate, y=YEAR, color = MaxMatch, group = MM), shape = 4) +
	scale_color_manual(values = c("1" = "blue", "0" = NA), na.value = NA) +
	scale_y_continuous(limits = c(min(maxDatePlotData$YEAR), max(maxDatePlotData$YEAR)),
										 breaks = min(maxDatePlotData$YEAR):max(maxDatePlotData$YEAR)) +
	scale_x_date(date_breaks = "1 week", date_labels = "%d")+
	guides(color = "none") +
	theme(axis.text.x = element_text(angle =90)) +
	facet_wrap(~MM, scales = "free_x")
