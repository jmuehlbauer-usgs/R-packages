#' @title Download Grand Canyon gage data

#' @description Allows data download from gages in and around the Colorado River in Grand Canyon.

#' @param gage Vector of gages to download. See Details. Default is \code{09380000} (Lees Ferry).
#' @param vars Vector of gage parameters to download. See Details. Default is \code{all}. 
#' @param startDate The beginning date for the data download (includes this day). See Details. Default is 
#' \code{first}.
#' @param endDate The end date for the data download (includes this day). See Details. Default is 
#' \code{today}.
#' @param cutCols Whether to cut any empty (\code{NA}) columns (gage parameters). Default is \code{TRUE}.
#' @param cutRows Whether to cut any empty (\code{NA}) rows of data. The website often does this 
#' internally regardless. Default is \code{TRUE}.
#' @param writeCSV Whether to write the downloaded data to a CSV file. See Details. Default is \code{FALSE}.

#' @details 
#' This function enables downloading of gage data from the US Geological Survey Grand Canyon Montoring 
#' and Research Center website (\url{https://www.gcmrc.gov/discharge_qw_sediment/stations/GCDAMP}). 
#'
#' Gage names and parameters available for download can be found using a call to \code{gages()} and 
#' \code{gageVars()}, respectively. Gages can be specified using either the gage \code{Name} or 
#' \code{Number}.
#'
#' The \code{startDate} and \code{endDate} specified indicate the first and last day of gage data to be 
#' dowloaded, includive of those dates. The start date for each gage can be found using a call to 
#' \code{gages()}. Dates should be formatted as \code{YYYY-MM-DD} (\code{'\%Y-\%m-\%d'} in \code{R} syntax);
#' shortcuts \code{'first'} and \code{'today'} can also be used to download data from the first date of 
#' record for each gage, to today.
#'
#' The arguments \code{cutCols} and \code{cutRows} will truncate the data for each gage to only the 
#' DateTimes that have any data associated with them. In other words, these arguments will cut out any 
#' columns and rows that contain only \code{NA}, respectively. Note that the website automatically 
#' truncates blank rows at the beginning and end of the dataset, so in practice much of this \code{NA} 
#' removal occurs regardless of the value specified for these arguments.
#'
#' The argument \code{writeCSV} specifies whether the downloaded gage data should be written to a .csv 
#' file. If \code{writeCSV = TRUE}, gage data are written to the working directory, with the filename
#' \code{GageData_NAME.csv}, where \code{NAME} is the name of each gage specified in the \code{gage}
#' argument. Alternately, instead of \code{TRUE}, \code{writeCSV} can accept a filename or filepath. 
#' In this case, the gage name will be appended to the character string specified by \code{writeCSV}
#' (see Examples).

#' @return If \code{gage} is a vector of multiple gages, returns a list of dataframes, with one list 
#' element for each gage. These are sorted in the order specified by \code{gage}, with the list elemetn 
#' named according to the names specified by \code{gage} as well. If \code{gage} is only a single gage, 
#' the function returns a single dataframe of gage data (not a list). If no data are available for a gage, 
#' only \code{NA} is returned. This is provided (rather than an error) to enable batch download of 
#' multiple gages. 

#'
#' Note on units: The first column is always \code{DateTime}, which is provided in \code{POSIXlt} format 
#' in Mountain Standard Time (MST). Units on other columns (parameters) can be found using a call to 
#' \code{gageVars()}.
#' 
#' Note on processing time: For downloads from a single gage for ~1 year or less of data, download times
#' should be similar to the time it would take to download a .tsv file directly from the website. For much 
#' larger datasets or multiple gages, download times can increase dramatically. These will be processed in 
#' parallel and are generally much faster than downloading directly from the website, but may take
#' several minutes.

#' @seealso \code{\link{gages}}, which provides a list of available gages.
#' \code{\link{gageVars}}, which provides a list of available gage parameters.

#' @concept database, website, gage, gauge

#' @examples
#' ## Look up available gages.
#' foo1 <- gages()
#'
#' ## Look up available gage parameters.
#' foo2 <- gageVars()
#'
#' ## Download discharge and stage data from Lees Ferry (AKA 09380000) and 30-Mile, from 1-2 January, 2002.
#' foo3 <- readGage(gage = c("09380000", "CR30Mile"), vars = c("Discharge", "Stage"), 
#'		startDate = "2002-01-01", endDate = "2002-01-02")
#'
#' ## Same as foo3, but write data to a "Data" folder in the working directory.
#' foo4 <- readGage(gage = c("09380000", "CR30Mile"), vars = c("Discharge", "Stage"), 
#'		startDate = "2002-01-01", endDate = "2002-01-02", writeCSV = "Data/")

#' @author Jeffrey D. Muehlbauer, \email{jmuehlbauer@usgs.gov}

#' @export

# Function call
readGage <- function(gage = "09380000", vars = "all", startDate = "first", endDate = "today", 
	cutCols = TRUE, cutRows = TRUE, writeCSV = FALSE){
	# Gage download and dataframe building, by gage
	gagefx <- function(x){
		# Get gage number
		if(substr(x, 1, 2) %in% c("09", "GC")){
			tgages2 <- tgages1[tgages1$Number == x,]
		} else{
			tgages2 <- tgages1[tgages1$Number == tgages1$Number[which(tgages1$Name == x)],]
		}
		vgages1 <- as.character(tgages2$Number)
		# Get gage variables
		tvars1 <- gageVars()
		if(vars[1] == "all"){
			tvars2 <- tvars1
		} else{
			t1 <- match(tvars1$Parameter, vars)
			tvars2 <- tvars1[which(!is.na(t1))[t1[which(!is.na(t1))]],]
		}
		# Get dates, format html for download
		if(class(startDate) == 'Date'){
			vstartd <- startDate
		} else{
			if(startDate == 'first'){
				vstartd <- tgages2$StartDate
			} else{
				if(startDate == 'today'){
					vstartd <- as.Date(Sys.time())
		}}}
		if(class(endDate) == 'Date'){
			vendd <- endDate
		} else{
			if(endDate == 'first'){
				vendd <- tgages2$StartDate
			} else{
				if(endDate == 'today'){
					vendd <- as.Date(Sys.time())
		}}}
		if(vendd < vstartd){
			vendd <- vstartd
		}
		vstartc <- as.character(vstartd)
		vendc <- as.character(vendd)
		vhtml1 <- paste0("&column%5B%5D=inst!", as.character(tvars2$WebCode), "!", vgages1, "!*default*", 
			collapse = "")
		# Download data for the specified dates in parallel
		if(vparallel1 == 'dates'){
			vcores1 <- parallel::detectCores()-1
			vcores2 <- ifelse(vcores1 == 0, 1, vcores1)
			cl1 <- parallel::makeCluster(getOption("cl1", vcores2))
			parallel::clusterExport(cl1, c("tvars2", "vhtml1"), envir = environment())
			vdates1 <- as.Date(vstartd):as.Date(vendd)
			ldates1 <- split(vdates1, rep(1:vcores2, length.out = length(vdates1), 
				each = ceiling(length(vdates1)/vcores2)))
			ldates2 <- lapply(ldates1, function(x){
				as.character(as.Date(c(x[1], x[length(x)]), origin = "1970-01-01"))
			})
			lgagedat1 <- parallel::parLapply(cl1, ldates2, function(x){
				vhtml2 <- paste0("https://www.gcmrc.gov/discharge_qw_sediment/services/agg?beginPosition=", 
					x[1], "&endPosition=", x[2], "&column%5B%5D=time!yyyy-MM-dd+HH%3Amm%3Ass!*default*")
				vhtml3 <- paste0(vhtml2, vhtml1, "&tz=-7&tzInHeader=true&output=tab&download=on")
				tgagedat1 <- tryCatch({
					read.delim(vhtml3)
					}, error = function(cond){
						tdaterr1 <- matrix(nrow = 2, ncol = 1 + dim(tvars2)[1])
						tdaterr2 <- as.data.frame(tdaterr1)
							tdaterr2[, 1] <- as.factor(x)
						return(tdaterr2)
					})
					colnames(tgagedat1) <- c("DateTime", as.character(tvars2$Parameter))
				return(tgagedat1)
			})
			parallel::stopCluster(cl1)
			tgagedat2 <- do.call(rbind, lgagedat1)
		# Download data for the specified dates, not in parallel
		} else{
			vhtml2 <- paste0("https://www.gcmrc.gov/discharge_qw_sediment/services/agg?beginPosition=", 
				vstartc, "&endPosition=", vendc, "&column%5B%5D=time!yyyy-MM-dd+HH%3Amm%3Ass!*default*")
			vhtml3 <- paste0(vhtml2, vhtml1, "&tz=-7&tzInHeader=true&output=tab&download=on")
			tgagedat2 <- tryCatch({
				read.delim(vhtml3)
				}, error = function(cond){
					tdaterr1 <- matrix(nrow = 2, ncol = 1 + dim(tvars2)[1])
					tdaterr2 <- as.data.frame(tdaterr1)
						tdaterr2[, 1] <- as.factor(c(vstartc, vendc))
					return(tdaterr2)
				})
				colnames(tgagedat2) <- c("DateTime", as.character(tvars2$Parameter))
		}
		# Format data
		tgagedat2$DateTime <- strptime(tgagedat2$DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "MST")
		tgagedat2[tgagedat2 == -999] <- NA
		# Cut columns
		if(cutCols == TRUE){
			tgagedat3 <- tgagedat2[colSums(!is.na(tgagedat2)) > 0]
		} else{
			tgagedat3 <- tgagedat2
		}
		# Cut rows
		if(cutRows == TRUE){
			tgagedat4 <- tgagedat3[rowSums(!is.na(data.frame(tgagedat3[, -1]))) > 0,]
		} else{
			tgagedat4 <- tgagedat3
		}
		# Warn if no data
		if(is.null(dim(tgagedat4))){
			message(paste0("Warning: No data available for any of these dates and parameters for gage '", 
				x, "'. NAs returned."))
		} else{
			if(dim(tgagedat4)[2] == 1 | 
				sum(colSums(data.frame(tgagedat4[, -1]), na.rm = TRUE)) == 0){
				message(paste0("Warning: No data available for any of these dates and parameters for gage '", 
					x, "'. NAs returned."))
				}
		}
		# Return dataframe
		if(is.null(dim(tgagedat4))){
			tgagedat5 <- NA
		} else{
			tgagedat5 <- tgagedat4
				rownames(tgagedat5) <- NULL
		}
		return(tgagedat5)
	}
	
	# Error check for gage name format
	vgage1 <- gage[!gage %in% unlist(gages()[, c('Name', 'Number')])]
	if(length(vgage1) > 0){
		vgage2 <- paste0(vgage1, collapse = "', '")
		stop(paste0("Gage(s) '", vgage2, "' are not accessible via this function. 
		Check for typos and use a call to 'gage()' to see available gages. 
		Gages can be called using either their gage name or number, in quotes."))
	}
	# Error check for gage parameter name format
	vvars1 <- vars[!vars %in% gageVars()$Parameter]
	if(length(vvars1) > 0 & vars != "all"){
		vvars2 <- paste0(vvars1, collapse = "', '")
		stop(paste0("Gage variable(s) '", vvars2, "' are not allowable parameters.
		Check for typos and use a call to 'gageVars()' to see available parameters."))
	}
	# Error check for gage date format
	datelist <- list(startDate, endDate)
	badclass <- sapply(datelist, function(x){
		'try-error' %in% class(try(as.Date(x), silent = TRUE))
	})
	if(badclass[1] == FALSE){startDate <- as.Date(startDate)}
	if(badclass[2] == FALSE){endDate <- as.Date(endDate)}	
	badchar <- sapply(datelist, function(x){
		!x %in% c('first', 'today')
	})
	if((badclass[1] == TRUE & badchar[1] == TRUE) |
		(badclass[2] == TRUE & badchar[2] == TRUE)){
		stop("The start &/or end date format is incorrect.
		It needs to be either a date (in YYYY-MM-DD, Date, or POSIX format), or 'first', or 'today' (in quotes).")		
	}
	# Error check for gage end date before start date
	if(!TRUE %in% badclass){
		if(endDate < startDate){
			warning(paste0("The end date (", endDate, ") is prior to the start date (", startDate, ").",
			"\nGage data have been downloaded with these dates swapped for one another."),
				call. = FALSE)	
			startDateTEMP <- startDate
			startDate <- endDate
			endDate <- startDateTEMP
		}
	}
	# Combine dataframes into list
	tgages1 <- gages()
	# Run gage download in parallel for multiple gages (1 cluster per gage)
	if(length(gage) > 1){
		vparallel1 <- 'gages'
		vcores1 <- parallel::detectCores()-1
		vcores2 <- ifelse(vcores1 == 0, 1, vcores1)
		cl1 <- parallel::makeCluster(getOption("cl1", vcores2))
		lgages1 <- parallel::parLapply(cl1, gage, gagefx)
		parallel::stopCluster(cl1)
	# Run gage download not in parallel (single gage)
	} else{
		vrange1 <- ifelse(nchar(startDate) < 10 | nchar(endDate) < 10, NA, 
			as.Date(endDate) - as.Date(startDate))
		# Run single gage download in parallel if number of days > 1 year
		if(vrange1 > 365 | is.na(vrange1)){
			vparallel1 <- 'dates'
		} else{
			vparallel1 <- 'none'
		}
		lgages1 <- lapply(gage, gagefx)
	}
	names(lgages1) <- gage
	if(length(lgages1) == 1){
		lgages1 <- lgages1[[1]]
	}
	# Write data to csv
	if(writeCSV != FALSE){
		if(writeCSV == TRUE){
			vfile1 <- "GageData_"
		} else{
			if(grepl("/", writeCSV, fixed = TRUE) & 
				dir.exists(substr(writeCSV, 1, regexpr("/", writeCSV)[1])) == FALSE){
				dir.create(substr(writeCSV, 1, regexpr("/", writeCSV)[1]), showWarnings = FALSE)
			}
			vfile1 <- writeCSV
		}
		for(i in 1:length(gage)){
			if(length(gage) == 1){
				write.csv(lgages1, paste0(vfile1, gage[i], ".csv"), row.names = FALSE)
			} else{
				write.csv(lgages1[[i]], paste0(vfile1, gage[i], ".csv"), row.names = FALSE)	
			}
		}
	}
	return(lgages1)
}