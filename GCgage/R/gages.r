#' @title List of gages available for download.

#' @description Provides a gage list and basic data about the gages that can be downloaded.

#' @details This function takes no arguments and is called simply using \code{gages()}. It is used 
#' internally within the \code{readGages} function, but might provide some minor interpretative value and 
#' so is provided here as well.
#'
#' When using \code{readGages} to download gage data from the US Geological Survey Grand Canyon Montoring
#' and Research Center website (\url{https://www.gcmrc.gov/discharge_qw_sediment/stations/GCDAMP}), 
#' gage names can be specified using either the gage \code{Name} or \code{Number} 
#' specified by this function.
#'
#' The start date for each gage, and its river mile on the Colorado River 
#' (or the river mile of its confluence if the gage is on a tributary) are provided as well.
#' Gages are listed as either \code{Mainstem} (on the Colorado River), 
#' \code{Tributary} (on a tributary adjacent to the mainstem), or 
#' \code{"Watershed"} (far up a tributary, generally with another gage closer to the mainstem confluence).

#' @return A simple table of available gages and information about these gages. Note that the 
#' \code{StartDate} is for any gage data available for a given gage, and may not be available for 
#' that entire date range for the parameters listed in \code{gageVars}.

#' @seealso \code{\link{gageVars}}, which provides a list of available gage parameters.
#' \code{\link{readGage}}, which allows downloading of these gage data from the website.

#' @concept database, website, gage, gauge

#' @examples
#' ## Look up available gages.
#' foo1 <- gages()

#' @author Jeffrey D. Muehlbauer, \email{jmuehlbauer@usgs.gov}

#' @export

gages <- function(){
	tgage1 <- data.frame(Name = c("GlenCanyonDam", "WaterHoles", "CRLeesFerry", "PariaRKanab", 
			"PariaRLeesFerry", "BadgerCreek", "TannerWash", "HouseRockWashAboveEmmettWash", 
			"HouseRockWashBelowEmmettWash", "NorthCanyon", "ShinumoWash", "CR30Mile", "CRLittleCR", 
			"LittleCRGrandFalls", "LittleCRCameron", "MoenkopiWashMoenkopi", "MoenkopiWashCameron", 
			"LittleCRCameron", "LittleCRDesertView", "CRLavaCanyon", "CRBrightAngel", "BrightAngelCreek", 
			"CR127Mile", "KanabCreek", "HavasuCreek", "CRNational", "CRDiamondCreek", "CRSpencerCreek"))
		tgage1$Number = as.factor(c("09379901", "GCMRC-GCLT1", "09380000", "09381800", "09382000", 
			"GCMRC-MCLT1", "GCMRC-MCLT2", "GCMRC-MCLT3", "GCMRC-MCLT4", "GCMRC-MCLT5", "GCMRC-MCLT6", 
			"09383050", "09383100", "09401000", "09401200", "09401260", "09401500", "09402000", 
			"09402300", "09402352", "09402500", "09403000", "09403270", "09403850", "09404115", 
			"09404120", "09404200", "09404220"))
		tgage1$Location <- as.factor(ifelse(substr(tgage1$Name, 1, 2) == "CR" | tgage1$Name == "GlenCanyonDam", 
			"Mainstem", ifelse(tgage1$Name %in% c("PariaRKanab", "HouseRockWashAboveEmmettWash", 
			"LittleCRGrandFalls", "LittleCRCameron", "MoenkopiWashMoenkopi", "MoenkopiWashCameron", 
			"LittleCRCameron"), "Watershed", "Tributary")))
		tgage1$RiverMile = c(-15.7, -3.9, -0.1, 0.8, 0.8, 8.0, 14.5, 17.1, 17.1, 20.7, 29.4, 29.7, 61.1, 61.8, 
			61.8, 61.8, 61.8, 61.8, 61.8, 65.8, 87.9, 88.2, 127.6, 144, 157.3, 166.8, 225.7, 246.2)
		tgage1$StartDate = as.Date(c("1988-08-10", "2001-01-09", "1921-05-08", "2002-09-07", "1980-10-16", 
			"2001-01-08", "2001-01-10", "2001-01-13", "2000-07-06", "2001-01-06", "2001-01-12", 
			"2002-07-08", "1985-09-28", "1925-11-15", "1949-07-19", "1976-06-28", "1953-12-09", 
			"1947-06-01", "1990-05-04", "1998-10-14", "1922-11-12", "1923-10-20", "2006-06-28", 
			"1990-10-25", "1990-11-04", "1989-09-27", "1983-10-01", "1995-07-21"))
		return(tgage1)
}