#' @title List of gage parameters available for download.

#' @description Provides a list of gage parameters, names, and units.

#' @details This function takes no arguments and is called simply using \code{gageVars()}. It is used 
#' internally within the \code{readGages} function, but might provide some minor interpretative value and 
#' so is provided here as well.
#'
#' When using \code{readGages} to download gage data from the US Geological Survey Grand Canyon Montoring
#' and Research Center website (\url{https://www.gcmrc.gov/discharge_qw_sediment/stations/GCDAMP}), 
#' gage parameters (variables) can be specified using the \code{Parameter} name specified by this function.

#' @return A simple table of available gage parameters, along with their units.

#' @seealso \code{\link{gages}}, which provides a list of available gages.
#' \code{\link{readGage}}, which allows downloading of these gage data from the website.

#' @concept database, website, gage, gauge

#' @examples
#' ## Look up available gage parameters.
#' foo1 <- gageVars()

#' @author Jeffrey D. Muehlbauer, \email{jmuehlbauer@usgs.gov}

#' @export

gageVars <- function(){
	data.frame(Parameter = c("Stage", "Discharge", "Temperature", "SpC", "DO", "Turbidity", "TSS", 
			"Fines", "Sand", "D50", "InstFines", "InstSand", "CumFines", "CumSand"),
		FullName = c("Gage Height", "Discharge", "Water Temperature", "Specific Conductance", 
			"Dissolved Oxygen", "Turbidity", "Total Suspended Sediment", "Suspended Fines (Silt+Clay)", 
			"Suspended Sand", "Median Sand Grain Size", "Instantaneous Fines Load", 
			"Instantaneous Sand Load", "Cumulative Fines Load", "Cumulative Sand Load"),
		WebCode = c("Stage", "Discharge", "Water+Temp", "Sp+Cond", "Dissolved+Oxygen", 
			"Turbidity", "Calc+Total+Susp+Sed", "Susp+Fines", "Susp+Sand", "D50+Sand", "S+Fines+Inst+Load", 
			"S+Sand+Inst+Load", "S+Fines+Cumul+Load", "S+Sand+Cumul+Load"),
		Units = c("ft", "ft^3/s", "C", "uS/cm", "mg/L", "FNU", "mg/L", "mg/L", "mg/L", "mm", "kg/s", 
			"kg/s", "1000 kg (Ton)", "1000 kg (Ton)"))
}