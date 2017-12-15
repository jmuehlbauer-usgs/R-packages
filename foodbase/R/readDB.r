#' @title Read in sample and specimen data from the Foodbase database.

#' @description Pulls exported data from the Foodbase database for use in R.

#' @param gear The sampling gear type of interest (\code{Drift}, \code{LightTrap}, etc). Default is \code{Drift}.
#' @param type Whether to download \code{Sample}, \code{Specimen} or  \code{SppList} (the master species list) data. Default is \code{Sample}.
#' @param updater Whether to download new versions of the Sample, Specimen, and Species List data, and from what source. See details. Default is \code{FALSE}.

#' @details
#' Currently only \code{Drift} is implemented for \code{gear}.
#'
#' The \code{type} argument specifies whether to return \code{Sample} data (i.e., sample collection information), \code{Specimen} data (i.e., bug counts and sizes, where relevant), or \code{SppList} data (i.e., the master species list). If you are interested in working with both \code{Sample} and \code{Specimen} data, then using this function to get the \code{Sample} data, filtering to only the data of interest, then running the \code{\link{foodbase}} function will be faster, and more useful (see examples).
#'
#' Regardless of the \code{updater} setting, \code{readDB} checks for a local copy of the \code{Sample}, \code{Specimen}, and \code{SppList} data in the \code{Data} folder of the \code{foodbase} package, and will install these three files there if they are not present (these local copies are what is used for other functions within the \code{foodbase} package). Using \code{updater} will update all three of these files, regardless of what data type is specified by \code{type}. 
#'
#' If not set to \code{FALSE}, the \code{updater} can be set to download new data from the \code{Network} (if \code{TRUE} or \code{Network}), or from \code{GitHub}.
	
#' @return Creates a dataframe containing the desired data from the Foodbase database.

#' @seealso \code{\link{foodbase}}, which provides more powerful formatting for working with data from the Foodbase database.

#' @concept access, database

#' @examples
#' ## Read in drift sample data from the network.
#' foo <- readDB(gear = "Drift", type = "Sample", updater = TRUE)
#'
#' ## Subset only data from Lees Ferry
#' foo2 <- foo[foo$Reach == "CRLeesFerry",]
#'
#' ## Get the specimen data for these samples, all wrapped together and formatted nicely.
#' foo3 <- foodbase(samp = foo2)
#'
#' ## Or, if you don't like shiny things, get the raw specimen data only.
#' fugly <- readDB(type = "Specimen")
#'
#' ## Work with existing drift sample data, with no update (all the defaults).
#' loc <- readDB()

#' @author Jeffrey D. Muehlbauer, \email{jmuehlbauer@usgs.gov} and Michael J. Dodrill, \email{mdodrill@usgs.gov}

#' @export

## Function call
readDB <- function(gear = "Drift", type = "Sample", updater = FALSE){

## Check if the Data folder of foodbase package install location exists
dbdir <- paste0(find.package('foodbase'),'/Data')
dbdir.exists <- dir.exists(path = dbdir)

## Update/add data as necessary
if(dbdir.exists == FALSE | updater != FALSE){
	if(updater == 'GitHub'){
		path <- paste0('https://raw.githubusercontent.com/jmuehlbauer-usgs/Database/master/')		
	} else{
		path <- paste0('P:/BIOLOGICAL/Foodbase/Database/Exports/')	
	} 	
	dir.create(dbdir, showWarnings = FALSE)
	samp <- read.csv(paste0(path, gear, 'Sample.csv'))
	spec <- read.csv(paste0(path, gear, 'Specimen.csv'))
	sppl <- read.csv(paste0(path, 'SppList.csv'))
	write.csv(samp,paste0(dbdir,'/', gear, 'Sample.csv'), row.names = FALSE)
	write.csv(spec,paste0(dbdir,'/', gear, 'Specimen.csv'), row.names = FALSE)
	write.csv(sppl,paste0(dbdir,'/', 'SppList.csv'), row.names = FALSE)
}

## Read in the data
if(type == 'SppList'){
dat <- read.csv(paste0(dbdir, '/SppList.csv'))
} else{
dat <- read.csv(paste0(dbdir, '/', gear, type, '.csv'))
}
return(dat)

## Close function
}
