#' @title Read in sample and specimen data from the Foodbase database.

#' @description Pulls exported data from the Foodbase database for use in R.

#' @param gear The sampling gear type of interest (\code{Drift},
#'   \code{LightTrap}, \code{FishGut}). Default is \code{Drift}.
#' @param type Whether to download \code{Sample}, \code{Specimen} or
#'   \code{SpeciesList} (the master species list) data. Default is
#'   \code{Sample}.
#' @param updater Whether to download new versions of the Sample, Specimen, and
#'   Species List data, and from what source. See Details. Default is
#'   \code{FALSE}.

#' @details Currently \code{Drift}, \code{LightTrap}, \code{Sticky}, and \code{FishGut} are implemented for
#' \code{gear}.
#'
#' The \code{type} argument specifies whether to return \code{Sample} data
#' (i.e., sample collection information), \code{Specimen} data (i.e., bug counts
#' and sizes, where relevant), or \code{SpeciesList} data (i.e., the master
#' species list). If you are interested in working with both \code{Sample} and
#' \code{Specimen} data, then using this function to get the \code{Sample} data,
#' filtering to only the data of interest, then running the
#' \code{\link{sampspec}} function will be faster and more useful (see
#' Examples).
#'
#' Regardless of the \code{updater} setting, \code{readDB} checks for a local
#' copy of the \code{Sample}, \code{Specimen}, and \code{SpeciesList} data in
#' the \code{Data} folder of the \code{foodbase} package, and will install these
#' three files there if they are not present (these local copies are used for
#' other functions within the \code{foodbase} package). Using \code{updater}
#' will update all three of these files, regardless of what data type is
#' specified by \code{type}.
#'
#' If not set to \code{FALSE}, the \code{updater} can be set to download new
#' data from the \code{Network} or from \code{GitLab}. However, it's generally
#' better to specify only \code{updater = TRUE}, in which case R will check to
#' see if you are connected to the DOI network and will download data from the
#' Network if so and from GitLab if not. Although data are pushed to GitLab
#' nightly, the Network data are almost always the most current.

#' @return Creates a dataframe containing the desired data from the Foodbase database.

#' @seealso \code{\link{sampspec}}, which provides more powerful formatting for
#'   working with data from the Foodbase database.

#' @concept access, database

#' @examples
#' ## Read in drift sample data from the network.
#' foo <- readDB(gear = "Drift", type = "Sample", updater = TRUE)
#'
#' ## Subset only data from Lees Ferry
#' foo2 <- foo[foo$Reach == "CRLeesFerry",]
#'
#' ## Get the specimen data for these samples, all wrapped together and formatted nicely.
#' foo3 <- sampspec(samp = foo2)
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

## Interpret gear for non-accepted cases
if(!(gear %in% c('Drift', 'FishGut', 'LightTrap', 'Sticky', 'Benthic'))){
	gear1 <- toupper(substr(gear, 1, 1))
	if(gear1 %in% c('D', 'F', 'L', 'S', 'B')){
		gear2 <- ifelse(gear1 == 'D', 'Drift',
			ifelse(gear1 == 'F', 'FishGut',
			ifelse(gear1 == 'L', 'LightTrap',
			ifelse(gear1 == 'S', 'Sticky', 'Benthic'))))
		warning(paste0('Invalid gear argument ("', gear, '"). Converted to "', gear2, '".'))
		gear <- gear2
	} else {
		message(paste0('Invalid gear argument ("', gear, '"). Please correct.'))
	}
}

## Check if the Data folder of foodbase package install location exists
dbdir <- paste0(find.package('foodbase'),'/Data')
dbdir.exists <- dir.exists(path = dbdir)

## Update/add data as necessary
netpath <- paste0('P:/BIOLOGICAL/Foodbase/Database/Exports/')
if(dbdir.exists == FALSE){dir.create(dbdir, showWarnings = FALSE)}
files <- paste0(c(paste0(gear, c('Sample', 'Specimen')), 'SpeciesList'), '.csv')
if(FALSE %in% (files %in% list.files(dbdir)) | updater != FALSE){
	netcheck <- length(grep('gs.doi.net', system('ipconfig', intern = TRUE)))
	if(updater == 'GitLab' | dir.exists(netpath) == FALSE){
		gitpath1 <- 'https://code.usgs.gov/api/v4/projects/5233/repository/files/'
		gitpath2 <- '/raw?ref=master&private_token=om_PQCgBNziJf-JFNdhD'
		lapply(files, function(x){download.file(paste0(gitpath1, x, gitpath2), paste0(dbdir, '/', x))})
    } else {
		file.copy(paste0(netpath, files), paste0(dbdir,'/'), overwrite = TRUE, copy.date = TRUE)
    }
}

## Read in the data
if(type == 'SpeciesList'){
	dat <- read.csv(paste0(dbdir, '/SpeciesList.csv'), stringsAsFactors = FALSE)
} else {
	dat <- read.csv(paste0(dbdir, '/', gear, type, '.csv'), stringsAsFactors = FALSE)
	if(type == 'Sample'){
		dat$Date <- as.Date(dat$Date, format = '%m/%d/%Y')
		dat$ProcessDate <- as.Date(dat$ProcessDate, format = '%m/%d/%Y')
	}
}
attr(dat, 'gear') <- gear
samp1 <- file.mtime(paste0(dbdir, '/', gear, 'Sample.csv'))
spec1 <- file.mtime(paste0(dbdir, '/', gear, 'Specimen.csv'))
sppl1 <- file.mtime(paste0(dbdir, '/SpeciesList.csv'))
cat(paste0('Data obtained. Dates last modified/imported:\n', gear, ' Samples: ', samp1,
	'\n', gear, ' Specimens: ', spec1, '\nSpecies List: ', sppl1, '\n'))
return(dat)
}