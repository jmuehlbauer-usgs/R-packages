##### packload function for installing, updating, and loading packages #####
	## Last updated 21 October 2020 by J.D. Muehlbauer
	
## Function checks if a package is currently installed on machine.
	## If not, it installs it (via CRAN or else jmuehlbauer-usgs/R-packages).
	## Also has an updater function to update the packages if requested.
	## Loads all packages fed to the function too.
	## The quiet argument does all this as quietly as possible.

#' @title Install, update, and load packages

#' @description Function to check if specified R packages are currently 
#' installed on the user's machine. It installs them if not, updates 
#' them if selected, and then loads them. 

#' @param packages The names of the R packages to be loaded. Specified 
#'   with quotes.
#' @param updater Whether to search for and istall any updates to packages,
#'   or just load existing packages (assuming they are already installed).
#'   Any packages specified that are not already installed will be 
#'   installed with the latest versions regardless. Default is \code{FALSE}.
#' @param quiet Whether to load packages with as little commentary as 
#'   possible. Default is \code{TRUE}.

#' @details
#'
#' The function checks for packages located on CRAN and on Jeff Muehlbauer's
#' GitHub space (\link{https://github.com/jmuehlbauer-usgs/R-packages}). 
#' In general specifying \code{updater = TRUE} is unnecessary unless you know a big 
#' an R package has recently been developed. The \code{quiet} argument tries its
#' best to prevent R's printing of package loading commentary, but some text may 
#' still get through.
#'
#' @return Loads the packages of interest, for use in the R workspace.

#' @concept packages

#' @examples
#' # Load Jeff's "bugR" package and the CRAN package "lubridate".
#' packload(c("bugR", "lubridate"))

#' @author Jeffrey D. Muehlbauer, \email{jmuehlbauer@usgs.gov}

#' @export

## Function call	
packload <- function(packages, updater = FALSE, quiet = TRUE){
	## Set system environment and download options for GitHub installs to work
	Sys.setenv(TAR = 'internal')
	options(download.file.method = "wininet")
	## Check for currently installed packages
	packs <- vector()
	mypacks <- rownames(installed.packages())
	for(i in 1 : length(packages)){
		if(!(packages[i] %in% mypacks)){
			packs <- c(packs, packages[i])
		}
	}
	## Specify CRAN mirror if not already done
	if(is.null(getOption('repos'))){
		repos <- "https://cran.cnr.berkeley.edu/" 
	} else{
		repos <- getOption('repos')
	}
	## Install new packages
	if(length(packs) > 0){
		avl <- rownames(available.packages())
		for(i in 1 : length(packs)){
			## Install from CRAN
			if(packs[i] %in% avl){
				install.packages(packs[i], quiet = quiet, repos = repos)
			## Install from GitHub jmuehlbauer/R-packages repository
			} else{
				## Make sure devtools is installed first
				if(!('devtools' %in% mypacks)){
					install.packages('devtools', quiet = quiet)
				}
				require(devtools)
				repo <- 'jmuehlbauer-usgs/R-packages'
				tryit <- try(suppressWarnings(install_github(repo = repo, 
					subdir = packs[i], quiet = quiet)), silent = TRUE)
			}
		}
	}
	## Update packages if requested. Performs many of the same tasks as above
	if(updater == TRUE){
		packs2 = packages[packages %in% packs == FALSE]
		if(exists('avl') == FALSE){
			avl <- rownames(available.packages())
		}
		for(i in 1 : length(packs2)){
			if(packs2[i] %in% avl){
				update.packages(packs2[i], quiet = quiet, repos = repos)
			} else{
				if(!('devtools' %in% rownames(installed.packages()))){
					install.packages('devtools', quiet = quiet)
				} else{
					update.packages('devtools', quiet = quiet)
				}
				require(devtools)
				repo <- 'jmuehlbauer-usgs/R-packages'
				tryit <- try(suppressWarnings(install_github(repo = repo, 
					subdir = packs2[i], quiet = quiet, force = TRUE)), silent = TRUE)
			}
		}
	}
	## See if any packages are already loaded
	unloaded <- packages[!packages %in% (.packages())]
	## Load unloaded packages (or all packages if updater = TRUE)
	if(updater == TRUE){
		loaded <- lapply(packages, require, quietly = quiet, 
			warn.conflicts = !quiet, character.only = TRUE)
	} else {
		if(quiet == TRUE){
			loaded <- suppressWarnings(lapply(unloaded, require, quietly = quiet,
				warn.conflicts = !quiet, character.only = TRUE))
		} else{
			loaded <- lapply(unloaded, require, quietly = quiet,
				warn.conflicts = !quiet, character.only = TRUE)
		}
	}
}