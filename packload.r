##### packload function for installing, updating, and loading packages #####
	## Last updated 21 October 2020 by J.D. Muehlbauer
	
## Function checks if a package is currently installed on machine.
	## If not, it installs it (via CRAN or else jmuehlbauer-usgs/R-packages).
	## Also has an updater function to update the packages if requested.
	## Loads all packages fed to the function too.
	## The quiet argument does all this as quietly as possible.
		
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