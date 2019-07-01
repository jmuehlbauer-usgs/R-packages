packload <- function(packages, updater = FALSE, quiet = TRUE){
	## Check for currently installed packages
	packs <- vector()
	mypacks <- rownames(installed.packages())
	for(i in 1 : length(packages)){
		if(!(packages[i] %in% mypacks)){
			packs <- c(packs, packages[i])
		}
	}
	## Install new packages
	if(length(packs) > 0){
		avl <- rownames(available.packages())
		for(i in 1 : length(packs)){
			## Install from CRAN
			if(packs[i] %in% avl){
				install.packages(packs[i], quiet = quiet)
			}
			## Install from GitHub jmuehlbauer/R-packages repository
			else{
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
				update.packages(packs2[i], quiet = quiet)
			}
			else{
				if(!('devtools' %in% rownames(installed.packages()))){
					install.packages('devtools', quiet = quiet)
				}else{
					update.packages('devtools', quiet = quiet)
				}
				require(devtools)
				repo <- 'jmuehlbauer-usgs/R-packages'
				tryit <- try(suppressWarnings(install_github(repo = repo, 
					subdir = packs2[i], quiet = quiet, force = TRUE)), silent = TRUE)
			}
		}
	}
	## Load all these packages
	loaded <- lapply(packages, require, quietly = quiet, character.only = TRUE)
}
packload(c('lme4', 'foodbase'), updater = TRUE)