##### Create an R package #####
## Last updated 14 June 2016 by J. D. Muehlbauer

##### Set working directory, create folder, load libraries

## Set working directory (local)
setwd('C:/Users/jmuehlbauer/Documents/R/Custom')

## Load requisite libraries
if('devtools' %in% rownames(installed.packages())==FALSE){install.packages('devtools')}
if('roxygen2' %in% rownames(installed.packages())==FALSE){install.packages('roxygen2')}
require(devtools)
require(roxygen2)

## Create folder for package
create('TEST')


##### Write functions #####

## In the folder just created (above), go to the R folder, and add any functions (as files with no filetype) you wish to include in the library.
	## Manipulate the header content of each function to include parameters, info, examples, etc. For example (below verbatim, including hashtags):

#' A basic function
#'
#' This is a test function I wrote.
#' @param test Tests if the function is working. Defaults to TRUE.
#' @examples test()
#' testfx() testfx<-function(test=TRUE){if(test==TRUE){print('It works!')}}

## Create documentation
setwd('./TEST')
document()


##### Install the package #####

## Install to the local directory
setwd('..')
install('TEST')


##### Set up the local repository using GitBASH #####

## Create a local Git repository
	## Use the following commands verbatim in GitBASH (hint: paste in BASH is Shift+Insert):
cd "C:/Users/jmuehlbauer/Documents/R/Custom"
git init
git add TEST/
git commit -m "Initial commit"

## Push to a GitHub repository
git remote add origin https://github.com/jmuehlbauer-usgs/R-packages.git
git pull origin master
git push origin master
## Other users can install the package from GitHub (once it's posted):
install_github(repo='jmuehlbauer-usgs/R-packages',subdir='plots')
