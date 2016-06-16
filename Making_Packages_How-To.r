##### Create an R package #####

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

## In the folder just created (above), go to the R folder, and add any functions (as files with no filetype) you wish to include in the library. Call the example below "testfx.r"
	## Manipulate the header content of each function to include parameters, info, examples, etc. For example (below verbatim, including hashtags):

#' @title A basic function
#' @description This is a test function I wrote.
#' @param test Tests if the function is working. Defaults to TRUE.
#' @examples test()
#' @export
testfx<-function(test=TRUE){
	if(test==TRUE){print('It works!')}
	else{'Hey, it still works!'}
	}

## Create documentation
setwd('./TEST')
document()


##### Install the package #####

## Install to the local directory and try it!
setwd('..')
install('TEST')
library(TEST)
testfx()


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
git commit -m "Merging with GitHub"
git push origin master


##### Download and install the package from GitHUB #####

## Other users can now install the package from GitHub:
install_github(repo='jmuehlbauer-usgs/R-packages',subdir='TEST')
