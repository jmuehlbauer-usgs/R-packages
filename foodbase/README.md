# :pizza: foodbase
**Functions for Reading and Working with Foodbase Data**

This packages contains some functions for importing and working with data from the Foodbase database. 

__Installation instructions:__  
In `R`, paste the following lines of code:  
`require(devtools)`  
`install_github(repo = "jmuehlbauer-usgs/R-packages", subdir = "foodbase")`  
Note that if `devtools` is not already installed, you will need to install that first (`install.packages("devtools")`).

__Current list of functions:__  
__readDB__: Pulls exported data from the Foodbase database for use in `R`.  
__sampspec__: Pulls exported data from the Foodbase database for use in `R`, combines Sample, Specimen, and Species code data, and formats the data to facilitate analysis.  
__sampstats__: Runs some simple statistics on formatted data sets from the Foodbase database.  
__ordmat__: Takes data from sampspec and builds ordination-friendly matrices of counts, mean/median/SD sizes, or biomasses, by taxon and sample.

__Disclaimer:__  
This software is in the public domain because it contains materials that originally came from the U.S. Geological Survey (USGS), an agency of the United States Department of Interior. For more information, see the official USGS [copyright policy](https://www.usgs.gov/visual-id/credit_usgs.html#copyright). Although this software program has been used by the USGS, no warranty, expressed or implied, is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the USGS in connection therewith. This software is provided "AS IS."
