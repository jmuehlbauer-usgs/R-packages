# foodbase
**Functions for Reading and Working with Foodbase Data**

This packages contains some functions for importing and working with data from the Foodbase database. Currently only implemented for Drift and Fish Guts data.

__Current list of functions:__  
__readDB__: Pulls exported data from the Foodbase database for use in R.  
__sampspec__: Pulls exported data from the Foodbase database for use in R, combines Sample, Specimen, and Species code data, and formats the data to facilitate analysis.  
__sampstats__: Runs some simple statistics on formatted data sets from the Foodbase database.  
__ordmat__: Takes data from sampspec and builds ordination-friendly matrices of counts, mean/median/SD sizes, or biomasses, by taxon and sample.  
