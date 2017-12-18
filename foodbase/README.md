# foodbase
Functions for Reading and Working with Foodbase Data

This packages contains some functions for importing and working with data from the Foodbase database. Currently only implemented for Drift data, with support for other gear types hopefully coming soon!

Current list of functions:  
readDB: Pulls exported data from the Foodbase database for use in R.  
sampspec: Pulls exported data from the Foodbase database for use in R, combines Sample, Specimen, and Species code data, and formats the data to facilitate analysis.  
sampstats: Runs some simple statistics on formatted data sets from the Foodbase database.  
ordmat: Takes data from sampspec and builds ordination-friendly matrices of counts, mean/median/SD sizes, or biomasses, by taxon and sample.  
