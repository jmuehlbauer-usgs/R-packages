# :ocean: GCgage
**Download data from Grand Canyon area gages**

Description: This packages contains functions for downloading stream gage data available on the US Geological Survey Grand Canyon Monitoring and Research Center [website](https://www.gcmrc.gov/discharge_qw_sediment/stations/GCDAMP). It also performs basic manipulations on these data for working in `R`.

__Installation instructions:__  
In `R`, paste the following lines of code:  
`require(devtools)`  
`install_github(repo = "jmuehlbauer-usgs/R-packages", subdir = "GCgage")`  
Note that if `devtools` is not already installed, you will need to install that first (`install.packages("devtools")`).

__Current list of functions:__  
__readGage__: Downloads and formats gage data for use in `R`.  
__gages__: A reference list of gage names and other attributes.  
__gageVars__: A reference list of gage parameters.

__Disclaimer:__  
This software is in the public domain because it contains materials that originally came from the U.S. Geological Survey (USGS), an agency of the United States Department of Interior. For more information, see the official USGS [copyright policy](https://www.usgs.gov/visual-id/credit_usgs.html#copyright). Although this software program has been used by the USGS, no warranty, expressed or implied, is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the USGS in connection therewith. This software is provided "AS IS."
