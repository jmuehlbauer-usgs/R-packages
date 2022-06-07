# R-packages
**Custom packages and functions for R**

These packages are created principally for use by my research group and colleagues, specifically at the [USGS Grand Canyon Monitoring and Research Center](https://www.usgs.gov/centers/sbsc/about/gcmrc), to facilitate common `R` tasks that we carry out with some regularity.  
  
__Current list of packages:__  
__plots__: Convenience functions for common plotting operations  
__bugR__: Functions for common ecological analyses of invertebrate data  
__foodbase__: Functions for reading and working with Foodbase data  
__GCgage__: Functions for downloading data from Grand Canyon area gages.   
__packload__: Function for more easily installing, updating, and loading packages.   
__trueAIC__: Function for comparing AICs from log-transformed response models vs. similar linear versions.   

The `trueAIC` function is currently only available as a sourceable function (not embedded in a package). The `packload` function is sourceable too but is also embedded within the [`foodbase`](https://github.com/jmuehlbauer-usgs/R-packages/tree/main/foodbase) package. Sourceable functions can be accessed in R using the following code (replace FUNCTIONNAME with the name of the function of interest):
`source('https://github.com/jmuehlbauer-usgs/R-packages/blob/main/FUNCTIONNAME.r?raw=TRUE')`

There is also some How-To code that explains how building and installing these packages from GitHub works.  

Improvements, additions, and suggestions are [welcomed](mailto:jdmuehlbauer@alaska.edu).  

__Disclaimer:__  
This software is in the public domain because it contains materials that originally came from the U.S. Geological Survey (USGS), an agency of the United States Department of Interior. For more information, see the [official USGS copyright policy](https://www.usgs.gov/information-policies-and-instructions/copyrights-and-credits). Although this software program has been used by the USGS, no warranty, expressed or implied, is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the USGS in connection therewith. This software is provided "AS IS."