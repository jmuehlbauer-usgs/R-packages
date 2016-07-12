#' @title Remove rare species from a dataset.

#' @description Deletes rare species from a dataset based on specified criteria for defining rarity.

#' @param Data A matrix containing only numeric data.
#' @param filt Specifies the method for determining rare species. Default is percent (\code{"perc"}).
#' @param level The level at which species are considered rare. Default is \code{0.05}.

#' @details
#' Deleting rare species from a dataset is common prior to ordination, as such species often have a large effect on ordination results in spite of generally having little biological effect within the samples or communities in which they are found. A common practice is to remove species occuring in less than 5\% of samples (the defaults for this function). Deleting rare species also commonly has the effect of reducing the \code{\link{cv}} of the dataset. If the dataset is to be also relativized (using, e.g., \code{\link{rel}}), this should be done after removing the rare species, not before. See McCune and Grace (2002) for additional detail.
#'
#' The matrix specified in \code{Data} must contain only numeric values (i.e., no rows or columns of factors, site or species names, etc.) Site and species names may be included as row and column names, however.
#'
#' The argument \code{filt} allows rare species determinations to be made according to their presence in a set percentage of samples (\code{"perc"}), in a set number of samples (\code{"num"}), or by their total abundance across all samples (\code{"abund"})
#'
#' The argument \code{level} should be a proportion (\code{0 < level < 1}) if deleting rare species by the percent of samples in which they appear. Conversely, it should be an integer if deleting rare species by the actual number of samples in which they appear, or by their total abundance across samples.

#' @return A dataframe in the same format as the original data. Any rare species (columns) deleted by the function will be removed from this resultant dataset, as will any samples (rows) that are empty after rare species column removal.

#' @seealso \code{\link{NMS}}, which runs ordinations on data, \code{\link{rel}} for relativizing by rows or columns in the dataset, and \code{\link{cv}} for assessing the coefficients of variaiton of the rows and columns of a dataset.

#' @concept relativization, ordination, coefficient of variation, rarity

#' @examples
#' ## Delete species whose total abundance is < 100 from a simple data matrix
#' drmat <- delRare(ordmat, filt = "abund", level = 100)

#' @author Jeffrey D. Muehlbauer, \email{jmuehlbauer@usgs.gov}.

#' @export

## Function call
delRare<-function(Data,filt='perc',level=.05){
spp.total<-spp.presence<-vector()
for(i in 1:dim(Data)[2]){
	spp.presence[[i]]<-length(grep(TRUE,Data[,i]>0,value=FALSE))
	spp.total[[i]]<-sum(Data[,i])
}
if(filt=='perc'){cols<-grep(TRUE,spp.presence>dim(Data)[1]*level)}
if(filt=='num'){cols<-grep(TRUE,spp.presence>level)}
if(filt=='abund'){cols<-grep(TRUE,spp.total>level)}
new.x0<-Data[,c(cols)]
	new.x<-new.x0[grep(TRUE,rowSums(new.x0)>0),]
	new.x
}