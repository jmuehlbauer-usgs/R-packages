#' @title Relativize a dataset.

#' @description Relativize a dataset by columns (species) or rows (samples).

#' @param Data A matrix containing only numeric data.
#' @param rel.by Specifies whether to relativize by column, by row, or both. Defaults to \code{"col"}.

#' @details
#' Data relativization is generally done prior to running an ordination (using function \code{\link{NMS}}, for example). Relativizing by species (columns), is particularly common, with relativization by samples (rows), somewhat less so. In general relativization should reduce the row and column coefficients of variation (\code{\link{cv}}) of the dataset substantially in order to be worth doing. See McCune and Grace (2002) for additional detail.
#'
#' The matrix specified in \code{Data} must contain only numeric values (i.e., no rows or columns of factors, site or species names, etc.) Site and species names may be included as row and column names, however.
#'
#' Relativization can be done by rows (\code{rel.by = "row"}, columns (\code{rel.by = "col"}), or both (\code{rel.by = c("row", "col")} or \code{rel.by = 'both'}).

#' @return A dataframe containing relativized values for further ordination analysis.

#' @seealso \code{\link{NMS}}, which runs ordinations on data, and \code{\link{cv}} for checking the cv of the rows and columns in the dataset.

#' @concept relativization, ordination, coefficient of variation

#' @examples
#' ## Relativize some simple data by species maximum
#' ordrel <- rel(ordmat, rel.by = "col")

#' @author Jeffrey D. Muehlbauer, \email{jmuehlbauer@usgs.gov}. Based on content from Dean Urban and Sarah Goslee.

#' @export

## Function call
rel<-function(Data,rel.by="col"){
if('col' %in% rel.by || 'both' %in% rel.by){
	cmax<-apply(Data,2,max)
	cmax[cmax==0]<-1
	Data<-sweep(Data,2,cmax,"/")
}
if('row' %in% rel.by || 'both' %in% rel.by){
	rsum<-apply(Data,1,sum)
	rsum[rsum==0]<- 1
	Data<-sweep(Data,1,rsum,"/")
}
Data	
}
