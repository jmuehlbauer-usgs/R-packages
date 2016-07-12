#' @title Determine the cv of rows and columns in a dataset.

#' @description Computes separate coefficients of variation (cv) for rows and columns in a dataset.

#' @param Data A matrix containing only numeric data.

#' @details
#' Coefficients of variation suggest how amenable your dataset is to ordination. In general, cv < 100 is desired. If it is substantially higher than this, and if there is some biological rationale for doing so, relativization \code{\link{rel}} or deleting rare species \code{\link{delRare}} may be useful next steps in data processing. See McCune and Grace (2002) for additional detail.
#'
#' The matrix specified in \code{Data} must contain only numeric values (i.e., no rows or columns of factors, site or species names, etc.) Site and species names may be included as row and column names, however.

#' @return A dataframe containing the cv of the rows and columns.

#' @seealso \code{\link{NMS}}, which runs ordinations on data, \code{\link{rel}} for relativizing by rows or columns in the dataset, and \code{\link{delRare}} for deleting rare species.

#' @concept relativization, ordination, coefficient of variation

#' @examples
#' ## Compute cv on some simple data
#' cvord <- cv(ordmat)

#' @author Jeffrey D. Muehlbauer, \email{jmuehlbauer@usgs.gov}.

#' @export

## Function call
cv<-function(Data){
cvs<-as.data.frame(matrix(nrow=2,ncol=1))
	cvs[1,]<-round(100*sd(rowSums(Data))/mean(rowSums(Data)),2)
	cvs[2,]<-round(100*sd(colSums(Data))/mean(colSums(Data)),2)
	rownames(cvs)<-c('Row','Column')
	colnames(cvs)<-'CV (%)'
cvs	
}