#' @title Get the \eqn{R^{2}}{R^2} for ordination axes.

#' @description Compute percent of variation in ordination data explained by each ordination axis.

#' @param Data A matrix containing only numeric data.
#' @param ord The ordination output points dataframe from \code{\link{NMS}}.
#' @param dist.method Ecological distance metric to use. Default is Bray-Curtis dissimilarity (\code{"bray"}).

#' @details
#' This function only works for ordinations in 2 or 3 dimensions.
#'
#' The matrix specified in \code{Data} must contain only numeric values (i.e., no rows or columns of factors, site or species names, etc.) Site and species names may be included as row and column names, however.
#'
#' Generally \code{ord} will be the \code{"NMSPoints2D.csv"} or \code{"NMSPoints3D.csv"} file created by function \code{\link{NMS}} (see example). However, in theory any matrix of ordination points (generally with samples as rows and ordination axes as columns) should work.
#'
#' The argument \code{dist.method} passes its value on to \code{\link{vegdist}} in package \code{vegan} and accepts any of its possible values. These include \code{"bray"}, \code{"jaccard"}, \code{"mahalanobis"}, \code{"manhattan"}, \code{"euclidean"}, \code{"binomial"}, and others (see \code{\link{vegdist}} documentation).

#' @return A dataframe containing the axis \eqn{R^{2}}{R^2} values for the specified ordination.

#' @seealso \code{\link{NMS}}, which runs ordinations on data, and \code{\link{ordRotate}} for rotating ordination axes based on environmental variables. This function wraps and builds upon functionality in \code{\link{vegdist}} from package \code{vegan}.

#' @concept relativization, ordination, variation

#' @examples
#' ## Create some really simple, random data to look at in ordination space
#' foo <- matrix(sample(0:20,100,replace=T), ncol = 5, nrow = 20)
#'	colnames(foo) <- paste("Spp", 1:5, sep="")
#'	rownames(foo) <- paste("Site", 1:20, sep="")
#' 
#' ## Run an NMS
#' NMS(foo,only23=TRUE,stepdown=FALSE)
#'
#' ## Read in resultant 2D points file, and get axis R^2 values.
#' pts2D<-read.csv("NMS Output/NMSPoints2D.csv", row.names = 1, header = TRUE)
#' axisR2(foo, pts2D)

#' @author Jeffrey D. Muehlbauer, \email{jmuehlbauer@usgs.gov}.

#' @export

## Function call

axisR2<-function(Data,ord,dist.method='bray'){
dist.x<-vegdist(Data,method=dist.method)
	dist.ord1<-dist(ord[,1])
	dist.ord2<-dist(ord[,1:2])
		axis1<-mantel(dist.ord1,dist.x)$statistic
		axis2<-mantel(dist.ord2,dist.x)$statistic
			axis.r2.single<-rbind(axis1,(axis2-axis1))
				rownames(axis.r2.single)<-c('Axis1','Axis2')
				colnames(axis.r2.single)<-c('R^2')
					total.axis.r2<-axis.r2.single[[1]]+axis.r2.single[[2]]
						axis.r2<-round(rbind(total.axis.r2,axis.r2.single),5)
							rownames(axis.r2)<-c('Total','Axis1','Axis2')
	
if(dim(ord)[2]==3){
	dist.ord3<-dist(ord[,1:3])
		axis3<-mantel(dist.ord3,dist.x)$statistic
			axis.r2.single<-rbind(axis1,(axis2-axis1),(axis3-axis2))
				rownames(axis.r2.single)<-c('Axis1','Axis2','Axis3')
				colnames(axis.r2.single)<-c('R^2')
					total.axis.r2<-axis.r2.single[[1]]+axis.r2.single[[2]]+axis.r2.single[[3]]
						axis.r2<-round(rbind(total.axis.r2,axis.r2.single),5)
							rownames(axis.r2)<-c('Total','Axis1','Axis2','Axis3')
}
round(axis.r2,6)
}
