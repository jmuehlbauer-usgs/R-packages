#' @title Rotate ordination axes.

#' @description Rotate a 2D ordination to line up an environmental variable from \code{vegan}'s \code{envfit} with an axis.

#' @param ord The ordination output points dataframe from \code{\link{NMS}}.
#' @param env.fit An \code{\link{envfit}} result object.
#' @param env.vector The environmental vector to rotate the ordination about (must be one of the vector names from \code{env.fit}).
#' @param x.axis Specifies whether to rotate the ordination along the \code{x} (\code{TRUE}) or \code{y} (\code{FALSE}) axis. Default is \code{TRUE}.
#' @param positive Specifies whether to rotate so that the environmental variable is positively (\code{TRUE}) or inversely (\code{FALSE}) related to the axis.  Default is \code{TRUE}.
#' @param flip Allows the ordination to be flipped 180 degrees around the other axis.  Default is \code{FALSE}.

#' @details
#' This function allows ordination rotation around axes. It is strictly a tool for visual interpretation of the data, with a common practice being to load the variation for the environmental variable of primary interest entirely onto the primary ordination axis. Using this function will not affect the total \eqn{R^{2}}{R^2} of the ordination; however, it will redistribute the variation across the 2 axes. It will also affect the \eqn{R^{2}}{R^2} values of environmental variables overlaid on each axis. Thus, good practice after running this function would be to re-run \code{\link{axisR2}} and \code{\link{envfit}} on these new, rotated points data to generate new \eqn{R^{2}}{R^2} values for axes and environmental variables on the ordination.
#'
#' This function only works for ordinations in 2 dimensions.

#' @return A dataframe in the same form as \code{ord}, with new values generated from the rotated axes.

#' @seealso \code{\link{NMS}}, which runs ordinations on data, and \code{\link{axisR2}} for the \eqn{R^{2}}{R^2} values of the axes. This function requires and builds upon functionality in \code{\link{envfit}} from package \code{vegan}.

#' @concept relativization, ordination, variation, rotation

#' @examples
#' ## Run envfit on some simple environmental data, using the 2-dimensional, NMSPoints2D file (pts2D) output by the NMS function
#' efits <- envfit(pts2D, evar)
#'
#' ## Rotate axis so the temperature variable loads entirely on Axis 1
#' rot <- ordRotate(pts2D, efits, 'Temperature')
#' ## Compare rotated ordination to original
#' par(mfrow = c(2, 1))
#' plot(pts2D, main = "Original")
#' plot(rot, main = "Rotated")

#' @author Jeffrey D. Muehlbauer, \email{jmuehlbauer@usgs.gov}.

#' @export

## Function call
ordRotate<-function(ord,env.fit,env.vector,x.axis=TRUE,positive=TRUE,flip=FALSE){
additive<-ifelse(x.axis==TRUE,0,pi/2)
ax<-ifelse(x.axis==TRUE,2,1)
pos<-ifelse(positive==TRUE,0,pi)
flp<-ifelse(flip==TRUE,-1,1)
vect.num<-match(env.vector,rownames(env.fit$vectors$arrows))
theta<-(additive-atan(env.fit$vectors$arrows[vect.num,2]/env.fit$vectors$arrows[vect.num,1]))+pos
rotate<-cbind(ord[,1]*cos(theta)-ord[,2]*sin(theta),ord[,2]*cos(theta)+ord[,1]*sin(theta))
	rownames(rotate)<-rownames(ord)
	colnames(rotate)<-colnames(ord)
	rotate[,ax]<-rotate[,ax]*flp
as.data.frame(rotate)
}
