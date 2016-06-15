#' @title Plot a fitted curve to data

#' @description Fit a \code{loess} or \code{lowess} line to data and overlay the resultant fitted line on a plot.

#' @param yvar The \code{y}, or dependent variable data to be fitted.
#' @param xvar The \code{x}, or independent variable data to be fitted.
#' @param span The smoother span for the line. Larger values produce smoother lines. Default is \code{0.75}. 
#' @param method the method for creating a fitted curve, either \code{loess} or \code{lowess}. Defaults to \code{loess}.
#' @param ... Additional arguments to be passed on to \code{lines} or \code{loess}.

#' @details
#' The function can accomodate several additional (\code{...}) variables for plotting the line, including \code{lwd}, \code{lty}, \code{col}, etc. Additional variables to pass to \code{loess} are also possible, including \code{family}, \code{degree}, and \code{parametric}, but note the function's behavior and output may be erratic when using these terms.

#' @section Warnings:
#' A plot must already have been created prior to running this function in order for it to plot the line.
#'
#' Although additional arguments can be passed to \code{loess}, no additional arguments can be passed to \code{lowess}, which is older and less flexible anyway.

#' @return Output is a line of the fitted curve, overlaid on an existing plot.

#' @seealso \code{\link{loess}}, \code{\link{lowess}}, \code{\link{lines}}, which this function wraps.

#' @concept plot, plotting, line, loess, lowess

#' @examples
#' ## Create a basic plot
#' ys<-c(1:10)
#' xs<-c(1,1,2,2,4,3,6,6,7,8)
#' plot(ys~xs)
#'
#' ## Fit a loess line to this plot, with a red line 
#' lineLoess(ys,xs,col=2)

#' @author Jeffrey D. Muehlbauer, \email{jmuehlbauer@usgs.gov}. With content from Charles B. Yackulic and Theodore A. Kennedy.

#' @export

## Function call
lineLoess<-function(yvar,xvar,span=.75,method='loess',...){
	rng<-c(round(min(xvar,na.rm=T)):round(max(xvar,na.rm=T)))
	if(method=='loess'){
		pred<-predict(loess(yvar~xvar,span=span,...),rng)
		suppressWarnings(lines(pred~rng,...))
	}
	if(method=='lowess'){
		suppressWarnings(lines(lowess(xvar,yvar,f=span),...))
	}
 }
 