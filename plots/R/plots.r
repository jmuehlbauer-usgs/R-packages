#' @title Save plots in various filetypes.

#' @description Allows plots to be saved as a variety of filetypes, or to save the same plot under multiple filetypes.

#' @param plotfx All the plotting commands to make the desired plot, wrapped as a function.
#' @param filename The desired filename of the output plot file.
#' @param directory If specified, the directory where the figure is to be saved.
#' @param filetype Filetypes to save. Possibilities include \code{pdf}, \code{png}, \code{jpeg}, \code{tiff}, \code{bmp}, and \code{eps}, or any combination therein. Defaults to \code{pdf}.
#' @param width Width of the figure. Defaults to \code{6.5}.
#' @param height Height of the figure. Defaults to \code{9}.
#' @param units Units on the width and height measurements (if relevant). Possibilities include \code{in}, \code{px}, \code{cm}, or \code{mm}. Defaults to \code{in}.
#' @param res Resolution of the figure (if relevant). Defaults to \code{600} dpi.
#' @param colormodel Specifies the color model (if relevant). Possibilities include (\code{srgb}, \code{gray}, or \code{cmyk}). Defaults to \code{srgb}.

#' @details
#' The \code{filename} argument should exclude the filetype from its name (e.g., type \code{"Fig1"}, not \code{"Fig1.pdf"}).
#'
#' If \code{directory} is not specified, files are saved to the working directory. If desired, filepaths can also be specified in the \code{filename} argument. For example [\code{filename = "PlotsFolder/Fig1"}] and [\code{filename = "Fig1", directory = "PlotsFolder"}] are equivalent.
#'
#' Plots can be saved as more than one filetype using, e.g., \code{filetype = c("pdf", "jpeg")}.

#' @section Warning:
#' The argument \code{res} is meaningless for \code{pdf} and \code{eps} filetypes, which are vector graphics (no native resolution). Similarly, using \code{units = "px"} is meaningless for \code{pdf} and \code{eps} filetypes; if pixel units are set, the output for these filetypes will be in units of inches. Finally, \code{colormodel} only applies to \code{pdf} and \code{eps} formats; other filetypes lack this functionality.

#' @return Creates and saves files of the plot under the desired filetypes.

#' @seealso \code{\link{pdf}}, \code{\link{png}}, \code{\link{jpeg}}, \code{\link{tiff}}, \code{\link{bmp}}, and \code{\link{setEPS}}, which this function wraps.

#' @concept plot, plotting, save

#' @examples
#' ## Wrap the desired plot commands as a function
#' foo<-function(){plot(0,0,main='Hiya!')}
#'
#' ## Save the plot to the working directory with the filename "HelloWorld" as a pdf and png file 
#' plotTypes(foo,'HelloWorld',filetype=c('pdf','png'))

#' @author Jeffrey D. Muehlbauer, \email{jmuehlbauer@usgs.gov}

#' @export

## Function call
plotTypes<-function(plotfx,filename,directory='',filetype=c('pdf'),width=6.5,height=9,units='in',res=600,colormodel='srgb'){

	## Create some variables for special cases
	directory2<-ifelse(directory=='',directory,paste(directory,'/',sep=''))	
	width2<-ifelse(units=='cm',width*.393701,ifelse(units=='mm',width*.0393701,width))
	height2<-ifelse(units=='cm',height*.393701,ifelse(units=='mm',height*.0393701,height))

	## Create pdf
	if('pdf'%in%filetype){
		pdf(paste0(directory2,filename,'.pdf'),width=width2,height=height2,colormodel=colormodel)
		plotfx()
		dev.off()
	}

	## Create png
	if('png'%in%filetype){
		png(paste0(directory2,filename,'.png'),width=width,height=height,units=units,res=res)
		plotfx()
		dev.off()
	}

	## Create jpeg
	if('jpeg'%in%filetype){
		jpeg(paste0(directory2,filename,'.jpg'),width=width,height=height,units=units,res=res)
		plotfx()
		dev.off()
	}

	## Create tiff
	if('tiff'%in%filetype){
		tiff(paste0(directory2,filename,'.tif'),width=width,height=height,units=units,res=res,compression='lzw')
		plotfx()
		dev.off()
	}

	## Create bmp
	if('bmp'%in%filetype){
		bmp(paste0(directory2,filename,'.bmp'),width=width,height=height,units=units,res=res)
		plotfx()
		dev.off()
	}

	## Create eps
	if('eps'%in%filetype){
		setEPS()
		postscript(paste0(directory2,filename,'.eps'),width=width2,height=height2,colormodel=colormodel)
		plotfx()
		dev.off()	
	}

	## Close function
}
