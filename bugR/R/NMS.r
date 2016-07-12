#' @title Non-metric multidimensional scaling ordination.

#' @description Run non-metric multidimensional scaling (NMS) ordinations, in several possible dimensions.

#' @param Data A matrix containing only numeric data.
#' @param maxruns The maximum number of random starts allowed to find a convergent ordination solution.  Default is \code{1000}.
#' @param only23 Should only 2D and 3D ordinations be run (\code{TRUE}), or should R ask if any other dimensions should be tried (\code{FALSE})?  Default is \code{FALSE}.  
#' @param stepdown Should a stepdown series of ordinations, from \code{6} to \code{1} axes, be run and plotted? Default is \code{TRUE}.
#' @param step.maxruns The number of maxruns used when creating the ordinations for the stepdown plot.  Default is \code{100}.
#' @param file.append If specified, appends a character(s) to the end of the code-generated filenames in the output ordination files.
#' @param cores Number of computer cores to parse the function onto. Defaults to \code{1} less than the computer's max.

#' @details
#' Under default settings, this function is rather interactive with the user, requiring input at multiple points. This is intended to allow the user to decide upon the ideal dimensions of the ordination to run after looking at a scree plot, and so on. If you know what you're doing and want instead to run and save an ordination with only a certain number of dimensions, you could specify \code{stepdown = FALSE} and \code{only23 = FALSE}. Note that the latter case is critical if you intend to run \code{NMS} as part of a batch of code; not setting \code{only23} to \code{FALSE} in that case will yield errors.
#'
#' The matrix specified in \code{Data} must contain only numeric values (i.e., no rows or columns of factors, site or species names, etc.) Site and species names may be included as row and column names, however.
#'
#' The argument \code{maxruns} corresponds to \code{trymax} in \code{vegan}'s \code{metaMDS} function.
#'
#' Any text appended to output filenames with \code{file.append} will appear as the last part of the filename, immediately before the \code{.csv} specifying the filetype. Appending such text is generally only useful if you intend to run and compare several competing ordinations (e.g., ordinations with or without rare species included, and so on).
#'
#' Theoretically, any number of \code{cores} can be specified. However, going above the number of cores present on your computer is generally nonsensical (although the function will still run in that scenario). Fully utilizing all of the computer's cores for this function also makes the computer unavaiable for other uses during the time \code{NMS} is running; thus, the default setting of using \code{1} less than the maximum nuber of available cores is advised.  

#' @return Depending on inputs, creates and saves a screeplot showing ordination stress by number of ordination dimensions. It also creates and saves dataframes containing species (columns) and points/sites (rows) ordination values, in the specified number of ordination dimensions. These are saved as \code{.png} and \code{.csv} files in a folder called \code{NMS Output} that the function creates in the working directory.

#' @seealso \code{\link{metaMDS}} in package \code{vegan}, on which this function depends. Pre-processing of the dataframe in preparation for running \code{NMS} may be accomplished using \code{\link{cv}}, \code{\link{delRare}}, and \code{\link{rel}}. Ueful post-processing tools include \code{\link{axisR2}}, and \code{\link{ordRotate}}, plus \code{\link{envfit}} in package \code{vegan}.

#' @concept NMS, MDS, NMDS, stress, ordination

#' @examples
#' ## Run an ordination on some simple data, with stepdown and all other defaults
#' NMS(ordmat)
#' ## Note the output that will be saved in the new "NMS Output" folder on the working directory.
#'
#' ## Plot the ordination 2D results
#' pts2D <- read.csv("NMS Output/NMSPoints2D.csv", row.names = 1, header = TRUE)
#' plot(pts2D)

#' @author Jeffrey D. Muehlbauer, \email{jmuehlbauer@usgs.gov}

#' @export

## Function call
NMS<-function(Data,maxruns=1000,step.maxruns=100,file.append='',stepdown=TRUE,only23=FALSE,cores=detectCores()-1)
{
## Create the output folder
dir.create('NMS Output',showWarnings=FALSE)

## Save datasheet so it can be run in parallel
write.csv(Data,paste('NMS Output/R_DataForNMS',file.append,'.csv',sep=''))
library(parallel,quietly=TRUE)

most<-{
if(stepdown==TRUE){
## Start with stepdown from 6 axes for preliminary analysis
run.NMS.stress<-function(X){
	suppressWarnings(library(vegan,quietly=TRUE))
	Data<-read.csv(paste('NMS Output/R_DataForNMS',file.append,'.csv',sep=''),header=T,row.names=1)
	vegan::metaMDS(Data,k=X,trymax=step.maxruns)
	}
cl.stress<-makeCluster(getOption("cl.cores",cores))
	MDS.stress<-parLapply(cl.stress,c(1:6),run.NMS.stress)
		stopCluster(cl.stress)

## Combine stress values from step down, make scree plot figure
step.stress<-sapply(c(1:6),function(x) MDS.stress[[x]]$stress)
	png(paste('NMS Output/ScreePlot',file.append,'.png',sep=''))
		plot(c(1:6),step.stress,xlab='# Axes',ylab='Stress')
		lines(c(1:6),step.stress)
		dev.off()
		
## Open the scree plot in the plotting window	
plot(c(1:6),step.stress,xlab='# Axes',ylab='Stress')
	lines(c(1:6),step.stress)
}
	
## Save 2D and 3D data, work from those rather than re-running MDS every time
run.NMS<-function(X){
	suppressWarnings(library(vegan,quietly=TRUE))
	Data<-read.csv(paste('NMS Output/R_DataForNMS',file.append,'.csv',sep=''),header=T,row.names=1)
	vegan::metaMDS(Data,k=X,trymax=maxruns,trace=0)
	}
cl<-makeCluster(getOption("cl.cores",cores))
	MDS<-parLapply(cl,c(2:3),run.NMS)
		stopCluster(cl)

for(i in 1:length(MDS)){
MDS.points <-MDS[[i]]$points	
	MDS.points[,1]<-MDS.points[,1] *(-1)
		write.csv(MDS.points,paste('NMS Output/NMSPoints',MDS[[i]]$ndim,'D',file.append,'.csv',sep=''),row.names=T)
MDS.species<-MDS[[i]]$species
	MDS.species[,1]<-MDS.species[,1] *(-1)
		write.csv(MDS.species,paste('NMS Output/NMSSpecies',MDS[[i]]$ndim,'D',file.append,'.csv',sep=''),row.names=T)
	}

## Save the 2D and 3D stress values to a datasheet
	thestress<-cbind(c(2:3),c(MDS[[1]]$stress,MDS[[2]]$stress))
		colnames(thestress)<-c('Number of Axes','Stress')
		write.csv(thestress,paste('NMS Output/Stress',file.append,'.csv',sep=''),row.names=FALSE)
		

		
## Run ordinations in additional dimensions upon request
print(cat("\n","2D and 3D solutions have been saved in the 'NMS Output' folder,",
	"\n","along with a csv of the input matrix, stress data, and the scree plot.			"))
}

if(only23==FALSE){
most
print(cat("\n","Based on the scree plot (either the saved one or the one that has popped up)...							"))
repeat{
print(cat("\n","Would you like to run the ordination in any additional dimensions?",
	"\n","Enter the desired dimensions separated by commas, or type '0' (no quotes) for 'No.'		"))
	
d<-scan(nlines=1,sep=',')
if(d[1]==0){break}
else{
	if(length(d)>1){
		cl.more<-makeCluster(getOption("cl.cores",cores))
		MDS.more<-parLapply(cl.more,c(d),run.NMS)
		stopCluster(cl)} 
	else{MDS.more<-lapply(d,run.NMS)}
for(i in 1:length(MDS.more)){
MDS.more.points <-MDS.more[[i]]$points	
	MDS.more.points[,1]<-MDS.more.points[,1] *(-1)
		write.csv(MDS.more.points,paste('NMS Output/NMSPoints',MDS.more[[i]]$ndim,'D',file.append,'.csv',sep=''),row.names=T)
MDS.more.species<-MDS.more[[i]]$species
	MDS.more.species[,1]<-MDS.more.species[,1] *(-1)
		write.csv(MDS.more.species,paste('NMS Output/NMSSpecies',MDS.more[[i]]$ndim,'D',file.append,'.csv',sep=''),row.names=T)
		}
	}
}
	
print(cat("\n","OK, All done!											"))	
}
else{
most
print(cat("\n","All done!											"))	
}
}