#' @title Produce a light trap datafame from Access database.

#' @description Queries the Access Foodbase database for light trap data, and creates useful dataframes.

#' @param helper Enables the use of a user interface to facilitate data subsetting when conditions are not already known. Default is \code{FALSE}.
#' @param subcol If specified, the light trap data conditions by which to subset.
#' @param subval If specified, the individual values within the chosen light trap data conditions by which to subset.
#' @param prepull Allows the function to run using data that have already been pulled from Access (\code{TRUE}). Default is \code{FALSE}.
#' @param Samples If \code{prepull = TRUE}, the samples dataframe to use.
#' @param Counts If \code{prepull = TRUE}, the counts dataframe to use.
#' @param setSQL If specified, a SQL statement for subsetting the data. 

##### Input Variables #####
## 
## advanced: If you have a more complicated data query to run and have already gotten Sample and Processed data from an sqlQuery, advanced=TRUE combines those nicely with a species matrix. Defaults to FALSE.
##advSamples: If you are using advanced, what is your samples dataframe (i.e., from Access Table tbl_LightTrapSample) called?
##advCounts: If you are using advanced, what is your counts dataframe (i.e., from Access Table tbl_LightTrapProcessed) called?
	## Function calls specify whether to just pull all data, speedmode subsetting, subsetting column for speedmode, and subsetting values for speedmode

#' @details
#' The interface enabled with \code{helper = TRUE} allows users to pick subsetting variables from conditions R provides after briefly querying the database. This can be very useful in situations where you do not already know how you might want to subset your data, but can be cumbersome if you already do. The default setting of \code{helper = FALSE} without specifying values for both \code{subcol} and \code{subval} will simply pull all the data from the database.
#'
#' If a value for \code{subcol} is supplied, \code{subval} must be specified as well. Values supplied to these arguments must be stated with quotes (e.g., \code{subcol = "BarcodeID", subval = c("L00262", "L00263")}. As noted in this example, multiple values can be provided to \code{subval}. These can be included as in the example above using the \code{c()} wrapper, but it is likely more efficient to create a vector of values beforehand that can subsequently be called within this argument.
#'
#' The argument \code{prepull} is intended to be used with existing light trap spreadsheets pulled from manually the Access database. Under this situation, Access is not queried, but the existing data are combined and formatted in a consistent way (very useful for standardizing old data with newer conventions). If \code{prepull = TRUE}, then \code{Samples} and \code{Counts} must also be specified. These should be dataframes, and are most likely copies of the Access-generated tables \code{tbl_LightTrapSample} and \code{tbl_LightTrapProcess}.
#'
#' \code{setSQL} provides advanced users with a way to create more complex subsetting conditions, without going through the user interface. If supplied, it should be a SQL statement in the format of the following example (including quotes): 
#'
#' \code{"SELECT * FROM tbl_LightTrapSample WHERE BarcodeID IN ('L00262', 'L00263')"}
#'
#' Note that only the arguments specified in the \code{WHERE} and \code{IN} conditions are likely to be useful; subsetting based on the \code{SELECT} or \code{FROM} conditions may produce undesirable outcomes.

#' @section Warning:
#' This function will only work when your Access, and R (including NpptoR and RStudio, if relevant) versions are all 32-bit or both 64-bit. Don't mix! Settings can be modified in NPPtoR and RStudio to make one or the other version the default. To check which version of Access you are using, open an Access database, select "File" (top left), then "Account", then click the "About Access" button.
	
#' @return Creates a list containing dataframes for using and analyzing light trap data from the Access database.

#' @seealso Package \code{RODBC}, specifically functions \code{\link{odbcConnectAccess2007}} and \code{\link{sqlQuery}}, which this function wraps.

#' @concept access, database

#' @examples
#' ## Pull all light trap data.
#' foo <- accessLT()
#'
#' ## Pull only light traps with barcodes L00262 and L00263
#' foo2 <- accessLT(subcol = "BarcodeID", subval = c("L00262", "L00263"))
#'
#' ## If you want to subset the data, but you are not sure what conditions to use, then use the helper.
#' foo3 <- accessLT(helper = TRUE)
#'
#' ## For pre-existing spredsheets already pulled fomr Access.
#' ## For this example to work, you need spreadsheets titled \code{tbl_LightTrapSample.csv} and \code{tbl_LightTrapProcess.csv} already saved in your working directory. The can be exported directly from Access.
#' samples <- read.csv('tbl_LightTrapSample.csv')
#' counts <- read.csv('tbl_LightTrapProcess.csv')
#' foo4 <- accessLT(prepull = TRUE, Samples = samples, Counts = counts)
#'
#' ## Write a SQL script to include only sample barcodes L00262 and L00263.
#' foo5 <- accessLT(setSQL = "SELECT * FROM tbl_LightTrapSample WHERE BarcodeID IN ('L00262', 'L00263')")

#' @author Jeffrey D. Muehlbauer, \email{jmuehlbauer@usgs.gov}

#' @export

## Function call
accessLT<-function(helper=FALSE,subcol,subval,prepull=FALSE,Samples,Counts,setSQL){

## Run the function in prepull mode
if(prepull==TRUE){
LTsample<-Samples
LTprocessed<-Counts

## Query the database
} else{
	channel<-odbcConnectAccess2007('M:/FOODBASE/Database/Foodbase.accdb')

	## Run the function in advanced mode
	if(!missing(setSQL)){
		LTsample<-sqlQuery(channel,setSQL)
	
	## Run function with pre-known subsetting conditions
	} else{
		if(!missing(subcol)&&!missing(subval)){
			LTsample<-sqlQuery(channel,paste("SELECT * FROM tbl_LightTrapSample WHERE ",subcol," IN (",noquote(paste("'",subval,"'",collapse=", ",sep="")),")",sep=''))
		
		## Run function for getting all data
		} else{
			if(helper==FALSE){
				LTsample<-sqlQuery(channel,"SELECT * FROM tbl_LightTrapSample")
		
			## Run function requesting user input for data subsetting
			} else{
				subNames<-names(sqlQuery(channel,"SELECT * FROM tbl_LightTrapSample WHERE SortID = 1"))
					cat('\nYou can subset by any of these conditions:\n')
					print(subNames)
					cat('\nWhich condition would you like to subset by?\nEnter condition without quotes (e.g., BarcodeID).\nFor multiple conditions, separate each by a comma (e.g., BarcodeID, River).\nOr enter 1, without quotes, to get all data.\n\n')
					a<-readline(prompt='Your entry: ')
				if(a[1]==1){LTsample<-sqlQuery(channel,"SELECT * FROM tbl_LightTrapSample")
				} else{
					a1.0 <- gsub(", ", ",", a)
						a1.1 <- strsplit(a1.0, ",")
							a1 <- a1.1[[1]]
							anum <- length(a1)
					blist <- list()
					cat("\nWhich values would you like to subset by?\nEnter values of interest, without quotes, separated by commas.\nOr enter the name of a vector that contains the values.\n\n")
					for(i in 1:anum){
						b <- readline(prompt = paste("Values for ", a1[i], ": ", sep = ""))
						if(grepl(",", b) == FALSE){
							beval <- try(eval(parse(text = b)),silent = TRUE)
							if(inherits(beval, "try-error")){
								beval <- b
							}
							blist[[i]] <- beval
						} else{
							b2.0 <- gsub(", ", ",", b)
								b2.1 <- strsplit(b2.0, ",")
									blist[[i]] <- b2.1[[1]]
							}
					}
					b3 <- paste("'", paste(blist[[1]], collapse = "', '"), "'", sep = "")
					LTsample <- sqlQuery(channel,paste("SELECT * FROM tbl_LightTrapSample WHERE ", a1[1], " IN (", b3, ")", sep=''))
						if(length(blist) > 1){
							for(i in 2:length(blist)){
							LTsample <- LTsample[LTsample[, a1[i]] %in% blist[[i]], ]
							}
						}
				}
			}
		}
	}

	## Get species data for the samples
	LTprocessed<-sqlQuery(channel,paste("SELECT * FROM tbl_LightTrapProcess WHERE SampleID IN (",noquote(paste(LTsample$SortID,collapse=", ",sep="")),")",sep=''))
	
	## Close database connection
	odbcClose(channel)
}

## Rename Sort ID column in LTsample to SampleID and BarcodeID column to Barcode, reset some column classes
colnames(LTsample)[1:2]<-c('SampleID','Barcode')
	class(LTsample$RedFlag) <- 'logical'

## Rename Estimated Count column in LTprocess to Count, delete unneeded columns and rows and reset classes
LTprocessed$Count <- LTprocessed$CountEstimated
LTprocessed1 <- LTprocessed[!is.na(LTprocessed$CountEstimated),c('PickID', 'SampleID', 'SpeciesID', 'Count', 'SampleKept', 'Reference', 'TotalParts', 'SubsampledParts', 'Notes')]
	class(LTprocessed1$SampleKept) <- 'logical'
	class(LTprocessed1$Reference) <- 'logical'

## Convert Open, close, and processing times to useable format, calculate trap duration
hrs <- suppressWarnings(as.numeric(substr(LTsample$ProcessTime, 1, 2)) * 60)
mins <- suppressWarnings(as.numeric(substr(LTsample$ProcessTime, 3, 4)))
secs <- suppressWarnings(as.numeric(substr(LTsample$ProcessTime, 5, 6))/60)
LTsample$ProcessMinutes <- round(hrs + mins + secs, 2)
LTsample$TimeOpen <- format(LTsample$OpenTime, format = '%H:%M')
LTsample$TimeClose <- format(LTsample$CloseTime, format = '%H:%M')
LTsample$SampleDate <- as.POSIXlt(LTsample$SampleDate, format = '%m/%d/%Y')
LTsample$DurationMinutes <- as.numeric(difftime(strptime(format(LTsample$CloseTime, format = '%H:%M'), format = '%H:%M'), strptime(format(LTsample$OpenTime, format = '%H:%M'), format = '%H:%M'), units = 'mins'))
	
## Create a column for temp (mostly open temp, with closed temps when open temps are unavailable).
LTsample$Temp <- ifelse(is.na(LTsample$OpenTemp), LTsample$CloseTemp, LTsample$OpenTemp)

## Clean up sample info dataframe to contain only columns of interest
LTsample1 <- LTsample[ , c('SampleID', 'Barcode', 'DeploymentSite', 'SampleDate', 'TimeOpen', 'TimeClose', 'DurationMinutes', 'Collector', 'River', 'RiverMile', 'RiverSide', 'WxDescrip', 'Wind', 'WindDirection', 'Habitat', 'Temp', 'TrapLocation', 'DistanceFromCenterpoint', 'AssociatedTrib', 'SampleNotes', 'RedFlag', 'Processor', 'ProcessMinutes', 'ProcessNotes')]
	
## Create dataframe of all Sample IDs (rows) and possible Species (columns)
ids<-sort(unique(LTprocessed1$SampleID))
spp<-sort(unique(LTprocessed1$SpeciesID))
tLT<-as.data.frame(matrix(nrow=length(ids),ncol=length(spp)+1))
	colnames(tLT)<-c('SampleID',as.character(as.factor(spp)))
	tLT$SampleID<-ids
LTp <- LTprocessed1
	LTp$SpeciesID <- as.character(LTprocessed1$SpeciesID)
for(i in 1:dim(LTprocessed1)[1]){
	tmp <- LTp[i,]
	tLT[which(tLT$SampleID == tmp$SampleID), tmp$SpeciesID] <- tmp$Count
}

## Clean up dataframe, converting variables to proper classes, removing No Bug data, and adding barcodes
LTmat0<-tLT
LTmat0[is.na(LTmat0)]<-0
LTmat<-cbind(LTmat0$SampleID,LTsample1$Barcode[match(LTmat0$SampleID,LTsample1$SampleID)],LTmat0[,-1])
	colnames(LTmat)[1:2]<-c('SampleID','Barcode')
if('NOBU'%in%colnames(LTmat)){LTmat1<-subset(LTmat,select=-NOBU)
} else{LTmat1<-LTmat}
LTprocessed2<-cbind(LTprocessed1[,1:2],LTsample1$Barcode[match(LTprocessed1$SampleID,LTsample1$SampleID)],LTprocessed1[,c(-1,-2)])
	colnames(LTprocessed2)[3]<-'Barcode'
	
## Remove unprocessed samples from LTsample
LTsample2 <- LTsample1[LTsample1$SampleID %in% LTmat1$SampleID, ]

## Assign dataframes to an output list, sort by SampleID
LTlist<-list()
LTlist$Info<-droplevels(LTsample2[order(LTsample2$SampleID),])
LTlist$Counts<-droplevels(LTprocessed2[order(LTprocessed2$SampleID,LTprocessed2$PickID),])
LTlist$SppMat<-droplevels(LTmat1[order(LTmat1$SampleID),])

## End the function
if(dim(LTlist$Info)[1]<1){stop("No data could be pulled based on the conditions you specified!\n  Perhaps you subset on conditions that don't exist?") 
} else {cat('\nAll done! The result is a list with the following elements:\n Info: A dataframe of sample information by barcode\n Counts: A dataframe of species counts by pick ID\n SppMat: A dataframe of species counts by barcode\n\n')}
return(LTlist)
}

