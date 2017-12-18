#' @title Read, format, and combine data from the Foodbase database

#' @description Pulls exported data from the Foodbase database for use in R, combines Sample, Specimen, and Species code data, and formats the data to facilitate analysis.

#' @param gear The sampling gear type of interest (\code{Drift}, \code{LightTrap}, etc). Default is \code{Drift}.
#' @param samp The name of the sample dataframe, if not working from auto-downloaded data. See details.
#' @param spec The name of the specimen dataframe, if not working from auto-downloaded data. See details.
#' @param sppl The name of the species list dataframe, if not working from auto-downloaded data. See details.
#' @param species Whether to subset the data for only a given taxon. See details. Default is \code{"All"}.
#' @param stats Whether to calculate total count, size, and biomass data for each taxon in each sample. Default is \code{FALSE}.

#' @details
#' Currently only \code{Drift} is implemented for \code{gear}.
#'
#' The data are based on data saved locally on your computer from the Foodbase database when you run the function \code{\link{readDB}}. To update these data, use \code{readDB} (see examples).
#'
#' The function will look for \code{samp}, \code{spec}, and \code{sppl} within the \code{Data/} directory of \code{foodbase}, unless these are specified individually. Any entry for \code{samp}, \code{spec}, and \code{sppl} overrides data in the \code{Data/} directory of \code{foodbase}. See examples. 
#'
#' In general, it is best to use the companion function \code{\link{readDB}} first to get sample data, subset those sample data, then run \code{sampspec} using those sample data as \code{samp}.
#'
#' Using \code{species} you can return data for only certain taxa of interest. In addition to choosing species individually (e.g., \code{species = "CHIL"} or \code{species = c("CHIL", "SIML"}, you can also use the shortcut \code{species = "Big4"} to subset only for species codes \code{CHIL}, \code{SIML}, \code{NZMS}, and \code{GAM}.

#' @return Creates a list containing the following dataframes:\cr
#' \code{Samples}: The sample data.\cr
#' \code{Specimens}: The specimen size data.\cr
#' \code{Biomass}: The specimen biomass data.\cr
#' \code{Statistics}: Total count, size, and biomass data, by specimen (if \code{stats = TRUE}).\cr
#' \code{Taxa}: The taxa list of taxa in the dataset.\cr
#' \code{Missing}: Sample data for any samples that don't have corresponding specimen data (or the species of interest) and were cut from the dataframes listed above.\cr
#' \code{SampDel}: Sample data for any samples that were in the read sample data but were flagged for deletion and therefore cut from the dataframes listed above.\cr
#' \code{SpecDel}: Specimen data for any samples that were in the read sample data but were flagged for deletion and therefore cut from the dataframes listed above.
#'
#' Note on units: All biomass values are in \code{mg}, and sizes are in \code{mm}. \code{Distance} is in \code{m}, \code{Velocity} is in \code{m/s} and \code{Volume} is in \code{m^3/s}. \code{TimeElapsed} is in \code{seconds} and \code{ProcessTime} is in decimal \code{hours}.

#' @seealso \code{\link{readDB}}, for initial read in of the Sample or Specimen data from the Foodbase database individually, and updating these data. \code{\link{ordmat}} for creating ordination-type matrices of counts, sizes, or biomasses by sample and taxon from a \code{sampspec} output. \code{\link{sampstats}} for computing sample-level statistics on the data.

#' @concept access, database

#' @examples
#' ## Read in drift sample data from the network, and update sample, specimen, and species list data.
#' foo <- readDB(updater = TRUE)
#'
#' ## Subset only data from Lees Ferry
#' foo2 <- foo[foo$Reach == "CRLeesFerry",]
#'
#' ## Get the specimen data for these samples, all wrapped together and formatted nicely.
#' foo3 <- sampspec(samp = foo2)
#'
#' ## Example to get these data and also show (separately) data that were cut
#' foo4 <- sampspec(samp = foo2, bads = TRUE)
#'
#' ## Or, if you want to analyze all drift data in the database for inexplicable some reason:
#' whyme <- sampspec(gear = "Drift")
#'
#' ## Example to get only drift samples with New Zealand mudsnails.
#' nzms <- sampspec(species = "NZMS")

#' @author Jeffrey D. Muehlbauer, \email{jmuehlbauer@usgs.gov}

#' @export

## Function call
sampspec <- function(gear = "Drift", samp = "", spec = "", sppl = "", species = "All", stats = FALSE){

## Read in sample data
dbdir <- paste0(find.package('foodbase'),'/Data')
if(is.null(dim(samp))){
	if(file.exists(paste0(dbdir, '/', gear, 'Sample.csv')) == FALSE){
		temp0 <- read.csv(paste0('P:/BIOLOGICAL/Foodbase/Database/Exports/', gear, 'Sample.csv'))
		dir.create(dbdir, showWarnings = FALSE)
		write.csv(temp0,paste0(dbdir,'/', gear, 'Sample.csv'), row.names = FALSE)
	}
	samp0 <- read.csv(paste0(dbdir,'/', gear, 'Sample.csv'))
} else{
	samp0 <- samp
	}
	
## Read in specimen data
if(is.null(dim(spec))){
	if(file.exists(paste0(dbdir, '/', gear, 'Specimen.csv')) == FALSE){
		temp0 <- read.csv(paste0('P:/BIOLOGICAL/Foodbase/Database/Exports/', gear, 'Specimen.csv'))
		dir.create(dbdir, showWarnings = FALSE)
		write.csv(temp0,paste0(dbdir,'/', gear, 'Specimen.csv'), row.names = FALSE)
	}
	spec0 <- read.csv(paste0(dbdir,'/', gear, 'Specimen.csv'))
} else{
	spec0 <- spec
	}
	
## Read in species list data
if(is.null(dim(sppl))){
	if(file.exists(paste0(dbdir, '/SppList.csv')) == FALSE){
		temp0 <- read.csv('P:/BIOLOGICAL/Foodbase/Database/Exports/SppList.csv')
		dir.create(dbdir, showWarnings = FALSE)
		write.csv(temp0,paste0(dbdir,'/SppList.csv'), row.names = FALSE)
	}
	sppl0 <- read.csv(paste0(dbdir,'/SppList.csv'))
} else{
	sppl0 <- sppl
	}

## Sort by barcode and SpeciesID
samp0 <- samp0[order(samp0$BarcodeID),]
spec0 <- spec0[order(spec0$BarcodeID, spec0$SpeciesID),]
sppl0 <- sppl0[order(sppl0$SpeciesID),]

## Sample Date, Process Date to date format
samp0$Date <- as.Date(samp0$Date, format = '%m/%d/%Y')
samp0$ProcessDate <- as.Date(samp0$ProcessDate, format = '%m/%d/%Y')

## Subset to only species of interest
if(species == "All" | species == ""){
	spec0 <- spec0
} else{
	if(species == "Big4"){
		spec0 <- spec0[spec0$SpeciesID %in% c('CHIL', 'SIML', 'GAM', 'NZMS'),]
	} else{
		spec0 <- spec0[spec0$SpeciesID %in% species,]
	}
}

## Add same size classes from coarse and fine sieves together
spec1 <- spec0[, c('BarcodeID', 'SpeciesID', 'Cpt5', 'C1', 'C2', 'C3', 'C4', 'C5', 'C6', 'C7', 'C8', 'C9', 'C10', 'C11', 'C12', 'C13', 'C14', 'C15', 'C16', 'C17', 'C18', 'C19', 'C20', 'CountTotal', 'Notes')]
	colnames(spec1) <- c('BarcodeID', 'SpeciesID', 'Bpt5', 'B1', 'B2', 'B3', 'B4', 'B5', 'B6', 'B7', 'B8', 'B9', 'B10', 'B11', 'B12', 'B13', 'B14', 'B15', 'B16', 'B17', 'B18', 'B19', 'B20', 'CountTotal', 'Notes')
spec1[, c('Bpt5', 'B1', 'B2', 'B3', 'B4', 'B5', 'B6', 'B7', 'B8', 'B9', 'B10', 'B11', 'B12', 'B13', 'B14', 'B15')] <- spec0[, c('Cpt5', 'C1', 'C2', 'C3', 'C4', 'C5', 'C6', 'C7', 'C8', 'C9', 'C10', 'C11', 'C12', 'C13', 'C14', 'C15')] + spec0[, c('Fpt5', 'F1', 'F2', 'F3', 'F4', 'F5', 'F6', 'F7', 'F8', 'F9', 'F10', 'F11', 'F12', 'F13', 'F14', 'F15')]

## Cut specimens that aren't in samples
spec2 <- spec1[spec1$BarcodeID %in% samp0$BarcodeID, ]

## Cut samples that aren't in specimens
samp1 <- samp0[samp0$BarcodeID %in% spec2$BarcodeID, ]
sampM <- samp0[!(samp0$BarcodeID %in% spec2$BarcodeID), ]
	sampM <- droplevels(sampM)
	rownames(sampM) <- 1:dim(sampM)[1]

## Cut samples and specimens that were flagged for deletion
samp2 <- samp1[samp1$FlagDelete != 1, ]
	samp2 <- droplevels(samp2)
sampD <- samp1[samp1$FlagDelete == 1, ]
	sampD <- droplevels(sampD)
	rownames(sampD) <- 1:dim(sampD)[1]
spec3 <- spec2[spec2$BarcodeID %in% samp2$BarcodeID, ]
	spec3 <- droplevels(spec3)
specD <- spec2[spec2$BarcodeID %in% sampD$BarcodeID, ]
	specD <- droplevels(specD)
	rownames(specD) <- 1:dim(specD)[1]

## Subset species list, reduce to only columns of interest
sppl1 <- sppl0[sppl0$SpeciesID %in% spec3$SpeciesID,]
sppl2 <- sppl1[, c('SpeciesID', 'Kingdom', 'Phylum', 'Class', 'Order', 'Family', 'Genus', 'Species', 'Habitat', 'Stage', 'FFG', 'Description', 'CommonName', 'RegressionA', 'RegressionB', 'Notes')]
	sppl2 <- droplevels(sppl2)
	rownames(sppl2) <- 1:dim(sppl2)[1]

## Get biomasses for each size class, taxon, and site
spec4 <- spec3[, c('BarcodeID', 'SpeciesID', 'Bpt5', 'B1', 'B2', 'B3', 'B4', 'B5', 'B6', 'B7', 'B8', 'B9', 'B10', 'B11', 'B12', 'B13', 'B14', 'B15', 'B16', 'B17', 'B18', 'B19', 'B20')]
	rownames(spec4) <- 1:dim(spec4)[1]
specB <- spec3[, c('Bpt5', 'B1', 'B2', 'B3', 'B4', 'B5', 'B6', 'B7', 'B8', 'B9', 'B10', 'B11', 'B12', 'B13', 'B14', 'B15', 'B16', 'B17', 'B18', 'B19', 'B20')]
reps <- c(0.5, 1:20)
lsize1 <- apply(specB, 1, function(x) rep(reps, x))
regs <- sppl1[match(spec3$SpeciesID, sppl1$SpeciesID), c('RegressionA', 'RegressionB')]
biom1 <- round((specB^regs$RegressionB)*regs$RegressionA, 2)
	biomsum <- rowSums(biom1, na.rm = TRUE)
biom2 <- spec3[, c('BarcodeID', 'SpeciesID', 'Bpt5', 'B1', 'B2', 'B3', 'B4', 'B5', 'B6', 'B7', 'B8', 'B9', 'B10', 'B11', 'B12', 'B13', 'B14', 'B15', 'B16', 'B17', 'B18', 'B19', 'B20')]
	biom2[, c('Bpt5', 'B1', 'B2', 'B3', 'B4', 'B5', 'B6', 'B7', 'B8', 'B9', 'B10', 'B11', 'B12', 'B13', 'B14', 'B15', 'B16', 'B17', 'B18', 'B19', 'B20')] <- biom1
biom3 <- biom2[which(biomsum > 0),]
	rownames(biom3) <- 1:dim(biom3)[1]
	biom3 <- droplevels(biom3)

## Combine all summary stats into a dataframe
if(stats == FALSE){
stat1 <- 'Statistics not computed (stats = FALSE).'
} else{
stat1 <- spec3[, c('BarcodeID', 'SpeciesID', 'CountTotal')]
	stat1[, c('SizeMean', 'SizeMedian', 'SizeSD')] <- round(t(sapply(lsize1, function(x) c(mean(x), median(x), sd(x)))), 2)
	stat1$BiomassTotal <- rowSums(biom1, na.rm = TRUE)
	stat1$Notes <- spec3$Notes
}

## Create and spit out list, close function
lout <- list()
	lout[[1]] <- samp2
	lout[[2]] <- spec4
	lout[[3]] <- biom3
	lout[[4]] <- stat1
	lout[[5]] <- sppl2
	lout[[6]] <- sampM
	lout[[7]] <- sampD
	lout[[8]] <- specD
	names(lout) <- c('Samples', 'Specimens', 'Biomass', 'Statistics', 'Taxa', 'Missing', 'SampDel', 'SpecDel')
	attr(lout, 'gear') <- gear	
return(lout)
}

