#' @title Read, format, and combine data from the Foodbase database

#' @description Pulls exported data from the Foodbase database for use in R, combines Sample, Specimen, and Species code data, formats the data in a variety of ways to facilitate analysis, and runs some simple statistics.

#' @param gear The sampling gear type of interest (\code{Drift}, \code{LightTrap}, etc). Default is \code{Drift}.
#' @param path The download location of the data on the \code{Network}, on \code{GitHub}, or elsewhere. Default is \code{Network}.
#' @param samp The name of the sample dataframe or the location of the \code{Sample.csv} file. See details.
#' @param spec The name of the specimen dataframe or the location of the \code{Specimen.csv} file. See details.
#' @param sppl The name of the species list dataframe or the location of the \code{SppList.csv} file. See details.

#' @details
#' Currently only \code{Drift} is implemented for \code{gear}.
#'
#' The \code{path} argument is helpful if you are off the USGS network, in which case data can be downloaded from the web by specifying \code{path = "GitHub"}. Note that data are not routinely pushed to GitHub, however, so the data there may be slightly dated. Alternately, you can specify \code{path} to a local directory (see examples).
#'
#' The function will look for \code{samp}, \code{spec}, and \code{sppl} within the directory of \code{path}, unless these are specified individually. Any entry for \code{samp}, \code{spec}, and \code{sppl} overrides \code{path}. See examples. 
#'
#'In general, it is best to use the companion function \code{\link{readDB}} first to get sample data, subset those sample data, then run \code{foodbase} using those sample data as \code{samp}.
	
#' @return Creates a list containing the following dataframes:\cr
#' \code{Samp}: The sample data.\cr
#' \code{Spec}: The specimen data.\cr
#' \code{Stats}: Statistics (counts, richness, sizes, etc.) on the specimen data, by sample.\cr
#' \code{Counts}: An ordination-friendly matrix of count data, by taxon, by sample.\cr
#' \code{SizeMeans}: An ordination-friendly matrix of mean sizes, by taxon, by sample.\cr
#' \code{SizeMedians}: An ordination-friendly matrix of median sizes, by taxon, by sample.\cr
#' \code{SizeSDs}: An ordination-friendly matrix of the standard deviation in sizes, by taxon, by sample.\cr
#' \code{Biomass}: An ordination-friendly matrix of biomass, by taxon, by sample.\cr
#' \code{Spp}: The taxa list of taxa in the dataset.\cr
#' \code{Missing}: Sample data for any samples that don't have corresponding specimen data (or the species of interest) and were cut from the dataframes listed above.\cr
#' \code{SampDel}: Sample data for any samples that were in the read sample data but were flagged for deletion and therefore cut from the dataframes listed above.\cr
#' \code{SpecDel}: Specimen data for any samples that were in the read sample data but were flagged for deletion and therefore cut from the dataframes listed above.
#'
#' Note on units: All biomass values are in \code{mg}, and sizes are in \code{mm}. \code{Distance} is in \code{m}, \code{Velocity} is in \code{m/s} and \code{Volume} is in \code{m^3/s}. \code{TimeElapsed} is in \code{seconds} and \code{ProcessTime} is in decimal \code{hours}.

#' @seealso \code{\link{readDB}}, for simply reading in the Sample or Specimen data from the Foodbase database individually, with no fanfare.

#' @concept access, database

#' @examples
#' ## Read in drift sample data from the network (all the defaults).
#' foo <- readDB()
#'
#' ## Subset only data from Lees Ferry
#' foo2 <- foo[foo$Reach == "CRLeesFerry",]
#'
#' ## Get the specimen data for these samples, all wrapped together and formatted nicely.
#' foo3 <- foodbase(samp = foo2)
#'
#' ## Or, if you want to analyze all drift data in the database for inexplicable some reason:
#' whyme <- foodbase(gear = "Drift")
#'
#' ## Example of all data stored in a local directory.
#' loc <- foodbase(path = "C:/Users/nflanders/Documents/Analysis/")
#'
#' ## Example of sample data stored in a local directory, while species data are not.
#' mysamp <- read.csv("C:/Users/nflanders/Documents/Analysis/DriftSample.csv")
#' loc <- foodbase(path = "Network", samp = mysamp)
#'
#' ## Example for downloading data from GitHub when off the USGS network.
#' gith <- foodbase(path = "GitHub")
#'
#' ## Example to get only samples with New Zealand mudsnails.
#' nzms <- foodbase(species = "NZMS")

#' @author Jeffrey D. Muehlbauer, \email{jmuehlbauer@usgs.gov}

#' @export

## Function call
foodbase <- function(gear = "Drift", path = "Network", samp = "", spec = "", sppl = "", species = "All"){
	##Need to add code to search for path.
## Read in sample data
if(is.null(dim(samp))){
	if(path == 'Network'){
		samp0 <- read.csv(paste0('P:/BIOLOGICAL/Foodbase/Database/Exports/', gear, 'Sample.csv'))
	} else{ 
		if(path == 'GitHub'){
			samp0 <- read.csv(paste0('https://raw.githubusercontent.com/jmuehlbauer-usgs/Database/master/', gear, 'Sample.csv'))		
		} else{
			samp0 <- read.csv(paste0(path, gear, '/Sample.csv'))
		}
	}
} else{
	samp0 <- samp
	}
	
## Read in specimen data
if(is.null(dim(spec))){
	if(path == 'Network'){
		spec0 <- read.csv(paste0('P:/BIOLOGICAL/Foodbase/Database/Exports/', gear, 'Specimen.csv'))
	} else{ 
		if(path == 'GitHub'){
			spec0 <- read.csv(paste0('https://raw.githubusercontent.com/jmuehlbauer-usgs/Database/master/', gear, 'Specimen.csv'))		
		} else{
			spec0 <- read.csv(paste0(path, '/', gear, 'Specimen.csv'))
		}
	}
} else{
	spec0 <- spec
	}
	
## Read in species list data
if(is.null(dim(sppl))){
	if(path == 'Network'){
		sppl0 <- read.csv('P:/BIOLOGICAL/Foodbase/Database/Exports/SppList.csv')
	} else{ 
		if(path == 'GitHub'){
			sppl0 <- read.csv('https://raw.githubusercontent.com/jmuehlbauer-usgs/Database/master/SppList.csv')		
		} else{
			sppl0 <- read.csv(paste0(path, '/SppList.csv'))
		}
	}
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
	if(species == "Big5"){
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

## Cut samples and specimens that were flagged for deletion
samp2 <- samp1[samp1$FlagDelete != 1, ]
	samp2 <- droplevels(samp2)
sampD <- samp1[samp1$FlagDelete == 1, ]
	sampD <- droplevels(sampD)
spec3 <- spec2[spec2$BarcodeID %in% samp2$BarcodeID, ]
	spec3 <- droplevels(spec3)
specD <- spec2[spec2$BarcodeID %in% sampD$BarcodeID, ]
	specD <- droplevels(specD)

## Subset species list, reduce to only columns of interest
sppl1 <- sppl0[sppl0$SpeciesID %in% spec3$SpeciesID,]
sppl2 <- sppl1[, c('SpeciesID', 'Kingdom', 'Phylum', 'Class', 'Order', 'Family', 'Genus', 'Species', 'Habitat', 'Stage', 'FFG', 'Description', 'CommonName', 'RegressionA', 'RegressionB', 'Notes')]
	sppl2 <- droplevels(sppl2)

## Get stats for each species within each site
specB <- spec3[, c('Bpt5', 'B1', 'B2', 'B3', 'B4', 'B5', 'B6', 'B7', 'B8', 'B9', 'B10', 'B11', 'B12', 'B13', 'B14', 'B15', 'B16', 'B17', 'B18', 'B19', 'B20')]
reps <- c(0.5, 1:20)
lsize1 <- apply(specB, 1, function(x) rep(reps, x))
regs <- sppl1[match(spec3$SpeciesID, sppl1$SpeciesID), c('RegressionA', 'RegressionB')]
spec4 <- subset(spec3, select = -Notes)
	spec4[, c('SizeMean', 'SizeMedian', 'SizeSD')] <- round(t(sapply(lsize1, function(x) c(mean(x), median(x), sd(x)))), 2)
	spec4$Biomass <- round(mapply(function(x,y,z) sum(y*x^z), lsize1, regs$RegressionA, regs$RegressionB), 2)
	spec4$Notes <- spec3$Notes

## Matrices for counts, size means, medians, standard deviations, and biomass
mat0 <- matrix(data = 0, nrow = dim(samp2)[1], ncol = dim(sppl2)[1])
	dimnames(mat0) <- list(sort(samp2$BarcodeID), sort(sppl2$SpeciesID))
mat0[as.matrix(spec4[1:2])] <- spec4$CountTotal
ctdat1 <- as.data.frame(mat0)
mat0[mat0 == 0] <- NA
mat0[as.matrix(spec4[1:2])] <- spec4$SizeMean
	mndat1 <- as.data.frame(mat0)
mat0[as.matrix(spec4[1:2])] <- spec4$SizeMedian
	mddat1 <- as.data.frame(mat0)
mat0[as.matrix(spec4[1:2])] <- spec4$SizeSD
	sddat1 <- as.data.frame(mat0)
mat0[as.matrix(spec4[1:2])] <- spec4$Biomass
	bidat1 <- as.data.frame(mat0)
	bidat1[,colSums(bidat1, na.rm = TRUE) > 0][is.na(bidat1[,colSums(bidat1, na.rm = TRUE) > 0])] <- 0
	bidat2 <- bidat1[,colSums(bidat1, na.rm = TRUE) > 0]

## Get stats by sample, across a variety of metrics
stat1 <- data.frame(samp2$BarcodeID)
	colnames(stat1) <- 'BarcodeID'
frichct <- function(spec){
	trich <- tapply(spec$CountTotal, spec$BarcodeID, length)
		crich <- trich[names(trich) == stat1$BarcodeID]
	tcount <- tapply(spec$CountTotal, spec$BarcodeID, sum)
		ccount <- tcount[names(tcount) == stat1$BarcodeID]
	tbiom <- tapply(spec$Biomass, spec$BarcodeID, function(x) sum(x, na.rm = TRUE))
		cbiom <- tbiom[names(tbiom) == stat1$BarcodeID]
	return(cbind(crich, ccount, cbiom))
}
spec4G <- spec4
	spec4G[, c('Habitat', 'Class', 'Order')] <- sppl1[match(spec4$SpeciesID, sppl1$SpeciesID), c('Habitat', 'Class', 'Order')]
spec4A <- spec4G[spec4G$Habitat == 'Aquatic', ]
spec4I <- spec4G[spec4G$Class == 'Insecta', ]
spec4E <- spec4G[spec4G$Order %in% c('Ephemeroptera', 'Plecoptera', 'Trichoptera'), ]
spec4D <- spec4G[spec4G$Order == 'Diptera' & spec4G$Habitat == 'Aquatic', ]
stat1[, c('RichTotal', 'CountTotal', 'BiomassTotal')] <- frichct(spec4)
stat1[, c('RichAqInvert', 'CountAqInvert', 'BiomassAqInvert')] <- frichct(spec4A)
stat1[, c('RichAqInsect', 'CountAqInsect', 'BiomassAqInsect')] <- frichct(spec4I)
stat1[, c('RichEPT', 'CountEPT', 'BiomassEPT')] <- frichct(spec4E)
stat1[, c('RichAqDipt', 'CountAqDipt', 'BiomassAqDipt')] <- frichct(spec4D)
spec4N <- spec4[spec4$SpeciesID=='NOBU', 'BarcodeID']
	stat1[stat1$BarcodeID %in% spec4N, 'RichTotal'] <- 0
stat1[is.na(stat1)] <- 0
fsize <- function(spec){
	specB <- spec[, c('Bpt5', 'B1', 'B2', 'B3', 'B4', 'B5', 'B6', 'B7', 'B8', 'B9', 'B10', 'B11', 'B12', 'B13', 'B14', 'B15', 'B16', 'B17', 'B18', 'B19', 'B20')]
	specB2 <- aggregate(. ~ spec$BarcodeID, specB, sum)
	specB3 <- specB2[,-1]
	lsize1 <- apply(specB3, 1, function(x) rep(c(0.5, 1:20), x))
	stat1$std <- stat1$md <- stat1$mn <- NA
	stat1[stat1$BarcodeID %in% specB2[,1], c('mn', 'md', 'std')] <- round(t(sapply(lsize1, function(x) c(mean(x), median(x), sd(x)))), 2)
	return(stat1[, c('mn', 'md', 'std')])
}
stat1[,c('SizeTotalMean', 'SizeTotalMed', 'SizeTotalSD')] <- fsize(spec4)
if(dim(spec4A)[1] > 0){
	stat1[, c('SizeAqInvertMean', 'SizeAqInvertMed', 'SizeAqInvertSD')] <- fsize(spec4A)
} else{
	stat1[, c('SizeAqInvertMean', 'SizeAqInvertMed', 'SizeAqInvertSD')] <- NA
}
if(dim(spec4I)[1] > 0){
	stat1[, c('SizeAqInsectMean', 'SizeAqInsectMed', 'SizeAqInsectSD')] <- fsize(spec4I)
} else{
	stat1[, c('SizeAqInsectMean', 'SizeAqInsectMed', 'SizeAqInsectSD')] <- NA
}
if(dim(spec4E)[1] > 0){
	stat1[, c('SizeEPTMean', 'SizeEPTMed', 'SizeEPTSD')] <- fsize(spec4E)
} else{
	stat1[, c('SizeEPTMean', 'SizeEPTMed', 'SizeEPTSD')] <- NA
}
if(dim(spec4D)[1] > 0){
	stat1[, c('SizeAqDiptMean', 'SizeAqDiptMed', 'SizeAqDiptSD')] <- fsize(spec4D)
} else{
	stat1[, c('SizeAqDiptMean', 'SizeAqDiptMed', 'SizeAqDiptSD')] <- NA
}
stat2 <- stat1[,c('BarcodeID', 'CountTotal', 'CountAqInvert', 'CountAqInsect', 'CountEPT', 'CountAqDipt', 'RichTotal', 'RichAqInvert', 'RichAqInsect', 'RichEPT', 'RichAqDipt', 'BiomassTotal', 'BiomassAqInvert', 'BiomassAqInsect', 'BiomassEPT', 'BiomassAqDipt', 'SizeTotalMean', 'SizeAqInvertMean', 'SizeAqInsectMean', 'SizeEPTMean', 'SizeAqDiptMean', 'SizeTotalMed', 'SizeAqInvertMed', 'SizeAqInsectMed', 'SizeEPTMed', 'SizeAqDiptMed', 'SizeTotalSD', 'SizeAqInvertSD', 'SizeAqInsectSD', 'SizeEPTSD', 'SizeAqDiptSD')]
## Create and spit out list, close function
lout <- list()
	lout[[1]] <- samp2
	lout[[2]] <- spec4
	lout[[3]] <- stat2
	lout[[4]] <- ctdat1
	lout[[5]] <- mndat1
	lout[[6]] <- mddat1
	lout[[7]] <- sddat1
	lout[[8]] <- bidat2
	lout[[9]] <- sppl2
	lout[[10]] <- sampM
	lout[[11]] <- sampD
	lout[[12]] <- specD
	names(lout) <- c('Samp', 'Spec', 'Stats', 'Counts', 'SizeMeans', 'SizeMedians', 'SizeSDs', 'Biomass', 'Spp', 'Missing', 'SampDel', 'SpecDel')
return(lout)
}

