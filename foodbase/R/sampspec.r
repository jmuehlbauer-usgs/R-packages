#' @title Read, format, and combine data from the Foodbase database

#' @description Pulls exported data from the Foodbase database for use in R,
#'   combines Sample, Specimen, and Species List data, and formats the data to
#'   facilitate analysis.

#' @param samp The name of the sample dataframe, if not working from
#'   auto-downloaded data. See Details.
#' @param spec The name of the specimen dataframe, if not working from
#'   auto-downloaded data. See Details.
#' @param sppl The name of the species list dataframe, if not working from
#'   auto-downloaded data. See Details.
#' @param species Whether to subset the data for only a given taxon. See
#'   Details. Default is \code{"All"}.
#' @param stats Whether to calculate total count, size, and biomass data for
#'   each taxon in each sample. Default is \code{FALSE}.
#' @param gear The sampling gear type of interest (\code{Drift},
#'   \code{LightTrap}, \code{FishGut}). Should be specified only in rare
#'   cases where you are not working from \code{\link{readDB}} output. See
#'   Details.

#' @details Currently only \code{Drift}, \code{LightTrap}, and \code{FishGut} are implemented for
#' \code{gear}.
#'
#' The data are based on data saved locally on your computer from the Foodbase
#' database when you run the function \code{\link{readDB}}. To update these
#' data, use \code{\link{readDB}} (see Examples).
#'
#' The function will look for \code{samp}, \code{spec}, and \code{sppl} within
#' the \code{Data/} directory of \code{foodbase}, unless these are specified
#' individually. Any entry for \code{samp}, \code{spec}, and \code{sppl}
#' overrides data in the \code{Data/} directory of \code{foodbase} (see
#' Examples).
#'
#' In general, it is best to use the companion function \code{\link{readDB}}
#' first to get sample data, subset those sample data, then run \code{sampspec}
#' using those sample data as \code{samp}.
#'
#' Using \code{species} you can return data for only certain taxa of interest.
#' In addition to choosing species individually (e.g., \code{species = "CHIL"}
#' or \code{species = c("CHIL", "SIML")}), you can also use the shortcut
#' \code{species = "Big4"} to subset only for species codes \code{CHIL},
#' \code{SIML}, \code{NZMS}, and \code{GAMM}, or the shortcut \code{species =
#' "Big9"} to subset only for species codes \code{CHIL}, \code{CHIP},
#' \code{CHIA}, \code{SIML}, \code{SIMP}, \code{SIMA}, \code{OLIG}, \code{NZMS},
#' and \code{GAMM}.
#'
#' The argument \code{gear} can be specified (e.g., \code{gear = 'Drift'}), and
#' in general there is no harm in doing so. However, in most cases \code{gear}
#' will inherit the sample type from the attributes of the \code{\link{readDB}}
#' output, so specifying it here is unnecessary. The exception is in rare cases
#' where \code{\link{readDB}} is not run before running \code{sampspec}, as in
#' the case of the \code{whyme} example below.

#' @return Creates a list containing the following dataframes:\cr
#'   \code{Samples}: The sample data.\cr
#'   \code{Specimens}: The specimen size
#'   data. Accounts for Count Extra counts by assigning them proportionally to
#'   size bins.\cr
#'   \code{Biomass}: The specimen biomass data. Accounts for Count
#'   Extra counts by assigning them proportionally to size bins.\cr
#'   \code{RawSpecimens}: The specimen size data, excluding any Count Extra
#'   counts.\cr
#'   \code{RawBiomass}: The specimen biomass data, excluding any
#'   Count Extra counts.\cr
#'   \code{Taxa}: The taxa list of taxa in the
#'   dataset.\cr
#'   \code{Missing}: Sample data for any samples that don't have
#'   corresponding specimen data (or the species of interest) and were cut from
#'   the dataframes listed above.\cr
#'   \code{SampDel}: Sample data for any samples
#'   that were in the read sample data but were flagged for deletion and
#'   therefore cut from the dataframes listed above.\cr
#'   \code{SpecDel}: Specimen
#'   data for any samples that were in the read sample data but were flagged for
#'   deletion and therefore cut from the dataframes listed above.\cr
#'   \code{Statistics}: Total count, size, and biomass data, by specimen (if
#'   \code{stats = TRUE}).

#'
#' Note on units: All count data are presented as raw counts (i.e., just number
#' of bugs, and not density, rate, or concentration). All biomass values are in
#' \code{mg}, and sizes are in \code{mm}. \code{Distance} is in \code{m},
#' \code{Velocity} is in \code{m/s} and \code{Volume} is in \code{m^3}. For Drift data,
#' \code{TimeElapsed} is in \code{seconds} and \code{ProcessTime} is in decimal
#' \code{hours} (these units may differ for other gear types).
#'
#' Note on compatibility: If you plan to use \code{\link{sampstats}} or
#' \code{\link{ordmat}} on the \code{sampspec} output, then set \code{stats =
#' TRUE}.

#' @seealso \code{\link{readDB}}, for initial read in of the Sample or Specimen
#'   data from the Foodbase database individually, and updating these data.
#'   \code{\link{ordmat}} for creating ordination-type matrices of counts,
#'   sizes, or biomasses by sample and taxon from a \code{sampspec} output.
#'   \code{\link{sampstats}} for computing sample-level statistics on the data.

#' @concept access, database

#' @examples
#' # Read in drift sample data from the network, and update sample, specimen, and species list data.
#' foo <- readDB(updater = TRUE)
#'
#' # Subset only data from Lees Ferry
#' foo2 <- foo[foo$Reach == "CRLeesFerry",]
#'
#' # Get the specimen data for these samples, all wrapped together and formatted nicely.
#' foo3 <- sampspec(samp = foo2)
#'
#' # Or, if you want to analyze all drift data in the database for some inexplicable reason:
#' whyme <- sampspec(gear = "Drift")
#'
#' # Example to get only drift samples with New Zealand mudsnails.
#' nzms <- sampspec(species = "NZMS", gear = "Drift")

#' @author Jeffrey D. Muehlbauer, \email{jmuehlbauer@usgs.gov} and Michael J. Dodrill, \email{mdodrill@usgs.gov}

#' @export

## Function call
sampspec <- function(samp = "", spec = "", sppl = "", species = "All", stats = FALSE, gear = ""){


##### Set directories and attributes, do some value checking #####

## Set local data storage directory and gear type attribute
dbdir <- paste0(find.package('foodbase'), '/Data')
if(gear == ''){
	if(is.null(attributes(samp)$gear)){
		if(is.null(attributes(spec)$gear)){
			if(is.null(attributes(sppl)$gear)){
				return(message('Invalid "gear" argument.'))
			} else{
				gear <- attributes(sppl)$gear
			}
		} else{
			gear <- attributes(spec)$gear
		}
	} else {
		gear <- attributes(samp)$gear
	}
}

## Interpret gear for non-accepted cases
if(!(gear %in% c('Drift', 'FishGut', 'LightTrap', 'Sticky', 'Benthic'))){
	gear1 <- toupper(substr(gear, 1, 1))
	if(gear1 %in% c('D', 'F', 'L', 'S', 'B')){
		gear2 <- ifelse(gear1 == 'D', 'Drift',
			ifelse(gear1 == 'F', 'FishGut',
			ifelse(gear1 == 'L', 'LightTrap',
			ifelse(gear1 == 'S', 'Sticky', 'Benthic'))))
		warning(paste0('Invalid gear argument ("', gear, '"). Converted to "', gear2, '".'))
		gear <- gear2
	} else {
		stop(paste0('Invalid gear argument ("', gear, '"). Please correct.'))
	}
}


##### Read in data #####

## Create list of data types and their data
type1 <- c('Sample', 'Specimen', 'SpeciesList')
type2 <- list(samp, spec, sppl)
type3 <- list()

## Read in Sample, Specimen, and Species List data
for(i in 1:3){
	if(i == 3){gear3 <- ''} else{gear3 <- gear}
	if(is.null(dim(type2[[i]]))){
		if(file.exists(paste0(dbdir, '/', gear3, type1[i], '.csv')) == FALSE){
		temp0 <- readDB(gear = gear, type = type1[i], updater = TRUE)
	}
	type3[[i]] <- read.csv(paste0(dbdir, '/', gear3, type1[i], '.csv'))
} else {
	type3[[i]] <- type2[[i]]
}
}
type4 <- lapply(type3, data.table)
	names(type4) <- type1


##### Clean up data and columns formats #####

## Remove DateTime "FishGutID" and change ID names for FishGut
	## Note: Done so later code runs. Change back near the end of function call.
if(gear == 'FishGut'){
	for(i in 1:2){
		type4[[i]] <- type4[[i]][, FishGutID:=NULL]
		names(type4[[i]])[which(names(type4[[i]]) == 'PITTagID')] <- 'BarcodeID'
	}
}

## Convert any lower case BarcodeIDs to upper case
type4 <- lapply(type4, function(x){
	if('BarcodeID' %in% colnames(x)){
		x$BarcodeID <- toupper(x$BarcodeID)
		return(x)
	} else {
		return(x)
	}
})

## Sort by BarcodeID and SpeciesID
type4[[1]] <- type4[[1]][order(BarcodeID),]
type4[[2]] <- type4[[2]][order(BarcodeID, SpeciesID),]
type4[[3]] <- type4[[3]][order(SpeciesID),]

## Change Sample Date and Process Date to date format
type4[[1]]$Date <- as.Date(type4[[1]]$Date, format = '%m/%d/%Y')
type4[[1]]$ProcessDate <- as.Date(type4[[1]]$ProcessDate, format = '%m/%d/%Y')


##### Subset, group, combine data #####

## Pull dataframes out of list
samp0 <- type4[[1]]
spec0 <- type4[[2]]
sppl0 <- type4[[3]]

## Subset to only species of interest
if(length(species) == 1) {
	if(species == 'All' | species == ''){
		species <- unique(sppl0$SpeciesID)
		spec0 <- spec0
    } else {
		if(species == 'Big4'){
			species <- c('CHIL', 'SIML', 'GAMM', 'NZMS')
			spec0 <- spec0[spec0$SpeciesID %in% species,]
		} else {
			if(species == 'Big9'){
				species <- c('CHIL', 'CHIA', 'CHIP', 'SIML', 'SIMA', 'SIMP', 'GAMM', 'NZMS', 'OLIG')
				spec0 <- spec0[spec0$SpeciesID %in% species,]
			} else {
				spec0 <- spec0[spec0$SpeciesID == species,]
			}
		}
    }
} else {
	spec0 <- spec0[spec0$SpeciesID %in% species,]
    if(nrow(spec0) == 0){
		return(warning(paste0('Invalid species argument ("', species, '"). Please correct.')))
    }
}

## Combine same size classes from coarse and fine sieves 
	## Note: For Drift and FishGut only
    ## Note: For FishGut, the old Aggregate and CountExtra have been added together in the database
if(gear %in% c('Drift', 'FishGut')){
	sizecols = function(letter = 'B'){
		as.character(paste0(letter, c(0:20)))
    }
	speccols = c('BarcodeID', 'SpeciesID', sizecols(), 'CountTotal', 'Notes')
if(gear == 'Drift'){
    spec1 <- spec0[, c('BarcodeID', 'SpeciesID', sizecols('C'), 'CountTotal', 'Notes')]
		colnames(spec1) <- speccols
		spec1[, sizecols()[1:16]] <- spec0[, sizecols('C')[1:16], with = FALSE] + 
			spec0[, sizecols('F')[1:16], with = FALSE]
		spec1$Extra <- spec0$CExtra + spec0$FExtra
	}
if(gear == 'FishGut'){
	ecol1 <- c(which(colnames(spec0) != 'BExtra'), which(colnames(spec0) == 'BExtra'))
    spec1 <- setcolorder(spec0, ecol1)
	colnames(spec1)[ncol(spec1)] <- 'Extra'
    }
} else {
	spec1 <- spec0
}

## Cut specimens that aren't in samples
spec2 <- spec1[spec1$BarcodeID %in% samp0$BarcodeID, ]

## Add 0 count rows in spec for processed samples containing none of the subsetted species
bar0 <- unique(type4[[2]][!(type4[[2]]$BarcodeID %in% spec2$BarcodeID),'BarcodeID'])
if(nrow(bar0) > 0){
	bar1 <- rep(bar0, rep(length(species), length(bar0)))
	spp1 <- as.factor(rep(species, length(bar0)))
	coln <- c('BarcodeID', 'SpeciesID', colnames(spec2[, !c('BarcodeID', 'SpeciesID')]))
	bar2 <- as.data.frame(matrix(nrow = length(bar1), ncol = length(coln)))
		colnames(bar2) <- coln
		bar2$BarcodeID <- bar1
		bar2[, 2] <- spp1
		bar2[is.na(bar2)] <- 0
		if(nrow(bar2) > 0){bar2$Notes <- ''}
	spec3 <- rbind(spec2, bar2)
	spec3 <- spec3[order(BarcodeID),]
} else {
	spec3 <- spec2
}

## Cut samples that aren't in specimens
sampcut <- samp0$BarcodeID %in% spec3$BarcodeID
samp1 <- samp0[sampcut, ]
sampM <- samp0[!sampcut, ]

## Cut samples and specimens that were flagged for deletion
sampdel <- samp1$FlagDelete == 1
samp2 <- samp1[!sampdel, ]
samp3 <- samp2[, FlagDelete:=NULL]
sampD <- samp1[sampdel, ]
spec4 <- spec3[spec3$BarcodeID %in% samp2$BarcodeID, ]
specD <- spec3[spec3$BarcodeID %in% sampD$BarcodeID, ]

## Subset species list, reduce to only columns of interest
sppl1 <- sppl0[sppl0$SpeciesID != 'NOBU' & sppl0$SpeciesID %in% spec4$SpeciesID,
	c('SpeciesID', 'Kingdom', 'Phylum', 'Class', 'Order', 'Suborder', 
	'Superfamily', 'Family', 'Subfamily', 'Genus', 'Species', 'Habitat', 'Stage', 
	'FFG', 'Description', 'RegressionA', 'RegressionB', 'Notes')]

## Add implicit 0 taxa counts into data, remove NOBUs
	## Note: NOBU is a code for "NO BUgs". But 0-count smaples are already accounted for.
barID1 <- unique(samp3$BarcodeID)
sppID1 <- unique(spec4$SpeciesID)
combs1 <- CJ(barID1, sppID1)
	colnames(combs1) <- c('BarcodeID', 'SpeciesID')
spec5 <- merge(combs1, spec4, by = c('BarcodeID', 'SpeciesID'), all.x = TRUE)
    nums <- which(!sapply(spec5, class) %in% c('factor', 'character'))
	for (i in nums){set(spec5,which(is.na(spec5[[i]])), i, 0)}
spec6 <- spec5[spec5$SpeciesID != 'NOBU',]


##### Reassign CountExtra, compute biomass #####

## Only applicable to gears other than LightTrap
if(gear != 'LightTrap'){

## Factor CountExtra into size classes
spec4$MeasuredTotal <- spec4$CountTotal - spec4$Extra
spec7 <- spec6[, .SD, .SDcols = !c('Notes', 'CountTotal')]
spec8 <- spec4[, sizecols(), with = FALSE]
spec9 <- spec4[, .SD, .SDcols = !c('CountTotal', 'MeasuredTotal', 'Extra')]
	spec9[, sizecols()] <- round(spec8 + spec8 * spec4$Extra / spec4$MeasuredTotal)
spec10 <- merge(combs1, spec9, by = c('BarcodeID', 'SpeciesID'), all.x = TRUE)
	for (i in nums[nums < ncol(spec10)]){set(spec10,which(is.na(spec10[[i]])), i, 0)}
spec11 <- spec10[spec10$SpeciesID != 'NOBU',]

## Get biomass for each size class, taxon, and site
reps1 <- c(0.5, 1:20)
size1 <- matrix(reps1, ncol = length(reps1), nrow = nrow(spec8), byrow = TRUE)
AB1 <- sppl1[match(spec4$SpeciesID, sppl1$SpeciesID), c('RegressionA', 'RegressionB')]
biom1 <- spec4[, .SD, .SDcols = !c('CountTotal', 'MeasuredTotal', 'Notes', 'Extra')]
	biom1[, sizecols()] <- round(spec8 * (size1^AB1$RegressionB) * AB1$RegressionA, 2)
biom2 <- merge(combs1, biom1, by = c('BarcodeID', 'SpeciesID'), all.x = TRUE)
	biom2[is.na(biom2)] <- 0
	biom2$Extra <- NA
biom3 <- biom2[biom2$SpeciesID != 'NOBU',]

## Get biomass again, this time accounting for CountExtra
AB2 <- sppl1[match(spec9$SpeciesID, sppl1$SpeciesID), c('RegressionA', 'RegressionB')]
biom4 <- spec9
	biom4[, sizecols()] <- round(spec9[, sizecols(), with = FALSE] * 
		(size1^AB1$RegressionB) * AB1$RegressionA, 2)
biom5 <- merge(combs1, biom4, by = c('BarcodeID', 'SpeciesID'), all.x = TRUE)
	for (i in nums[nums < ncol(biom5)]){set(biom5,which(is.na(biom5[[i]])), i, 0)}
biom6 <- biom5[biom5$SpeciesID != 'NOBU',]

## Set Biomass and Raw conditions for LightTrap
} else {
	spec11 <- spec6
	spec7 <- 'Raw specimens are identical to Specimens table for LightTrap. Use that table instead.'
	biom3 <- biom6 <- 'No biomass data are available for LightTrap.'
}
  

##### Compute statistics #####

## Set conditions for LightTrap or non-computed condition
if(gear != 'LightTrap' & stats == TRUE){
	spec12 <- spec4[, sizecols(), with = FALSE]
	size2 <- apply(spec12, 1, function(x) rep(reps1, x))
	stat1 <- spec4[, c('BarcodeID', 'SpeciesID', 'CountTotal')]
		stat1$SizeMean <- round(sapply(size2, mean), 2)
		stat1$SizeMedian <- round(sapply(size2, median), 2)
		stat1$SizeSD <- round(sapply(size2, sd), 2)
		stat1$BiomassTotal <- rowSums(biom4[, sizecols(), with = FALSE])
		stat1$Notes <- spec4$Notes
	stat2 <- merge(combs1, stat1, by = c('BarcodeID', 'SpeciesID'), all.x = TRUE)
		stat2$CountTotal[is.na(stat2$CountTotal)] <- 0
		stat2$BiomassTotal <- ifelse(stat2$CountTotal == 0, 0, stat2$BiomassTotal)
	stat3 <- stat2[stat2$SpeciesID != 'NOBU',]

## Set Statistics conditions for LightTrap or non-computed condition
} else {
	if(gear == 'LightTrap' & stats == TRUE){
		stat3 <- 'Statistics are identical to Specimens table for LightTrap. Use that table instead.'	
	} else {
		stat3 <- 'Statistics not computed (stats = FALSE).'
	}
}


##### Final formatting on tables and list #####

## Create list
lout1 <- list('Samples' = samp3, 'Specimens' = spec11, 'Biomass' = biom6, 
	'RawSpecimens' = spec7, 'RawBiomass' = biom3, 'Taxa' = sppl1, 'Missing' = sampM, 
	'SampDel' = sampD, 'SpecDel' = specD, 'Statistics' = stat3)

## Convert 'BarcodeID' to 'PITTagID' if FishGut
if(gear == 'FishGut'){
	lout1 <- lapply(lout1, function(x){
		if('BarcodeID' %in% colnames(x)){
			colnames(x)[which(colnames(x) == 'BarcodeID')] <- 'PITTagID'
		}
		return(x)
	})
}
	
## Convert columns to desired classes, data.tables to dataframes.
	## Note: Might eventually keep as data.tables instead. But don't want to confuse people for now.
fact1 <- c('BarcodeID', 'PITTagID', 'FishGutID', 'SpeciesID', 
	'Region', 'Reach', 'Bank', 'Collector', 'Weather', 'WindSpeed', 'Habitat', 'Battery', 
	'EntererSample', 'Processor', 'Checker', 'EntererSpecimen', 
	'Kingdom', 'Phylum', 'Class', 'Order', 'Suborder', 'Superfamily', 'Family', 'Subfamily', 
	'Genus', 'Species', 'Habitat', 'Stage', 'FFG')
logi1 <- c('Bats', 'FlagStrange', 'FlagDelete', 'QAQC')
lout2 <- lapply(lout1, function(x){
	if(class(x)[1] == 'data.table'){
		if(dim(x)[1] == 0){
			l2 <- 'No data are available for this category (probably a good thing!)'
		} else {
			fact2 <- which(colnames(x) %in% fact1)
			logi2 <- which(colnames(x) %in% logi1)
			l1 <- x[,(fact2):= lapply(.SD, as.factor), .SDcols = fact2]
			if(length(logi2) > 0){
				l1 <- x[,(logi2):= lapply(.SD, as.logical), .SDcols = logi2]
			}
			l2 <- as.data.frame(l1)
		}
	} else{
		l2 <- x
	}
	return(l2)
})

## Set gear attribute, close function
attr(lout2, 'gear') <- gear
return(lout2)
}

