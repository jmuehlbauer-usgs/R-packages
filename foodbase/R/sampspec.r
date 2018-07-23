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
#'   each taxon in each sample. Default is \code{FALSE}. Currently only works
#'   for \code{Drift}.
#' @param gear The sampling gear type of interest (\code{Drift},
#'   \code{LightTrap}, \code{FishGut}, etc). Should be specified only in rare
#'   cases where you are not working from \code{\link{readDB}} output. See
#'   Details.

#' @details Currently only \code{Drift} and \code{FishGut} are implemented for
#' \code{gear}.
#'
#' The data are based on data saved locally on your computer from the Foodbase
#' database when you run the function \code{\link{readDB}}. To update these
#' data, use \code{\link{readDB}} (see Examples).
#'
#' The function will look for \code{samp}, \code{spec}, and \code{sppl} within
#' the \code{Data/} directory of \code{foodbase}, unless these are specified
#' individually. Any entry for \code{samp}, \code{spec}, and \code{sppl}
#' overrides data in the \code{Data/} directory of \code{foodbase}. See
#' Examples.
#'
#' In general, it is best to use the companion function \code{\link{readDB}}
#' first to get sample data, subset those sample data, then run \code{sampspec}
#' using those sample data as \code{samp}.
#'
#' Using \code{species} you can return data for only certain taxa of interest.
#' In addition to choosing species individually (e.g., \code{species = "CHIL"}
#' or \code{species = c("CHIL", "SIML")}), you can also use the shortcut
#' \code{species = "Big4"} to subset only for species codes \code{CHIL},
#' \code{SIML}, \code{NZMS}, and \code{GAM}, or the shortcut \code{species =
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
#'   \code{Samples}: The sample data.\cr \code{Specimens}: The specimen size
#'   data. Accounts for Count Extra counts by assigning them proportionally to
#'   size bins.\cr \code{Biomass}: The specimen biomass data. Accounts for Count
#'   Extra counts by assigning them proportionally to size bins.\cr
#'   \code{RawSpecimens}: The specimen size data, excluding any Count Extra
#'   counts.\cr \code{RawBiomass}: The specimen biomass data, excluding any
#'   Count Extra counts.\cr \code{Taxa}: The taxa list of taxa in the
#'   dataset.\cr \code{Missing}: Sample data for any samples that don't have
#'   corresponding specimen data (or the species of interest) and were cut from
#'   the dataframes listed above.\cr \code{SampDel}: Sample data for any samples
#'   that were in the read sample data but were flagged for deletion and
#'   therefore cut from the dataframes listed above.\cr \code{SpecDel}: Specimen
#'   data for any samples that were in the read sample data but were flagged for
#'   deletion and therefore cut from the dataframes listed above.\cr
#'   \code{Statistics}: Total count, size, and biomass data, by specimen (if
#'   \code{stats = TRUE}).

#'
#' Note on units: All count data are presented as raw counts (i.e., just number
#' of bugs, and not density, rate, or concentration). All biomass values are in
#' \code{mg}, and sizes are in \code{mm}. \code{Distance} is in \code{m},
#' \code{Velocity} is in \code{m/s} and \code{Volume} is in \code{m^3}.
#' \code{TimeElapsed} is in \code{seconds} and \code{ProcessTime} is in decimal
#' \code{hours}.
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

# Function call
sampspec <- function(samp = "", spec = "", sppl = "", species = "All", stats = FALSE, gear = ""){

  # Set local data storage directory and gear type attribute
  dbdir <- paste0(find.package('foodbase'),'/Data')
  if(gear == ''){
    if(is.null(attributes(samp)$gear)){
	  if(is.null(attributes(spec)$gear)){
	    if(is.null(attributes(sppl)$gear)){
          return(message("Invalid 'gear' argument."))
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

  #------------------------------------
  # Read in sample data
  if(is.null(dim(samp))){
    if(file.exists(paste0(dbdir, '/', gear, 'Sample.csv')) == FALSE){
      temp0 <- readDB(gear = gear, type = "Sample", updater = TRUE)
    }
    samp0 <- read.csv(paste0(dbdir, '/', gear, 'Sample.csv'))
  } else {
    samp0 <- samp
  }

  # Read in specimen data
  if(is.null(dim(spec))){
    if(file.exists(paste0(dbdir, '/', gear, 'Specimen.csv')) == FALSE){
      temp0 <- readDB(gear = gear, type = "Specimen", updater = TRUE)
    }
    spec0 <- read.csv(paste0(dbdir, '/', gear, 'Specimen.csv'))
  } else {
    spec0 <- spec
  }

  # Read in species list data
  if(is.null(dim(sppl))){
    if(file.exists(paste0(dbdir, '/SpeciesList.csv')) == FALSE){
      temp0 <- readDB(gear = gear, type = "SpeciesList", updater = TRUE)
    }
    sppl0 <- read.csv(paste0(dbdir, '/SpeciesList.csv'))
  } else {
    sppl0 <- sppl
  }

  # change ID names for FishGut (so code works below), then change back at the end
  if(gear == "FishGut"){
    names(samp0)[which(names(samp0) == "PITTagID")] = "BarcodeID"
    names(spec0)[which(names(spec0) == "PITTagID")] = "BarcodeID"
  }

  # Sort by barcode and SpeciesID
  samp0 <- samp0[order(samp0$BarcodeID),]
  spec0 <- spec0[order(spec0$BarcodeID, spec0$SpeciesID),]
  sppl0 <- sppl0[order(sppl0$SpeciesID),]

  # Sample Date, Process Date to date format
  samp0$Date <- as.Date(samp0$Date, format = '%m/%d/%Y')
  samp0$ProcessDate <- as.Date(samp0$ProcessDate, format = '%m/%d/%Y')

  #------------------------------------
  # Subset to only species of interest
  if(length(species) == 1) {
    if(species == "All" | species == ""){
      spec0 <- spec0
    } else {
      if(species == "Big4"){
        spec0 <- spec0[spec0$SpeciesID %in% c('CHIL', 'SIML', 'GAMM', 'NZMS'),]
      } else {
        if(species == "Big9"){
          spec0 <- spec0[spec0$SpeciesID %in% c('CHIL', 'CHIA', 'CHIP', 'SIML', 'SIMA', 'SIMP', 'GAMM', 'NZMS', 'OLIG'),]
        } else {
          spec0 <- spec0[spec0$SpeciesID == species,]
        }
      }
    }
  } else {
    spec0 <- spec0[spec0$SpeciesID %in% species,]
    if(dim(spec0)[1] == 0){
      return(message("Invalid 'species' argument."))
    }
  }

  #------------------------------------
  # Add same size classes from coarse and fine sieves together for drift
  money.cols = function(letter = NULL){
    if(is.null(letter)){
      as.character(paste0("B", c(0:20)))
    } else {
      as.character(paste0(letter, c(0:20)))
    }
  }

  spec.cols = c('BarcodeID', 'SpeciesID', money.cols(), 'CountTotal', 'Notes')

  if(gear == "Drift"){
    spec1 <- spec0[, c('BarcodeID', 'SpeciesID', money.cols("C"), 'CountTotal', 'Notes')]
    colnames(spec1) <- spec.cols

    spec1[,money.cols()[1:16]] <- spec0[,money.cols("C")[1:16]] + spec0[,money.cols("F")[1:16]]

    spec1$Extra = spec0$CExtra + spec0$FExtra
    }

  # Subset columns to match case for drift above
  if(gear == "FishGut"){

    # exclude 'Notes' here, as it's only returned for the drift, when running stats
    spec1 <- spec0[,which(colnames(spec0) %in% spec.cols[-length(spec.cols)])]

    # Note: The old aggregate & count extra have been added together in the db
    spec1$Extra = spec0$BExtra
    spec1$CountTotal = rowSums(spec0[,money.cols()], na.rm = T) + spec1$Extra
  }

  #------------------------------------
  # Cut specimens that aren't in samples
  spec2 <- spec1[spec1$BarcodeID %in% samp0$BarcodeID, ]

  # Cut samples that aren't in specimens
  samp1 <- samp0[samp0$BarcodeID %in% spec2$BarcodeID, ]
  sampM <- samp0[!(samp0$BarcodeID %in% spec2$BarcodeID), ]
  sampM <- droplevels(sampM)
  if(dim(sampM)[1] > 0){
    rownames(sampM) <- 1:dim(sampM)[1]
  }

  # Cut samples and specimens that were flagged for deletion
  samp2 <- samp1[samp1$FlagDelete != 1, ]
  samp2 <- droplevels(samp2)
  sampD <- samp1[samp1$FlagDelete == 1, ]
  sampD <- droplevels(sampD)
  if(dim(sampD)[1] > 0){
    rownames(sampD) <- 1:dim(sampD)[1]
  }

  spec3 <- spec2[spec2$BarcodeID %in% samp2$BarcodeID, ]
  spec3 <- droplevels(spec3)
  specD <- spec2[spec2$BarcodeID %in% sampD$BarcodeID, ]
  specD <- droplevels(specD)
  if(dim(specD)[1] > 0){
    rownames(specD) <- 1:dim(specD)[1]
  }

  #------------------------------------
  # Subset species list, reduce to only columns of interest
  sppl1 <- sppl0[sppl0$SpeciesID %in% spec3$SpeciesID,]
  sppl2 <- sppl1[, c('SpeciesID', 'Kingdom', 'Phylum', 'Class', 'Order',
					 'Suborder', 'Superfamily', 'Family', 'Subfamily', 
					 'Genus', 'Species', 'Habitat', 'Stage', 'FFG',
                     'Description', 'RegressionA', 'RegressionB', 
					 'Notes')]
  sppl2 <- droplevels(sppl2)
  rownames(sppl2) <- 1:dim(sppl2)[1]

  #------------------------------------
  # Add implicit 0 taxa counts into data, remove NOBUs

  combs <- expand.grid(BarcodeID = unique(samp2$BarcodeID),
                      SpeciesID = unique(spec3$SpeciesID))

  # only the combos that aren't already in the spec
  combs1 <- combs[paste(combs$BarcodeID, combs$SpeciesID) %in%
                    paste(spec3$BarcodeID, spec3$SpeciesID) == FALSE,]

  spec5 <- dplyr::bind_rows(spec3, combs1)
  if(gear == "Drift"){spec5$Notes = as.character(spec5$Notes)}
  spec5[is.na(spec5)] <- 0
  spec6 <- spec5[spec5$SpeciesID != 'NOBU',]
  spec7 <- spec6[order(spec6$BarcodeID, spec6$SpeciesID), -which(names(spec6) %in% c('Notes', 'Extra', 'CountTotal')) ]
  rownames(spec7) <- 1:dim(spec7)[1]
  spec7 <- droplevels(spec7)

  #------------------------------------
  # Build new spec dataframe with Count Extra factored into size classes
  snew1 <- spec6

  snew1$MeasuredTotal <- snew1$CountTotal - snew1$Extra

  snew2 <- snew1[, money.cols()]
  snew3 <- round(snew2 + snew2 * snew1$Extra / snew1$MeasuredTotal)
  snew4 <- cbind(snew1$BarcodeID, snew1$SpeciesID, snew3)
  colnames(snew4) <- colnames(snew1[1:dim(snew4)[2]])
  snew4[is.na(snew4)] <- 0
  snew5 <- snew4[order(snew4$BarcodeID, snew4$SpeciesID),]
  rownames(snew5) <- 1:dim(snew5)[1]
  snew5 <- droplevels(snew5)

  #------------------------------------
  # Get biomasses for each size class, taxon, and site
  sppregs <- sppl2[!is.na(sppl2$RegressionA) & !is.na(sppl2$RegressionB), 'SpeciesID']
  regs <- spec7[spec7$SpeciesID %in% sppregs,]
  specB <- regs[,money.cols()]
  reps <- c(0.5, 1:20)
  lsize <- matrix(reps, ncol = length(reps), nrow = dim(specB)[1], byrow = TRUE)
  ABs <- sppl2[match(regs$SpeciesID, sppl2$SpeciesID), c('RegressionA', 'RegressionB')]
  biom1 <- round(specB * (lsize^ABs$RegressionB) * ABs$RegressionA, 5)
  biomsum <- rowSums(biom1)
  biom2 <- regs
  biom2[, money.cols()] <- biom1
  biom3 <- biom2[!is.na(biomsum),]
  rownames(biom3) <- 1:dim(biom3)[1]
  biom3 <- droplevels(biom3)

  #------------------------------------
  # Get biomasses again, this time accounting for Count Extras
  regs1 <- snew5[snew5$SpeciesID %in% sppregs,]
  specB1 <- regs1[, money.cols()]
  ABs <- sppl2[match(regs1$SpeciesID, sppl2$SpeciesID), c('RegressionA', 'RegressionB')]
  nbiom1 <- round(specB1 * (lsize^ABs$RegressionB) * ABs$RegressionA, 2)
  nbiomsum <- rowSums(nbiom1)
  nbiom2 <- regs1
  nbiom2[, money.cols()] <- nbiom1
  nbiom3 <- nbiom2[!is.na(nbiomsum),]
  rownames(nbiom3) <- 1:dim(nbiom3)[1]
  nbiom3 <- droplevels(nbiom3)

  #------------------------------------
  # Combine all summary stats into a dataframe
  if(stats == FALSE){
    stat4 <- 'Statistics not computed (stats = FALSE).'
  } else {
    specB2 <- spec3[, money.cols()]
    lsize2 <- apply(specB2, 1, function(x) rep(reps, x))
    stat1 <- spec3[, c('BarcodeID', 'SpeciesID', 'CountTotal')]
    stat1[, c('SizeMean', 'SizeMedian', 'SizeSD')] <- round(t(sapply(lsize2, function(x) c(mean(x), median(x), sd(x)))), 2)
    stat1$BiomassTotal <- nbiomsum[match(paste(stat1$BarcodeID, stat1$SpeciesID), paste(regs1$BarcodeID, regs1$SpeciesID))]
    stat1$Notes <- spec3$Notes
    stat2 <- dplyr::bind_rows(stat1, combs1)
    stat2$CountTotal[is.na(stat2$CountTotal)] <- 0
    stat2$Notes[is.na(stat2$Notes)] <- ''
    stat2$BiomassTotal <- ifelse(stat2$CountTotal==0 & is.na(stat2$BiomassTotal) & stat2$SpeciesID %in% sppregs, 0, stat2$BiomassTotal)
    stat3 <- stat2[stat2$SpeciesID != 'NOBU',]
    stat4 <- stat3[order(stat3$BarcodeID, stat3$SpeciesID),]
    rownames(stat4) <- 1:dim(stat4)[1]
    stat4 <- droplevels(stat4)
  }

  #------------------------------------
  # Create and spit out list
  lout <- list('Samples' = samp2,
               'Specimens' = snew5,
               'Biomass' = nbiom3,
               'RawSpecimens' = spec7,
               'RawBiomass' = biom3,
               'Taxa' = sppl2,
               'Missing' = sampM,
               'SampDel' = sampD,
               'SpecDel' = specD,
               'Statistics' = stat4)

  # Convert 'BarcodeID' to 'PITTagID' if FishGut
  if(gear == "FishGut"){
    lout <- lapply(lout, function(x) {if('BarcodeID' %in% colnames(x)){colnames(x)[which(colnames(x) == 'BarcodeID')] <- 'PITTagID'}; x})
  }
  
  # Set gear attribute
  attr(lout, 'gear') <- gear
  return(lout)
}

