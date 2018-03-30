#' @title Read, format, and combine data from the Foodbase database

#' @description Pulls exported data from the Foodbase database for use in R, combines Sample, Specimen, and Species List data, and formats the data to facilitate analysis.

#' @param samp The name of the sample dataframe, if not working from auto-downloaded data. See Details.
#' @param spec The name of the specimen dataframe, if not working from auto-downloaded data. See Details.
#' @param sppl The name of the species list dataframe, if not working from auto-downloaded data. See Details.
#' @param species Whether to subset the data for only a given taxon. See Details. Default is \code{"All"}.
#' @param stats Whether to calculate total count, size, and biomass data for each taxon in each sample. Default is \code{FALSE}. Currently only works for \code{Drift}.
#' @param gear The sampling gear type of interest (\code{Drift}, \code{LightTrap}, \code{FishGut}, etc). Should be specified only in rare cases where you are not working from \code{\link{readDB}} output. See Details.

#' @details
#' Currently only \code{Drift} and \code{FishGut} are implemented for \code{gear}.
#'
#' The data are based on data saved locally on your computer from the Foodbase database when you run the function \code{\link{readDB}}. To update these data, use \code{\link{readDB}} (see Examples).
#'
#' The function will look for \code{samp}, \code{spec}, and \code{sppl} within the \code{Data/} directory of \code{foodbase}, unless these are specified individually. Any entry for \code{samp}, \code{spec}, and \code{sppl} overrides data in the \code{Data/} directory of \code{foodbase}. See Examples.
#'
#' In general, it is best to use the companion function \code{\link{readDB}} first to get sample data, subset those sample data, then run \code{sampspec} using those sample data as \code{samp}.
#'
#' Using \code{species} you can return data for only certain taxa of interest. In addition to choosing species individually (e.g., \code{species = "CHIL"} or \code{species = c("CHIL", "SIML")}), you can also use the shortcut \code{species = "Big4"} to subset only for species codes \code{CHIL}, \code{SIML}, \code{NZMS}, and \code{GAM}, or the shortcut \code{species = "Big9"} to subset only for species codes \code{CHIL}, \code{CHIP}, \code{CHIA}, \code{SIML}, \code{SIMP}, \code{SIMA}, \code{LUMB}, \code{NZMS}, and \code{GAM}.
#'
#' The argument \code{gear} can be specified (e.g., \code{gear = 'Drift'}), and in general there is no harm in doing so. However, in most cases \code{gear} will inherit the sample type from the attributes of the \code{\link{readDB}} output, so specifying it here is unnecessary. The exception is in rare cases where \code{\link{readDB}} is not run before running \code{sampspec}, as in the case of the \code{whyme} example below.

#' @return Creates a list containing the following dataframes:\cr
#' \code{Samples}: The sample data.\cr
#' \code{Specimens}: The specimen size data. Accounts for Count Extra counts by assigning them proportionally to size bins.\cr
#' \code{Biomass}: The specimen biomass data. Accounts for Count Extra counts by assigning them proportionally to size bins.\cr
#' \code{RawSpecimens}: The specimen size data, excluding any Count Extra counts.\cr
#' \code{RawBiomass}: The specimen biomass data, excluding any Count Extra counts.\cr
#' \code{Taxa}: The taxa list of taxa in the dataset.\cr
#' \code{Missing}: Sample data for any samples that don't have corresponding specimen data (or the species of interest) and were cut from the dataframes listed above.\cr
#' \code{SampDel}: Sample data for any samples that were in the read sample data but were flagged for deletion and therefore cut from the dataframes listed above.\cr
#' \code{SpecDel}: Specimen data for any samples that were in the read sample data but were flagged for deletion and therefore cut from the dataframes listed above.\cr
#' \code{Statistics}: Total count, size, and biomass data, by specimen (if \code{stats = TRUE}).

#'
#' Note on units: All count data are presented as raw counts (i.e., just number of bugs, and not density, rate, or concentration). All biomass values are in \code{mg}, and sizes are in \code{mm}. \code{Distance} is in \code{m}, \code{Velocity} is in \code{m/s} and \code{Volume} is in \code{m^3/s}. \code{TimeElapsed} is in \code{seconds} and \code{ProcessTime} is in decimal \code{hours}.
#'
#' Note on compatibility: If you plan to use \code{\link{sampstats}} or \code{\link{ordmat}} on the \code{sampspec} output, then set \code{stats = TRUE}.

#' @seealso \code{\link{readDB}}, for initial read in of the Sample or Specimen data from the Foodbase database individually, and updating these data. \code{\link{ordmat}} for creating ordination-type matrices of counts, sizes, or biomasses by sample and taxon from a \code{sampspec} output. \code{\link{sampstats}} for computing sample-level statistics on the data.

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

  if(attributes(samp)$gear == "FishGut"){                             # take this out with new fish gut tables
    samp0$BarcodeID = samp0$FishGutSampleID
    spec0$BarcodeID = spec0$FishgutSample
  }

  # Sort by barcode and SpeciesID
  samp0 <- samp0[order(samp0$BarcodeID),]
  spec0 <- spec0[order(spec0$BarcodeID, spec0$SpeciesID),]
  sppl0 <- sppl0[order(sppl0$SpeciesID),]

  # Sample Date, Process Date to date format                              # can this be handled by readDB (or is it already)?
  samp0$Date <- as.Date(samp0$Date, format = '%m/%d/%Y')
  samp0$ProcessDate <- as.Date(samp0$ProcessDate, format = '%m/%d/%Y')

  #------------------------------------
  # Subset to only species of interest                                # Need to add case for just one taxa (i.e., "GAMM")
  if(species == "All" | species == ""){
    spec0 <- spec0
  } else {
    if(species == "Big4"){
      spec0 <- spec0[spec0$SpeciesID %in% c('CHIL', 'SIML', 'GAM', 'NZMS'),]
    } else {
      if(species == "Big9"){
        spec0 <- spec0[spec0$SpeciesID %in% c('CHIL', 'CHIA', 'CHIP', 'SIML', 'SIMA', 'SIMP', 'GAM', 'NZMS', 'LUMB'),]
      } else {
        spec0 <- spec0[spec0$SpeciesID %in% species,]
		  if(dim(spec0)[1] == 0){
		    return(message("Invalid 'species' argument."))
		  }
      }
    }
  }

  #------------------------------------
  # Add same size classes from coarse and fine sieves together for drift

  money.cols = c('Bpt5', as.character(paste0("B", c(1:20))))
  spec.cols = c('BarcodeID', 'SpeciesID', money.cols, 'CountTotal', 'Notes')

  if(attributes(samp)$gear == "Drift"){
    spec1 <- spec0[, c('BarcodeID', 'SpeciesID', 'Cpt5', 'C1', 'C2', 'C3', 'C4', 'C5', 'C6', 'C7', 'C8', 'C9', 'C10', 'C11', 'C12', 'C13', 'C14', 'C15', 'C16', 'C17', 'C18', 'C19', 'C20', 'CountTotal', 'Notes')]
    # colnames(spec1) <- c('BarcodeID', 'SpeciesID', 'Bpt5', 'B1', 'B2', 'B3', 'B4', 'B5', 'B6', 'B7', 'B8', 'B9', 'B10', 'B11', 'B12', 'B13', 'B14', 'B15', 'B16', 'B17', 'B18', 'B19', 'B20', 'CountTotal', 'Notes')
    colnames(spec1) <- spec.cols
    spec1[, c('Bpt5', 'B1', 'B2', 'B3', 'B4', 'B5', 'B6', 'B7', 'B8', 'B9', 'B10', 'B11', 'B12', 'B13', 'B14', 'B15')] <-
      spec0[, c('Cpt5', 'C1', 'C2', 'C3', 'C4', 'C5', 'C6', 'C7', 'C8', 'C9', 'C10', 'C11', 'C12', 'C13', 'C14', 'C15')] +
      spec0[, c('Fpt5', 'F1', 'F2', 'F3', 'F4', 'F5', 'F6', 'F7', 'F8', 'F9', 'F10', 'F11', 'F12', 'F13', 'F14', 'F15')]

    spec1$Extra = spec0$CExtra + spec0$FExtra         # add this in for drift
    }

  # Subset columns to match case for drift above
  if(attributes(samp)$gear == "FishGut"){

    # git rid of NA in the count cols                                                          # could this be handled with the .csv inport?
    cols <- which(colnames(spec0) %in% c(money.cols, "TotalExtra", "AGG"))
    spec0[,cols] = apply(spec0[,cols], 2, function(x) ifelse(is.na(x), 0, x))

    # exclude 'Notes' here, as it's only returned for the drift, when running stats
    spec1 <- spec0[,which(colnames(spec0) %in% spec.cols[-length(spec.cols)])]

    spec1$Extra = spec0$TotalExtra #+ spec0$AGG
    spec1$CountTotal = rowSums(spec0[,c('Bpt5', as.character(paste0("B", c(1:20))))], na.rm = T) +
      spec1$Extra
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
                     'Family', 'Genus', 'Species', 'Habitat', 'Stage', 'FFG',
                     'Description', 'CommonName', 'RegressionA',
                     'RegressionB', 'Notes')]
  sppl2 <- droplevels(sppl2)
  rownames(sppl2) <- 1:dim(sppl2)[1]


  #------------------------------------
  # Add implicit 0 taxa counts into data, remove NOBUs
  # spec4 <- spec3[, c('BarcodeID', 'SpeciesID', 'Bpt5', 'B1', 'B2', 'B3',
  #                    'B4', 'B5', 'B6', 'B7', 'B8', 'B9', 'B10', 'B11',
  #                    'B12', 'B13', 'B14', 'B15', 'B16', 'B17', 'B18',
  #                    'B19', 'B20', 'CountTotal')]

  # combs <- data.frame(BarcodeID = rep(sort(unique(samp2$BarcodeID)),
  #                                     rep(length(unique(spec4$SpeciesID)),
  #                                         length(unique(samp2$BarcodeID)))),
  #                     SpeciesID = rep(sort(unique(spec4$SpeciesID)),
  #                                     length(unique(samp2$BarcodeID))))

  spec4 <- spec3                      # update names in this block...

  # Does this need to be ordered.........? (as combs is?)
  combs <- expand.grid(BarcodeID = unique(samp2$BarcodeID),
                      SpeciesID = unique(spec4$SpeciesID))

  # only the combos that aren't already in the spec
  combs1 <- combs[paste(combs$BarcodeID, combs$SpeciesID) %in%
                    paste(spec4$BarcodeID, spec4$SpeciesID) == FALSE,]

  spec5 <- bind_rows(spec4, combs1)
  # spec5[is.na(spec5)] <- 0            # warning thrown here, b/c 'notes' is a factor, maybe add stringsAsFactors=FALSE when reading .csv?
  spec6 <- spec5[spec5$SpeciesID != 'NOBU',]
  spec7 <- spec6[order(spec6$BarcodeID, spec6$SpeciesID), -which(names(spec6) %in% c('Notes', 'Extra', 'CountTotal')) ]
  rownames(spec7) <- 1:dim(spec7)[1]
  spec7 <- droplevels(spec7)

  #------------------------------------
  # Build new spec dataframe with Count Extra factored into size classes
  snew1 <- spec6

    # snew1$Extra <- with(snew1, ifelse(CountTotal == Bpt5 + B1 + B2 + B3 + B4 +
  #                                     B5 + B6 + B7 + B8 + B9 + B10 + B11 + B12 +
  #                                     B13 + B14 + B15 + B16 + B17 + B18 + B19 + B20, 0,
  #                                   CountTotal - (Bpt5 + B1 + B2 + B3 + B4 + B5 + B6 +
  #                                                   B7 + B8 + B9 + B10 + B11 + B12 + B13
  #                                                 + B14 + B15 + B16 + B17 + B18 + B19 + B20)))

  snew1$MeasuredTotal <- snew1$CountTotal - snew1$Extra

  # snew1$Extra <- snew1$
  # snew1$MeasuredTotal <- rowSums(snew1[,c('Bpt5', as.character(paste0("B", c(1:20))))])


  # snew2 <- snew1[, -which(names(snew1) %in% c('BarcodeID', 'SpeciesID', 'CountTotal', 'Extra', 'MeasuredTotal'))]
  snew2 <- snew1[, money.cols]
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
  # specB <- regs[, c('Bpt5', 'B1', 'B2', 'B3', 'B4', 'B5', 'B6', 'B7', 'B8', 'B9', 'B10', 'B11', 'B12', 'B13', 'B14', 'B15', 'B16', 'B17', 'B18', 'B19', 'B20')]
  specB <- regs[,money.cols]
  reps <- c(0.5, 1:20)                                                                           # should this be reps <- c(.25, 1:20) ?
  lsize <- matrix(reps, ncol = length(reps), nrow = dim(specB)[1], byrow = TRUE)
  ABs <- sppl2[match(regs$SpeciesID, sppl2$SpeciesID), c('RegressionA', 'RegressionB')]
  biom1 <- round(specB * (lsize^ABs$RegressionB) * ABs$RegressionA, 5)    # think we should carry more digits in the mass estimates
  biomsum <- rowSums(biom1)
  biom2 <- regs
  # biom2[, c('Bpt5', 'B1', 'B2', 'B3', 'B4', 'B5', 'B6', 'B7', 'B8', 'B9', 'B10', 'B11', 'B12', 'B13', 'B14', 'B15', 'B16', 'B17', 'B18', 'B19', 'B20')] <- biom1
  biom2[, money.cols] <- biom1
  biom3 <- biom2[!is.na(biomsum),]
  rownames(biom3) <- 1:dim(biom3)[1]
  biom3 <- droplevels(biom3)

  #------------------------------------
  # Get biomasses again, this time accounting for Count Extras
  regs1 <- snew5[snew5$SpeciesID %in% sppregs,]
  # specB1 <- regs1[, c('Bpt5', 'B1', 'B2', 'B3', 'B4', 'B5', 'B6', 'B7', 'B8', 'B9', 'B10', 'B11', 'B12', 'B13', 'B14', 'B15', 'B16', 'B17', 'B18', 'B19', 'B20')]
  specB1 <- regs1[, money.cols]
  ABs <- sppl2[match(regs1$SpeciesID, sppl2$SpeciesID), c('RegressionA', 'RegressionB')]
  nbiom1 <- round(specB1 * (lsize^ABs$RegressionB) * ABs$RegressionA, 2)
  nbiomsum <- rowSums(nbiom1)
  nbiom2 <- regs1
  # nbiom2[, c('Bpt5', 'B1', 'B2', 'B3', 'B4', 'B5', 'B6', 'B7', 'B8', 'B9', 'B10', 'B11', 'B12', 'B13', 'B14', 'B15', 'B16', 'B17', 'B18', 'B19', 'B20')] <- nbiom1
  nbiom2[, money.cols] <- nbiom1
  nbiom3 <- nbiom2[!is.na(nbiomsum),]
  rownames(nbiom3) <- 1:dim(nbiom3)[1]
  nbiom3 <- droplevels(nbiom3)

  #------------------------------------
  # Combine all summary stats into a dataframe
  if(stats == FALSE){
    stat4 <- 'Statistics not computed (stats = FALSE).'
  } else {
    specB2 <- spec3[, c('Bpt5', 'B1', 'B2', 'B3', 'B4', 'B5', 'B6', 'B7', 'B8', 'B9', 'B10', 'B11', 'B12', 'B13', 'B14', 'B15', 'B16', 'B17', 'B18', 'B19', 'B20')]
    lsize2 <- apply(specB2, 1, function(x) rep(reps, x))
    stat1 <- spec3[, c('BarcodeID', 'SpeciesID', 'CountTotal')]
    stat1[, c('SizeMean', 'SizeMedian', 'SizeSD')] <- round(t(sapply(lsize2, function(x) c(mean(x), median(x), sd(x)))), 2)
    stat1$BiomassTotal <- nbiomsum[match(paste(stat1$BarcodeID, stat1$SpeciesID), paste(regs1$BarcodeID, regs1$SpeciesID))]
    stat1$Notes <- spec3$Notes
    stat2 <- bind_rows(stat1, combs1)
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

  attr(lout, 'gear') <- gear
  return(lout)
}

