#' @title Sample-level statistics on foodbase data

#' @description Runs some simple statistics on formatted data sets from the Foodbase database.

#' @param sampspec Output from the \code{\link{sampspec}} function. See Details.

#' @details
#' This function will likely only work with \code{sampspec} set to an output generated by the companion \code{\link{sampspec}} function, and with \code{sampspec(..., stats = TRUE)}. See Examples.

#' Note on units: All count data are presented as raw counts (i.e., just number of bugs, and not density, rate, or concentration). All biomass values are in \code{mg}, and sizes are in \code{mm}.

#' @return Creates a dataframe containing totaled counts, richness, mean/median/standard deviation in size, and biomass for each sample, as well as the same values for aquatic invertebrates, aquatic insects, EPT taxa, and aquatic Diptera.

#' @seealso \code{\link{sampspec}}, for creating the input list required for making \code{sampstats} work.

#' @concept access, database

#' @examples
#' ## Read in drift sample data from the network (all the defaults).
#' foo <- readDB()
#'
#' ## Subset only data from Lees Ferry
#' foo2 <- foo[foo$Reach == "CRLeesFerry",]
#'
#' ## Get the specimen data for these samples, all wrapped together and formatted nicely.
#' foo3 <- sampspec(samp = foo2)
#'
#' ## Get statistics on these data
#' myout <- sampstats(foo3)

#' @author Jeffrey D. Muehlbauer, \email{jmuehlbauer@usgs.gov}

#' @export

# Function call
sampstats <- function(sampspec){

  # Determine what sample type we're working with
  gear <- attributes(sampspec)$gear

  # Create a unique ID (BarcodeID for most, PITTagID for FishGuts)
  if(gear == "FishGut"){
    id <- 'PITTagID'
  } else{id <- 'BarcodeID'}

  # Get stats by sample, across a variety of metrics
  stat1 <- data.frame(sampspec$Samples[, id])
  colnames(stat1) <- id
  frichct <- function(spec){
    spec1 <- spec[spec$CountTotal > 0,]
    trich <- tapply(spec1$CountTotal, spec1[, id], length)
    crich <- trich[names(trich) == stat1[, id]]
    tcount <- tapply(spec$CountTotal, spec[, id], sum)
    ccount <- tcount[names(tcount) == stat1[, id]]
    tbiom <- tapply(spec$Biomass, spec[, id], function(x) sum(x, na.rm = TRUE))
    cbiom <- tbiom[names(tbiom) == stat1[, id]]
    return(cbind(crich, ccount, cbiom))
  }

  spec4G <- sampspec$Statistics
  spec4G[, c('Habitat', 'Class', 'Order')] <- sampspec$Taxa[match(sampspec$Specimens$SpeciesID, sampspec$Taxa$SpeciesID), c('Habitat', 'Class', 'Order')]
  spec4A <- spec4G[spec4G$Habitat == 'Aquatic', ]
  spec4I <- spec4G[spec4G$Class == 'Insecta', ]
  spec4E <- spec4G[spec4G$Order %in% c('Ephemeroptera', 'Plecoptera', 'Trichoptera'), ]
  spec4D <- spec4G[spec4G$Order == 'Diptera' & spec4G$Habitat == 'Aquatic', ]
  stat1[, c('RichTotal', 'CountTotal', 'BiomassTotal')] <- frichct(spec4G)
  stat1[, c('RichAqInvert', 'CountAqInvert', 'BiomassAqInvert')] <- frichct(spec4A)
  stat1[, c('RichAqInsect', 'CountAqInsect', 'BiomassAqInsect')] <- frichct(spec4I)
  stat1[, c('RichEPT', 'CountEPT', 'BiomassEPT')] <- frichct(spec4E)
  stat1[, c('RichAqDipt', 'CountAqDipt', 'BiomassAqDipt')] <- frichct(spec4D)
  spec4N <- sampspec$Specimens[sampspec$Specimens$SpeciesID == 'NOBU', id]
  stat1[stat1[, id] %in% spec4N, 'RichTotal'] <- 0
  stat1[is.na(stat1)] <- 0

  fsize <- function(spec){
    specB <- spec[, paste0('B', 0:20)]
    specB2 <- aggregate(. ~ spec[, id], specB, sum)
    specB3 <- specB2[,-1]
    lsize1 <- apply(specB3, 1, function(x) rep(c(0.5, 1:20), x))
    stat1$std <- stat1$md <- stat1$mn <- NA
    stat1[stat1[, id] %in% specB2[,1], c('mn', 'md', 'std')] <- round(t(sapply(lsize1, function(x) c(mean(x), median(x), sd(x)))), 2)
    return(stat1[, c('mn', 'md', 'std')])
  }

  taxa4G <- sampspec$Specimens
  taxa4G[, c('Habitat', 'Class', 'Order')] <- sampspec$Taxa[match(sampspec$Specimens$SpeciesID, sampspec$Taxa$SpeciesID), c('Habitat', 'Class', 'Order')]
  taxa4A <- taxa4G[taxa4G$Habitat == 'Aquatic', ]
  taxa4I <- taxa4G[taxa4G$Class == 'Insecta', ]
  taxa4E <- taxa4G[taxa4G$Order %in% c('Ephemeroptera', 'Plecoptera', 'Trichoptera'), ]
  taxa4D <- taxa4G[taxa4G$Order == 'Diptera' & taxa4G$Habitat == 'Aquatic', ]
  stat1[,c('SizeTotalMean', 'SizeTotalMed', 'SizeTotalSD')] <- fsize(taxa4G)

  if(dim(taxa4A)[1] > 0){
    stat1[, c('SizeAqInvertMean', 'SizeAqInvertMed', 'SizeAqInvertSD')] <- fsize(taxa4A)
  } else {
    stat1[, c('SizeAqInvertMean', 'SizeAqInvertMed', 'SizeAqInvertSD')] <- NA
  }

  if(dim(taxa4I)[1] > 0){
    stat1[, c('SizeAqInsectMean', 'SizeAqInsectMed', 'SizeAqInsectSD')] <- fsize(taxa4I)
  } else {
    stat1[, c('SizeAqInsectMean', 'SizeAqInsectMed', 'SizeAqInsectSD')] <- NA
  }

  if(dim(taxa4E)[1] > 0){
    stat1[, c('SizeEPTMean', 'SizeEPTMed', 'SizeEPTSD')] <- fsize(taxa4E)
  } else {
    stat1[, c('SizeEPTMean', 'SizeEPTMed', 'SizeEPTSD')] <- NA
  }

  if(dim(taxa4D)[1] > 0){
    stat1[, c('SizeAqDiptMean', 'SizeAqDiptMed', 'SizeAqDiptSD')] <- fsize(taxa4D)
  } else {
    stat1[, c('SizeAqDiptMean', 'SizeAqDiptMed', 'SizeAqDiptSD')] <- NA
  }

  stat2 <- stat1[,c(id, 'CountTotal', 'CountAqInvert', 'CountAqInsect', 'CountEPT',
                    'CountAqDipt', 'RichTotal', 'RichAqInvert', 'RichAqInsect', 'RichEPT',
                    'RichAqDipt', 'BiomassTotal', 'BiomassAqInvert', 'BiomassAqInsect',
                    'BiomassEPT', 'BiomassAqDipt', 'SizeTotalMean', 'SizeAqInvertMean',
                    'SizeAqInsectMean', 'SizeEPTMean', 'SizeAqDiptMean', 'SizeTotalMed',
                    'SizeAqInvertMed', 'SizeAqInsectMed', 'SizeEPTMed', 'SizeAqDiptMed',
                    'SizeTotalSD', 'SizeAqInvertSD', 'SizeAqInsectSD', 'SizeEPTSD', 'SizeAqDiptSD')]

  # Close function
  return(stat2)
}

