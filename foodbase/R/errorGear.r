#' @title Check for errors in \code{gear} argument formatting.

#' @description Internal function that makes sure the \code{gear} argument is formatted correctly and attempts to correct it if not.

#' @param checkGear The sampling gear type of interest (from \code{readDB} or \code{sampspec}).

#' @details Function checks for common errors in gear formatting such as lower case or misspellings.
#' It tries to correct these as best as possible.

#' @return Function proceeds invisibly if the gear type is OK. Otherwise it proceeds with a warning
#' if it can convert the gear type to something interpretable. Otherwise it returns an error.

#' @seealso \code{\link{readDB}} and \code{\link{sampspec}}, which utilize this function.

#' @concept access, database

#' @examples
#' ## Correct a gear misspelling (lowercase)
#' foo <- readDB(gear = "drift")

#' @author Jeffrey D. Muehlbauer, \email{jmuehlbauer@usgs.gov}

#' @keywords internal

## Function call
errorGear <- function(checkGear){

## Interpret gear for non-accepted cases
if(!(checkGear %in% c('Drift', 'FishGut', 'LightTrap', 'Sticky', 'Benthic'))){
	gear1 <- toupper(substr(checkGear, 1, 1))
	if(gear1 %in% c('D', 'F', 'L', 'S', 'B')){
		gear2 <- ifelse(gear1 == 'D', 'Drift',
			ifelse(gear1 == 'F', 'FishGut',
			ifelse(gear1 == 'L', 'LightTrap',
			ifelse(gear1 == 'S', 'Sticky', 'Benthic'))))
		warning(paste0('Invalid gear argument ("', checkGear, '"). Converted to "', gear2, '".'))
		return(gear2)
	} else {
		stop(paste0('Invalid gear argument ("', checkGear, '"). Please correct.'))
	}
} else {
	return(checkGear)
}
}