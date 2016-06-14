#' @title A basic function
#' @description This is a test function I wrote.
#' @param test Tests if the function is working. Defaults to TRUE.
#' @examples test()
#' @export
testfx<-function(test=TRUE){
	if(test==TRUE){print('It works!')}
	else{'Hey, it still works!'}
	}