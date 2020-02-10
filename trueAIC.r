##### trueAIC function to convert AIC values from a log response #####
	## For the purpose of direct comparison with AIC values from a linear response
	## Last modified 10 February 2020 by J.D. Muehlbauer


## Function allows direct comparison with AIC values from a linear response.
	## Modification of code from Jack Weiss formerly found here (Link is now bad): 
		## http://www.unc.edu/courses/2008fall/ecol/563/001/docs/lectures/lecture11.htm
	## The model argument takes a model output from lm, glm, lme4, or nlme.
		## Possibly others as well (no guarantees! But if it doesn't work, you'll get an error!)
	## The IC argument specifies the information criterion to calculate. Accepts "AIC" (the default) or "BIC".
	
trueAIC <- function(model, IC = 'AIC'){
	## Extract model information
	modclass <- class(model)
	if('lme' %in% modclass){
		log.y <- getResponse(model)
		K <- sum(model$dims$ngrps * model$dims$ncol)
	} else{
		if('lmerMod' %in% modclass){
			log.y <- attributes(model)$resp$y
			K <- summary(model)$ngrps + dim(summary(model)$coefficients)[1] + 1
		} else{
			log.y <- model$model[,1]
			K <- dim(summary(model)$coefficients)[1] + 1
			}
		}
	## Compute log-likelihood and associated parameters
	N <- length(log.y)
	sigma2 <- (sum(residuals(model)^2)) / N
		## Squared residuals divided by sample size is the MLE of the variance
	loglike <- sum(log(dnorm(log.y, mean = predict(model), sd = sqrt(sigma2)) * 1 / exp(log.y)))
	## Compute information criterion
	if(IC == 'AIC'){
		theAIC <- (-2 * loglike + 2 * K)
		theAICc <- theAIC + ((2 * K * (K + 1))/(N - K - 1))
		outAIC <-cbind(theAIC, theAICc, K, N)
			colnames(outAIC) <- c('AIC', 'AICc', 'k', 'n')
	}
	if(IC == 'BIC'){
		theBIC <- (-2 * loglike + K * log(N))
		outAIC <- cbind(theBIC, K, N)
			colnames(outAIC) <- c('BIC', 'k', 'n')
	}
	rownames(outAIC) <- NULL
	outAIC
}