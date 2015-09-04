corr <- function(directory, threshold = 0) {
	## 'directory' is a character vector of length 1 indicating
	## the location of the CSV files
	
	## 'threshold' is a numeric vector of length 1 indicating the
	## number of completely observed observations (on all
	## variables) required to compute the correlation between
	## nitrate and sulfate; the default is 0
	
	## Return a numeric vector of correlations
	## NOTE: Do not round the result!
	
	##Empty vector for results
	results <- vector()
	
	##Determine the working directory, and read the file list
	workdir <- paste(getwd(), directory, sep = "/")
	pollutant.filelist <- list.files(path = workdir, pattern = "*.csv", ignore.case = TRUE, full.names = TRUE)
	if (length(pollutant.filelist) == 0) {
		return
	}
	
	##Check the number of complete observations of each file
	##Use only the complete observations
	for (i in 1:length(pollutant.filelist)) {
		
		pollutant.checkdata <- read.csv(pollutant.filelist[i])
		pollutant.completesubset <- pollutant.checkdata[complete.cases(pollutant.checkdata),]
		
		if (nrow(pollutant.completesubset) > threshold) {
			##The complete observation count exceeds the threshold
			results <- c(results, 
						 cor(pollutant.completesubset[, "sulfate"], 
						 	 pollutant.completesubset[, "nitrate"]))
		}
	}
	
	results
}