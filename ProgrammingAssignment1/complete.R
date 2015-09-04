complete <- function(directory, id = 1:332) {
	## 'directory' is a character vector of length 1 indicating
	## the location of the CSV files
	
	## 'id' is an integer vector indicating the monitor ID numbers
	## to be used
	
	## Return a data frame of the form:
	## id nobs
	## 1  117
	## 2  1041
	## ...
	## where 'id' is the monitor ID number and 'nobs' is the
	## number of complete cases
	
	##Determine the working directory, and read the file list
	workdir <- paste(getwd(), directory, sep = "/")
	pollutant.filelist <- list.files(path = workdir, pattern = "*.csv", ignore.case = TRUE, full.names = TRUE)
	if (length(pollutant.filelist) == 0) {
		return
	}
	##Subset the file list to only the requested
	pollutant.filesubset <- pollutant.filelist[id]
	
	##Initialise data frame for nob counts
	pollutant.completecount <- data.frame(id = integer(),
										  nobs = integer(),
										  stringsAsFactors=FALSE)
	
	##Check the data from each of the requested files
	for (i in 1:length(pollutant.filesubset)) {
		pollutant.data <- read.csv(pollutant.filesubset[i])
		pollutant.completesubset <- pollutant.data[complete.cases(pollutant.data),]
		pollutant.completecount <- rbind(pollutant.completecount, 
										 data.frame(id = pollutant.completesubset[1, "ID"], 
										 		    nobs = nrow(pollutant.completesubset)))
	}
	pollutant.completecount
}