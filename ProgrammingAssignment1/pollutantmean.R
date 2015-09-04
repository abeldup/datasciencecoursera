pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result!
  
  ##Determine the working directory, and read the file list
  workdir <- paste(getwd(), directory, sep = "/")
  pollutant.filelist <- list.files(path = workdir, pattern = "*.csv", ignore.case = TRUE, full.names = TRUE)
  if (length(pollutant.filelist) == 0) {
  	return
  }
  ##Subset the file list to only the requested
  pollutant.filesubset <- pollutant.filelist[id]
  
  ##Combine the data from all the requested files
  pollutant.data <- data.frame()
  for (i in 1:length(pollutant.filesubset)) {
  	pollutant.data <- rbind(pollutant.data, read.csv(pollutant.filesubset[i]))
  }
  
  ##Determine the median for the pollutant
  mean(pollutant.data[, pollutant], na.rm = TRUE)
}