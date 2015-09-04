best <- function(p.state, p.outcome) {
	

	## Read outcome data
	oldwd <- getwd()
	setwd("C:/Users/abel.duplessis/Documents/GitHub/datasciencecoursera/rprog-data-ProgAssignment3-data")
	checkdata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	setwd(oldwd)
	
	condition.names <- c("heart attack", "heart failure", "pneumonia")
	condition.columns <- c(11, 17, 23)
	conditions <- setNames(condition.columns, condition.names)
	
	## Check that state and outcome are valid
	if (!p.state %in% checkdata[,7]) {
		stop("invalid state")
	}
	if (! p.outcome %in% condition.names) {
		stop("invalid outcome")
	}

	## Reduce the data to only that of the requested state and condition
	statedata <- subset(checkdata, checkdata$State == p.state)
	reddata <- statedata[, c(2, conditions[p.outcome])]
	
	ordereddata <- reddata[order(suppressWarnings(as.numeric(reddata[,2])), reddata[,1]), ]

	## Return hospital name in the specified state with lowest 30-day death
	## rate for the condition specified
	ordereddata$Hospital.Name[1]	
	
}
