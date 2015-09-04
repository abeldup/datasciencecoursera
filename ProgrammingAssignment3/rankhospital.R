rankhospital <- function(p.state, p.outcome, p.num = "best") {
	

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
	
	## Reduce the data to only that of the requested state
	statedata <- subset(checkdata, checkdata$State == p.state)
	## Reduce the data to only that of the requested condition
	conditiondata <- statedata[, c(2, conditions[p.outcome])]
	## Simplify the column names
	conditiondata <- setNames(conditiondata, c("Hospital.Name", "Hospital.Rate"))
	## Reduce the data to only the complete cases
	compdata <- transform(conditiondata, Hospital.Rate = suppressWarnings(as.numeric(Hospital.Rate)))
	reddata <- compdata[complete.cases(compdata), ]
	## Order the set by rate asc and names asc
	ordereddata <- reddata[order(reddata[,2], reddata[,1]), ]
	
	n.num <- NA
	if (is.numeric(p.num)) {
		n.num <- as.numeric(p.num)
		if (n.num > nrow(ordereddata)) {
			return(NA)
		}
	}
	
	## Return hospital name in that state with the given rank
	## 30-day death rate
	if (p.num == "best") {
	   	ordereddata$Hospital.Name[1]
	}
	else if (p.num == "worst") {
		ordereddata$Hospital.Name[nrow(ordereddata)]
	}
	else {
		ordereddata$Hospital.Name[n.num]
	}
}
