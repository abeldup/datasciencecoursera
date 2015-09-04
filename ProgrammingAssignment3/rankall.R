rankall <- function(p.outcome, p.num = "best") {

	##################################################################################################
	## A utility function to rank the hospitals within the requested state on the requested outcome.
	## The function returns the hospital that matches the requested rank, e.g. best, worst or number.
	##################################################################################################
	getHospital <- function(i.state, i.cond)
	{
		i.result = NA
		
		## Reduce the data to only that of the requested state
		i.statedata <- subset(checkdata, checkdata$State == i.state)
		## Reduce the data to only that of the requested condition
		i.conditiondata <- i.statedata[, c(2, i.cond)]
		## Simplify the column names
		i.conditiondata <- setNames(i.conditiondata, c("Hospital.Name", "Hospital.Rate"))
		## Reduce the data to only the complete cases
		i.completedata <- transform(i.conditiondata, Hospital.Rate = suppressWarnings(as.numeric(Hospital.Rate)))
		i.reduceddata <- i.completedata[complete.cases(i.completedata), ]
		## Order the set by rate asc and names asc
		i.ordereddata <- i.reduceddata[order(i.reduceddata[,2], i.reduceddata[,1]), ]
		
		i.num <- NA
		if (is.numeric(p.num)) {
			i.num <- as.numeric(p.num)
			if (i.num > nrow(i.ordereddata)) {
				return(i.result)
			}
		}
		
		## Return hospital name in the request state with the given rank
		## 30-day death rate
		if (p.num == "best") {
			i.result <- i.ordereddata$Hospital.Name[1]
		}
		else if (p.num == "worst") {
			i.result <- i.ordereddata$Hospital.Name[nrow(i.ordereddata)]
		}
		else {
			i.result <- i.ordereddata$Hospital.Name[i.num]
		}
		
		
		return(i.result)
		
	}
	##################################################################################################	
	
	## Read outcome data
	oldwd <- getwd()
	setwd("C:/Users/abel.duplessis/Documents/GitHub/datasciencecoursera/rprog-data-ProgAssignment3-data")
	checkdata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	setwd(oldwd)
	
	condition.names <- c("heart attack", "heart failure", "pneumonia")
	condition.columns <- c(11, 17, 23)
	conditions <- setNames(condition.columns, condition.names)
	
	## Check that outcome is valid
	if (! p.outcome %in% condition.names) {
		stop("invalid outcome")
	}
	
	## For each state, find the hospital of the given rank
	## Return a data frame with the hospital names and the
	## (abbreviated) state name
	
	## Create empty frame for the results
	l.result <- data.frame(hospital = character(),
						   state = character(),
						   stringsAsFactors=FALSE)
	
	## Create an ordered list of states
	l.states <- unique(checkdata[, 7])
	l.states <- l.states[order(l.states)]
	
	for (l.state in l.states) {
		l.hospital <- getHospital(l.state, conditions[p.outcome])
		l.result <- rbind(l.result, data.frame(hospital = l.hospital, state = l.state))
	}
	l.result
}