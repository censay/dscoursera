## rankhospital()
## takes 3 arguments
## 1) 2-character abbr. name of state
## 2) outcome
## 3) ranking (number)

## Returns the hospital with the given rank in a certain
## outcome in that state

rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    outcomedata <- read.csv("outcome-of-care-measures.csv", colClasses="character")
    
    ## Check that state and outcome are valid
    if (!(state %in% outcomedata$State)) 
        stop("invalid state")
    if (!(outcome %in% c("heart attack", "heart failure", "pneumonia")))
        stop("invalid outcome")
    
    # Get the column of for the data we are after    
    index <- NULL
    if (outcome == "heart attack") index <- 11
    if (outcome == "heart failure") index <- 17
    if (outcome == "pneumonia") index <- 23
    
    #import data as numeric and suppress warnings, then
    #remove NA data
    outcomedata[,index] <- suppressWarnings(as.numeric(outcomedata[,index]))
    outcomedata <- na.omit(outcomedata)
    
    #limit data to just one state
    sub1 <- subset(outcomedata, State==state)
    
    #limit to just the names of the hospitals
    #in order from lowest mortality to highest
    sub1 <- sub1[order(sub1[,index], sub1[,2], na.last=TRUE),2]
    
    #get rid of any NAs 
    sub1 <- na.omit(sub1)
    
    #choose the number rank
    if (num == "best") num <- 1
    if (num == "worst") num <- length(sub1)
    
    #if num is too big, return NA and quit
    if (num > length(sub1)) return(NA)
    
    sub1[num]
}