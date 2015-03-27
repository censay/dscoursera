# Function called best that takes 2 arguments
# 1) 2 character abbr. name of a state
# 2) outcome name

best <- function(state, outcome) {
    ## Read outcome data
    outcomedata <- read.csv("outcome-of-care-measures.csv", colClasses="character")
    
    ## Check that state and outcome are valid
    if (!(state %in% outcomedata$State)) 
        stop("invalid state")
    if (!(outcome %in% c("heart attack", "heart failure", "pneumonia")))
        stop("invalid outcome")
    
    ## Return hospital name in that state with
    ## lowest 30-day death rate
  
 
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
    sub1 <- sub1[order(sub1[,index], na.last=TRUE),2]
    
    #get rid of any NAs 
    sub1 <- na.omit(sub1)
    
    sub1[1]
}