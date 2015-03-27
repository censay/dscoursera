## rankall() function
## Return the hospital from every state that has the given rank
## Return NA if there is no hospital with that rank

rankall <- function(outcome, num="best") {
    ## Read outcome data (od)
    od <- read.csv("outcome-of-care-measures.csv", colClasses="character")
    state <- od$State
    state <- sort(unique(state))
    
    ## Check that outcome is valid
    if (!(outcome %in% c("heart attack", "heart failure", "pneumonia")))
        stop("invalid outcome")
    
    # Get the column of for the data we are after    
    index <- NULL
    if (outcome == "heart attack") index <- 11
    if (outcome == "heart failure") index <- 17
    if (outcome == "pneumonia") index <- 23
    
    #import data as numeric and suppress warnings, then
    #remove NA data
    od[,index] <- suppressWarnings(as.numeric(od[,index]))
    od <- na.omit(od)
    
    sorted <- od[order(od[,7], od[,index], od[,2], na.last=TRUE), ]
    sorted[,index] <- suppressWarnings(as.numeric(sorted[,index]))
    # sorted <- od[order(od[,index], od[,2], na.last=TRUE),]
    # sorted <- na.omit(sorted)
    
    hospital_ID <- NULL
    state_ID <- NULL
    
    for (i in 1:length(state)) {
        onestate <- sorted[which(sorted$State == state[i]),]
        num_hospitals <- nrow(onestate)
        
        ndx <- NULL
        ndx <- suppressWarnings(as.numeric(num))
        if (num == "best") ndx <- 1
        if (num == "worst") ndx <- nrow(onestate)

        if (ndx > num_hospitals) {
            hospital_ID[i] <- NA
            state_ID[i] <- state[i]
            
        } else {
            hospital_ID[i] <- onestate[ndx,2]
            state_ID[i] <- state[i]
        }
        
    }
    data.frame(hospital = hospital_ID, state=state_ID)
}