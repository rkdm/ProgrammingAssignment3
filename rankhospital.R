setwd("C:/Users/Rachel/Documents/coursera-courses/r-programming/ProgrammingAssignment3")

readoutcomedata <- function(file) {
    outcome <- read.csv(file,colClasses="character")
    outcome
}

rankhospital <- function(statename,outcomename,num="best") {
    ## read outcome data
    outcome <- readoutcomedata("outcome-of-care-measures.csv")
    
    ##########
    ## check that state and outcome are valid
    ##########    
    ## statename needs to be 2-letter abbreviation
    ##   this is stored in column 7
    
    ## split data by state
    statefactor <- split(outcome,outcome[,7])
    
    ## check to make sure input state name is valid
    teststate <- which(statename == names(statefactor))
    if (length(teststate) == 0) {
        stop("invalid state")
    }
    ## separate data for just this state
    thisstatedata <- statefactor[[statename]]
    
    ##########
    ## outcomename should be one of:
    ##    heart attack, heart failure, pneumonia
    ## columns: heart attack = 11, heart failure = 17
    ##      pneumonia = 23
    conditions <- list("heart attack"=11,"heart failure"=17,
                       "pneumonia"=23)
    ## check to make sure input 'outcome' is valid
    testcondition <- which(outcomename == names(conditions))
    if (length(testcondition) == 0) {
        stop("invalid outcome")
    }
    
    ## translate name of condition to column number 
    ##      in data frame
    thiscondcol <- conditions[[outcomename]]
    
    ## rank mortality rates for the 
    ##      appropriate condition in this state
    rankedlist <- order(as.numeric(thisstatedata[,thiscondcol]),thisstatedata[,2])
    ## rankedlist is a list of indices, not values
    ## ranks first by mortality rate, then by alphabetic
    ##      list of hospital names
    sortedtable <- thisstatedata[rankedlist,]
    sortedrates <- as.numeric(sortedtable[,thiscondcol])
    rankings <- 1:length(rankedlist)
    ## rankings is just a vector of row numbers
    
    if (num=="best") {
        ## minimum rate is in sortedtable[1,]
        output <- sortedtable[1,2]
    } else if (num == "worst") {
        maxrate <- max(sortedrates,na.rm=TRUE)
        worstplaces <- sortedtable[which(sortedrates == maxrate),2]
        output <- worstplaces[1]
    } else if (num > length(rankings)) {
        output <- NA
    } else {
        output <- sortedtable[which(rankings == num),2]
    } 
    output
}
