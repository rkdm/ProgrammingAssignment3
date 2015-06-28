setwd("C:/Users/Rachel/Documents/coursera-courses/r-programming/ProgrammingAssignment3")

readoutcomedata <- function(file) {
    outcome <- read.csv(file,colClasses="character")
    outcome
}

rankall <- function(outcomename,num="best") {
    ## read outcome data
    outcome <- readoutcomedata("outcome-of-care-measures.csv")
    
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
    
    ## split data by state
    statefactor <- split(outcome,outcome[,7])
    statenameslist <- names(statefactor)
    ranksforeachstate <- vector(mode="character",length=0)
    
    for (statename in names(statefactor)) {
        ## separate data for just this state
        thisstatedata <- statefactor[[statename]]
        rankedlist <- order(as.numeric(thisstatedata[,thiscondcol]),thisstatedata[,2])
        sortedtable <- thisstatedata[rankedlist,]
        sortedrates <- as.numeric(sortedtable[,thiscondcol])
        rankings <- 1:length(rankedlist)
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
        ranksforeachstate[statename] <- output
    }
    
    finaldataframe <- data.frame("hospital"=ranksforeachstate,
                                 "state"=statenameslist,
                                 row.names=NULL,
                                 stringsAsFactors=FALSE)
    
}
