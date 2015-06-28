setwd("C:/Users/Rachel/Documents/coursera-courses/r-programming/ProgrammingAssignment3")

best <- function(statename,outcomename) {
    ## read outcome data
    outcome <- read.csv("outcome-of-care-measures.csv",
                        colClasses="character")
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
    } ######else {
    ######    print("state ok")
    ######}
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
    } ######else {
    ######    print("outcome ok")
    ######}
    
    ## translate name of condition to column number in data frame
    thiscondcol <- conditions[[outcomename]]
    
    ## calculate minimum mortality rate for the appropriate condition in
    ##      this state
    ## remove any NA values
    numericrates <- as.numeric(thisstatedata[,thiscondcol])
    minrate <- min(numericrates,na.rm=TRUE)
    ######print(summary(as.numeric(thisstatedata[,thiscondcol])))
    
    ## return hospital name in that state with the 
    ##      lowest 30-day death rate
    ##  hospital names are in column 2
    ######print(typeof(as.numeric(thisstatedata[,thiscondcol])))
    ######print(which(as.numeric(thisstatedata[,thiscondcol]) == minrate))
    ######bestplaces <- outcome[which(outcome[,17] == minrate),2]
    bestplaces <- thisstatedata[which(numericrates == minrate),2]
    
    ## if there is more than 1 hospital with the same rate, 
    ##      sort them alphabetically and return the first one 
    
    sort(bestplaces)[1]
    
}
