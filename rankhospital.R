#
# rankhospital for R week 4 programming assignment part 2
#

rankhospital <- function(state, outcome, ranK = "best") {
    
    ## Read outcome data
        outcomeData <- read.csv("./outcome-of-care-measures.csv", colClasses = "character",na.strings="Not Available")
    
    ## build a vector of all possible valid outcomes
        validOutcomes = vector(mode="character",length=0)
        fullColNames = vector(mode="character",length=0)    ## used later
        colnames = names(outcomeData)
        searchKey = "Hospital.30.Day.Death..Mortality..Rates.from."
        lenKey = nchar(searchKey)
        for (i in 1:ncol(outcomeData)) {
            leN = nchar(colnames[i])
            beginninG = regexpr(searchKey,colnames[i])
            if ( beginninG[1] > 0 ) {
                starT=beginninG[1]+lenKey
                outcomeSubstr = substr ( colnames[i], starT, leN  )                
                thisOutcome = tolower( sub( "\\."," ", outcomeSubstr  )  )
                if ( !thisOutcome %in% validOutcomes ) { 
                    validOutcomes = append(validOutcomes, thisOutcome, after=0) 
                    fullColNames = append(fullColNames, colnames[i], after=0) 
                }
            }
        }
    
    ## check that the outcome is valid
        outcome = tolower(outcome)
        if (!outcome %in% validOutcomes) { stop("invalid outcome")}
    
    ## Check that state is valid
        state = toupper(state)
        validStates = unique(outcomeData[,7])
        if ( length(state) != 1) { stop("Please input only 1 state ID") }
        if (!state %in% validStates) { stop("invalid state") }
    
    ## convert outcome name into column name
        thisColName = fullColNames[match(outcome,validOutcomes)]
    
    ## Buid a vector of all data for the requested state
        data.state = outcomeData[outcomeData$State==state,]
    
    ## order data by outcome
        sorted.data.state = data.state[order(as.numeric(data.state[[thisColName]]),data.state[["Hospital.Name"]],decreasing=FALSE,na.last=NA), ]
    
    ## convert "best"/"worst" rank into a numeric
        if (ranK=="best") { ranK = 1 }
        if (ranK=='worst') { ranK = nrow(sorted.data.state) }
    
    ## return the hospital name
        return(sorted.data.state[ranK,"Hospital.Name"])
}