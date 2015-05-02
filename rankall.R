#
# rankall for R week 4 programming assignment part 3
#

rankall <- function(outcome, ranK = "best") {
    
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
    
    ## convert outcome name into column name
        thisColName = fullColNames[match(outcome,validOutcomes)]
    
    ## create a list of state values that exist in the data
        states <- levels(factor(outcomeData[, 7]))
    
    ## create a vector to hold the hospital names
        hospitals <- vector(mode="character", length=0)
    
    ## for each state in the list, get the hospital with the given rank
        for (s in seq(states) ) {
            
            ## Buid a vector of all data for the 'requested 'next' state
                data.state = outcomeData[outcomeData$State==states[s],]
                
            ## order data by outcome
                sorted.data.state = data.state[ order(as.numeric(data.state[[thisColName]]), data.state[["Hospital.Name"]], decreasing=FALSE,na.last=NA), ]

            ## convert "best"/"worst" rank into a numeric
                if (ranK=="best") { 
                    newRank = 1 
                } else if (ranK=="worst") { 
                    newRank = nrow(sorted.data.state) 
                } else {
                    newRank = ranK
                }
            
            ## append the hospital name to the hospital vector
                leN = length(hospitals)
                hospitals = append( hospitals, sorted.data.state[newRank,"Hospital.Name"], after=leN ) 
        }
    
    return( data.frame(hospitals,states) )
}