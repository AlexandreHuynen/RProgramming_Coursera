best <- function(state, outcome) {
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", 
                                 colClasses = "character")
        
        data <- data.frame(unlist(data$Hospital.Name), 
                               unlist(data$State),
                               suppressWarnings(as.numeric(data[, 11])),
                               suppressWarnings(as.numeric(data[, 17])),
                               suppressWarnings(as.numeric(data[, 23]))
                               )
        
        poss_outco <- c("heart attack","heart failure","pneumonia")
        names(data) <- c("name", "state", poss_outco)
        
        ## Check that state and outcome are valid
        if (!any(outcome == poss_outco)){ stop("invalid outcome") }
        if (!any(state == data$state)){ stop("invalid state") }
        
        ## Both state and outcome are valide
        ## Order the data according to 'outcome', hospital names are 
        ## used to break the ties
        data_sub <- data[data$state == state,c("name",outcome)]
        ii <- order(data_sub[[outcome]],data_sub$name)
        
        ## Return hospital name in that state with lowest 30-day death rate
        as.character(data_sub[ii[1],1])
}