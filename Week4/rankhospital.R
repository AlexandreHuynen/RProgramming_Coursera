rankhospital <- function(state, outcome, num = "best") {
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
        
        ## Order the data according to 'outcome', hospital names are 
        ## used to break the ties (note: remove the NA's)
        data_sub <- data[data$state == state, c("name", outcome)]
        ii <- order(data_sub[[outcome]], data_sub$name, na.last = NA)
        
        ## Check that num is valid
        if (num == "best"){num <- 1}
        if (num == "worst"){num <- length(ii)}
        
        ## Return hospital name in that state with the given rank 30-day death
        ## rate
        as.character(data_sub[ii[num], 1])
}