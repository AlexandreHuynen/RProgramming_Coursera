rankall <- function(outcome, num = "best") {
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
        
        ## For each state, find the hospital of the given rank
        data_split <- split(data,data$state)
        data_list <- sapply(data_split, function(x, y, z){
                ii <- order(x[[outcome]], x$name, na.last = NA)
                
                ## Check that num is valid
                if (num == "best"){num <- 1}
                if (num == "worst"){num <- length(ii)}
                
                as.character(x[ii[num], 1])
        }, outcome, num, simplify = TRUE, USE.NAMES = FALSE)
        
        ## Return a data frame with the hospital names and the (abbreviated) 
        ## state name
        out <- data.frame(as.vector(data_list),levels(data$state))
        names(out) <- c("hospital","state")
        row.names(out) <- levels(data$state)
        
        return(out)
}