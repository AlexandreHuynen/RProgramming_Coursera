complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
        
        # Set the working directory to Week2
        setwd("/Users/Alexandre/Coursera/01 - Data Science/[R]/01 - R Programming/Week2")
        
        # Get list of all visible files in 'directory'
        filesname <- list.files(directory, full.names = TRUE)
        
        # Initialize 'out' and 'data_i' as a data.frame 
        out <- data.frame()
        data_i <- data.frame()
        
        # Loop on 'id'
        for(i in id){
                # Read file and compute number of complete case
                data_i <- read.csv(filesname[i])
                nobs <- sum(complete.cases(data_i))
                
                # Append 'out'
                out <- rbind(out,data.frame(i,nobs))
        }
        
        names(out) <- c("id","nobs")
        out
        
}