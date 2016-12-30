corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        ## Return a numeric vector of correlations
        ## NOTE: Do not round the result!
        
        
        # Set the working directory to Week2
        setwd("/Users/Alexandre/Coursera/01 - Data Science/[R]/01 - R Programming/Week2")
        
        # Get list of all visible files in 'directory'
        filesname <- list.files(directory, full.names = TRUE)
        
        # Initialize 'out' and 'data_i' as a data.frame 
        out <- vector(mode = "numeric")
        data_i <- data.frame()
        
        # Loop on 'id'
        for(i in seq_along(filesname)){
                # Read file and compute number of complete cases
                data_i <- read.csv(filesname[i])
                good_i <- complete.cases(data_i)
                nobs_i <- sum(good_i)
                
                # Check threshold
                if(nobs_i > threshold){
                        # Evaluate correlation and append 'out'
                        subdata_i <- data_i[good_i,]
                        out <- c(out,cor(subdata_i$sulfate,subdata_i$nitrate))
                }
        }
        
        out
        
}