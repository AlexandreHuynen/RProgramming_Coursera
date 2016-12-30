pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
        ## NOTE: Do not round the result!
        
        # Set the working directory to Week2
        setwd("/Users/Alexandre/Coursera/01 - Data Science/[R]/01 - R Programming/Week2")
        
        # Get list of all visible files in 'directory'
        filesname <- list.files(directory, full.names = TRUE)
        
        # Initialize 'data' as a data.frame
        data <- data.frame()
        
        # Loop on 'id'
        for(i in id){
                # Append 'data'
                data <- rbind(data,read.csv(filesname[i]))
        }
        
        # Evaluate the mean for the pollutant (excluding NA values)
        mean(data[,pollutant], na.rm = TRUE)
        
}