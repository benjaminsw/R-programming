setwd("..")
getwd()
setwd("specdata")

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
  
    #initialize variable to hold data from pollutant column
    filemeans <- c()
    #initialize file directory
    root <- "C:\\Users\\thymemine\\Desktop\\coursera\\r programming\\"
    directory <- paste( root, directory, "\\", sep="" )
    setwd( directory )
    #UNIX pathname to all files
    filenames <- as.character( list.files() )
    filepaths <- paste( directory, filenames, sep="" )
    #iterate through files and store data in filemeans
    for(i in id) {
      loadfile <- read.csv( filepaths[i] )
      result <- loadfile[,pollutant]
      valid <- result[ !is.na( result ) ]
      filemeans <- c(filemeans, valid )
    }
    return( mean( filemeans ) ) 
  
}
pollutantmean("specdata", "sulfate", 1:10)
