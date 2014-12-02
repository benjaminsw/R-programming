setwd("C:\\Users\\thymemine\\Desktop\\coursera\\r programming\\rprog\\")

complete <- function(directory, id = 1:332) {

  
  dataFrame<-NULL
  for(i in id){
    df<-getmonitor(i,directory)
    nobs<-nrow(subset(df,sulfate!="NA" & nitrate!="NA")) #this is strong-coupled on the data frame format of all the monitors
    rbind(dataFrame,data.frame(id=i,nobs=nobs))->dataFrame
  }
  
  return(dataFrame)
}

complete("specdata", 1)

complete("specdata", c(2, 4, 8, 10, 12))
