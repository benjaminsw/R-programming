rm(list=ls(all=TRUE))
rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  cases <- c("heart attack", "heart failure", "pneumonia")
  if(state %in% unique(data$State) && outcome %in% cases){
    if(outcome == "heart attack"){
      results <- subset(data, data$State == state, select = c(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, Hospital.Name))
      results <- results[with(results, order(as.numeric(results[,1]), results[,2])), ]
      results[,1] <- as.numeric(results[,1])
      results <- results[complete.cases(results),]
      for ( i in 1:nrow(results)){
        results$Rank[i] <- i
      }
      if(num == "worst"){
        results <- subset(results, results$Rank == nrow(results), select = Hospital.Name)
      }else{
        results <- subset(results, results$Rank == num, select = Hospital.Name)
      }
    }else if(outcome == "heart failure"){
      results <- subset(data, data$State == state, select = c(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, Hospital.Name))
      results <- results[with(results, order(as.numeric(results[,1]), results[,2])), ]
      results[,1] <- as.numeric(results[,1])
      results <- results[complete.cases(results),]
      for ( i in 1:nrow(results)){
        results$Rank[i] <- i
      }
      if(num == "worst"){
        results <- subset(results, results$Rank == nrow(results), select = Hospital.Name)
      }else{
        results <- subset(results, results$Rank == num, select = Hospital.Name)
      }
    }else if(outcome == "pneumonia"){
      results <- subset(data, data$State == state, select = c(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, Hospital.Name))
      results <- results[with(results, order(as.numeric(results[,1]), results[,2])), ]
      results[,1] <- as.numeric(results[,1])
      results <- results[complete.cases(results),]
      for ( i in 1:nrow(results)){
        results$Rank[i] <- i
      }
      if(num == "worst"){
        results <- subset(results, results$Rank == nrow(results), select = Hospital.Name)
      }else{
        results <- subset(results, results$Rank == num, select = Hospital.Name)
      }
    }    
    if(nrow(results) != 0){
      return(as.character(results))
    }else{
      return(NA)
    }
  }else if(!state %in% unique(data[,7])){
    stop("invalid state")
  }else if(!outcome %in% cases){
    stop("invalid outcome")
  }        
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
}