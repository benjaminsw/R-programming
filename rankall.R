#rm(list=ls(all=TRUE))
rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  final.results <- data.frame(hospital = character(),state = character())
  final.results[,1] <- as.character(final.results[,1]) 
  final.results[,2] <- as.character(final.results[,2]) 
  ## Check that state and outcome are valid
  cases <- c("heart attack", "heart failure", "pneumonia")
  if(outcome %in% cases){
    if(outcome == "heart attack"){
      for(state in unique(data$State)){
        results <- subset(data, data$State == state, select = c(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, Hospital.Name))
        results <- results[with(results, order(as.numeric(results[,1]), results[,2])), ]
        results[,1] <- as.numeric(results[,1])
        results <- results[complete.cases(results),]
        for ( i in 1:nrow(results)){
          results$Rank[i] <- i
        }
        if(num > nrow(results)){
          final.results[nrow(final.results)+1,1] <- NA
          final.results[nrow(final.results),2] <- state
        }else{
          final.results[nrow(final.results)+1,1] <- as.character(subset(results, results$Rank == num, select = Hospital.Name))
          final.results[nrow(final.results),2] <- state
        }
      }
    }else if(outcome == "heart failure"){
      for(state in unique(data$State)){
        results <- subset(data, data$State == state, select = c(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, Hospital.Name))
        results <- results[with(results, order(as.numeric(results[,1]), results[,2])), ]
        results[,1] <- as.numeric(results[,1])
        results <- results[complete.cases(results),]
        for ( i in 1:nrow(results)){
          results$Rank[i] <- i
        }
        if(num > nrow(results)){
          final.results[nrow(final.results)+1,1] <- as.character(subset(results, results$Rank == nrow(results), select = Hospital.Name))
          final.results[nrow(final.results),2] <- state
        }else{
          final.results[nrow(final.results)+1,1] <- as.character(subset(results, results$Rank == num, select = Hospital.Name))
          final.results[nrow(final.results),2] <- state
        }
      }
    }else if(outcome == "pneumonia"){
      for(state in unique(data$State)){
        results <- subset(data, data$State == state, select = c(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, Hospital.Name))
        results <- results[with(results, order(as.numeric(results[,1]), results[,2])), ]
        results[,1] <- as.numeric(results[,1])
        results <- results[complete.cases(results),]
        for ( i in 1:nrow(results)){
          results$Rank[i] <- i
        }
        if(num > nrow(results)){
          final.results[nrow(final.results)+1,1] <- as.character(subset(results, results$Rank == nrow(results), select = Hospital.Name))
          final.results[nrow(final.results),2] <- state
        }else{
          final.results[nrow(final.results)+1,1] <- as.character(subset(results, results$Rank == num, select = Hospital.Name))
          final.results[nrow(final.results),2] <- state
        }
      }
    }    
    if(nrow(results) != 0){
      return(final.results)
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