best <- function(state, outcome) {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  cases <- c("heart attack", "heart failure", "pneumonia")
  if(state %in% unique(data[,7]) && outcome %in% cases ){
    if(outcome == "heart attack"){
      results <- subset(data, data$State == state, select = c(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, Hospital.Name))
      results <- results[with(results, order(results[,1], results[,2])), ]
      #results <- as.numeric(results[,1])
      results <- subset(results, as.numeric(results[,1]) == min(as.numeric(results[,1]), na.rm=T), select =  Hospital.Name)
    }else if(outcome == "heart failure"){
      results <- subset(data, data$State == state, select = c(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, Hospital.Name))
      results <- results[with(results, order(results[,1], results[,2])), ]
      results <- subset(results, as.numeric(results[,1]) == min(as.numeric(results[,1]), na.rm=T), select =  Hospital.Name)
    }else if(outcome == "pneumonia"){
      results <- subset(data, data$State == state, select = c(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, Hospital.Name))
      results <- results[with(results, order(results[,1], results[,2])), ]
      results <- subset(results, as.numeric(results[,1]) == min(as.numeric(results[,1]), na.rm=T), select =  Hospital.Name)
    }
    return(as.character(results))
  }else if(!state %in% unique(data[,7])){
    stop("invalid state")
  }else if(!outcome %in% cases){
    stop("invalid outcome")
  }        
  ## Return hospital name in that state with lowest 30-day death
  
  ## rate
}