rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  df <- read.csv("data/outcome-of-care-measures.csv", 
                 colClasses = "character")
  
  outcome_names <- function(outcome)
  {
    if(outcome == "heart attack") { return("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack")}
    else if(outcome == "heart failure") { return("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure")}
    else { return("Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")}
  }
  
  data_without_na <- function(outcome)
  {
    replace(df[[outcome_names(outcome)]], df[[outcome_names(outcome)]] == "Not Available", NA)
  }
  
  df[[outcome_names(outcome)]] <- data_without_na(outcome)
  df <- na.omit(df)
  df <- df[order(df$Hospital.Name), ]
  
  ## Check that state and outcome are valid
  states <- unique(df$State)
  if(!(state %in% states))
  {
    stop("invalid state")
  }
  else if(!(outcome == "heart attack" || outcome == "heart failure"|| outcome == "pneumonia"))
  {
    stop("invalid outcome")
  }
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  
  rank <- function(state, outcome)
  {
    rank_data <- df[df$State == state, c("Hospital.Name", outcome_names(outcome))]
    rank_data <- rank_data[order(as.numeric(rank_data[[outcome_names(outcome)]])),]
    rank_data$Rank <- c(1:nrow(rank_data))
    rank_data
  }
  
  hospitals <- rank(state, outcome)

  if(num == "best"){
    hospitals[hospitals$Rank == min(hospitals$Rank), c("Hospital.Name")]
  }
  else if(num == "worst"){
    hospitals[hospitals$Rank == max(hospitals$Rank), c("Hospital.Name")]
  }
  else if(num > nrow(hospitals)){
    return(NA)
  }
  else{
    hospitals[hospitals$Rank == as.numeric(num), c("Hospital.Name")]
  }
}