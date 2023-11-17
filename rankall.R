rankall <- function(outcome, num = "best") {
  ## Read outcome data
  df <- read.csv("data/outcome-of-care-measures.csv", 
                 colClasses = "character")
  
  outcome_names <- function(outcome)
  {
    if(outcome == "heart attack") { return("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack")}
    else if(outcome == "heart failure") { return("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure")}
    else {return("Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")}
  }
  
  data_without_na <- function(outcome)
  {
    replace(df[[outcome_names(outcome)]], df[[outcome_names(outcome)]] == "Not Available", NA)
  }
  
  df[[outcome_names(outcome)]] <- data_without_na(outcome)
  df <- na.omit(df)
  df <- df[order(df$Hospital.Name), ]
  df <- df[order(df$State), ]
  ## Check that state and outcome are valid
  if(!(outcome == "heart attack" || outcome == "heart failure"|| outcome == "pneumonia"))
  {
    stop("invalid outcome")
  }
  else if(num !="best" && num !="worst" &&  num > nrow(df))
  {
    return(NA)
  }
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  
  rank <- function(state, outcome)
  {
    rank_data <- df[df$State == state, c("Hospital.Name", outcome_names(outcome))]
    rank_data <- rank_data[order(as.numeric(rank_data[[outcome_names(outcome)]])),]
    rank_data$Rank <- c(1:nrow(rank_data))
    rank_data
  }
  
  rank_all_hospitals <- function(state, outcome, num)
  {
    hospitals <- rank(state, outcome)
    if(num == "best"){
      hospitals[hospitals$Rank == min(hospitals$Rank), c("Hospital.Name")]
    }
    else if(num == "worst"){
      hospitals[hospitals$Rank == max(hospitals$Rank), c("Hospital.Name")]
    }
    else{
      hospitals[hospitals$Rank == as.numeric(num), c("Hospital.Name")]
    }
  }
  
  states <- unique(df$State)
  hospital <- character()
  state <- character()
  for(state_temp in states)
  {
    if(length(rank_all_hospitals(state_temp, outcome, num)) == 0)
    {
      hospital <- c(hospital, NA)
    }
    else
    {
      hospital <- c(hospital, rank_all_hospitals(state_temp, outcome, num))
    }
    state <- c(state, state_temp)
  }
  rank_all_df <- data.frame(hospital = hospital, state = state)
  rank_all_df
  
}
