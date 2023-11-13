best <- function(state, outcome) {
  ## Read outcome data
  df <- read.csv("data/outcome-of-care-measures.csv", 
                 colClasses = "character")
  ## Check that state and outcome are valid
  states <- unique(df$State)
  if(!(state %in% states))
     {
       message("invalid state")
     }
  else if(!(outcome == "heart attack" || outcome == "“heart failure"|| outcome == "pneumonia"))
     {
       message("invalid outcome")
     }
  ## Return hospital name in that state with lowest 30-day death
  ## rate
}

