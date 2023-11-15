best <- function(state, outcome) {
  ## Read outcome data
  df <- read.csv("data/outcome-of-care-measures.csv", 
                 colClasses = "character")
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
  
  df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- 
    replace(df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, 
            df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == "Not Available", NA)
  df <- na.omit(df)
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  if(outcome == "heart attack") ## death rate from heart attack
  {
    cleand_df <- df[df$State == state, c("Hospital.Name", "State",
                        "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack")] ##Creates a data frame with selected state and outcome
    the_best <- cleand_df[cleand_df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == ##Save the best mortality rate in a vector 
                            min(as.numeric(cleand_df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)),
              c("Hospital.Name")]
  }
  else if(outcome == "heart failure") ## death rate from heart failure
  {
    cleand_df <- df[df$State == state, c("Hospital.Name", "State", 
                        "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure")]
    the_best <- cleand_df[cleand_df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure == 
                            min(as.numeric(cleand_df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, na.rm=T)), 
              c("Hospital.Name")]
  }
  else ## death rate from pneumonia
  {
    cleand_df <- df[df$State == state, c("Hospital.Name", "State", 
                        "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")]
    the_best <- cleand_df[cleand_df$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia == 
                            min(as.numeric(cleand_df$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)), 
              c("Hospital.Name")]
  } 
  
  ##Checks if the vector has more then one hospital
  if(length(the_best) > 1) 
  {
    the_best <- sort(the_best) ##if - yes, sorts it in the alphabetical order
    the_best
    the_best[1] ##and prints the first hospital
  }
  else
  {
    the_best
  }
}

