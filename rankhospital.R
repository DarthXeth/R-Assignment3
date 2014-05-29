## The rankhospital function is a lot like the best.R function,
## but includes the ability to specify a ranking, rather than just returning 
## the 'Best'
## Overly simplified and commented, by Xeth Waxman

rankhospital <- function(state, outcome, num = "best"){
  
  # Step 1: Read in the file
  outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  # Step 2: Per instructions, 'If an invalid state value is passed to best, the
  # function should throw an error via the stop function with the exact message 
  # "invalid state"'.
  if (!any(state == outcomes$State)) {
    stop('invalid state')
  }
  
  # Step 3: Determine what column in outcomes we should be looking at
  if (outcome == 'heart attack') {
    i <- 11
  } else if (outcome == 'heart failure') {
    i <- 17
  } else if (outcome == 'pneumonia') {
    i <- 23
  } else {
    stop('invalid outcome')
  }
  
  # Step 4: get values for just our passed state, and make the correct column numeric
  outcomes.state <- outcomes[outcomes$State == state, ]
  outcomes.state[, i] <- as.numeric(x = outcomes.state[, i])
  
  # Step 5: Filter out the incomplete data
  outcomes.state <- outcomes.state[complete.cases(outcomes.state), ]
  
  # Step 6: determine what rank we should be looking for; discardbreak for invalid
  # params
  if(num == "best") {
    num = 1
  }
  else if(num == "worst") {
    num = nrow(outcomes.state)
  }
  else if(is.numeric(x=num)) {
    # print(num)
    if(num<1 || num > nrow(outcomes.state)) {
      return(NA)
    }
  }
  else {
    stop('invalid num')
  }
  
  
  # Step 7: There's probably a better, math-ier way to do this, 
  # but I am just sorting by the appropriate column and taking the passed column
  outcomes.state <- outcomes.state[order(outcomes.state[, i], outcomes.state$Hospital.Name), ]
  return.names <- outcomes.state[num, ]$Hospital.Name
  
  # Step 8: Return it!
  return.names[1]
  
}