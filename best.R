## The best function takes a passed state and a desired outcome, 
## and looks in the file 'outcome-of-care-measures.csv' and 
## finds the hospital in the 'passed'state' variable with the lowest
## 30-day mortality rate for the desired outcome
## Overly simplified and commented, by Xeth Waxman

best <- function(state, outcome) {
  
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
  
  # Step 6: There's probably a better, math-ier way to do this, 
  # but I am just sorting by the appropriate column and taking the top row
  outcomes.state <- outcomes.state[order(outcomes.state[, i], outcomes.state$Hospital.Name), ]
  return.names <- outcomes.state[1, ]$Hospital.Name
  
  # Step 7: Return it!
  return.names[1]
  
}