## Write a function that takes two parameters: 'outcome', which describes 'bizarrely'
## a symptom, and a numeric-ish ranking which can be 'best','worst', or a number. Then get
## All the hospitals that rank in that category from each state. For example, 
## rankall('heart attack', 3) would getcreate a dataframe of 
## the 3rd ranked heart attack hospital from each state

rankall <- function(outcome, num = "best") {
  
  # Read in the data. I am starting to use 'myVar' names because I find the R variable naming
  # conventions odd and unstelling. Fuck you people.
  myData <- read.csv(file="outcome-of-care-measures.csv", colClasses = 'character')
  
  # determine what column we're looking at. We've done this before, which indicates this should
  # really be its own function, but we were specifically asked not to do that
  if(outcome == 'heart attack') {
    i <- 11
  }
  else if(outcome == 'heart failure') {
    i <- 17
  }
  else if(outcome == 'pneumonia') {
    i <- 23
  }
  else {
    stop('invalid outcome')
  }
  
  # create vector of each state represented
  unique.states <- sort(unique(myData$State))
  
  # Weird R 'make-a-list-a-better-list-called-a-data-frame' thing. I don't get it.
  result.df <- list()
  
  # Go through our states and get all hospitals with a ranking in column i, 
  # and turn that rank into a number
  for(state in unique.states) {
    myData.state <- myData[myData$State == state, ]
    myData.state[, i] <- as.numeric(x = myData.state[, i])
    myData.state <- myData.state[complete.cases(myData.state), ]
    
    # Do the best/worst/or-number thing. Again, we've done this before.
    if(num == "best") {
      numrank = 1
    }
    else if(num == "worst") {
      numrank = nrow(myData.state)
    }
    else if(is.numeric(x = num)) {
      if(num < 1 || num > nrow(myData.state)) {
        result.df <- rbind(result.df, list(NA, state))
        next
      }
      else numrank <- num
    }
    else {
      stop('invalid num')
    }
    
    # sort the data top to bottom by column i
    myData.state <- myData.state[order(myData.state[,i], myData.state$Hospital.Name), ]
    
    # stuff the num'th ranked hospitals in column i into our retVal    
    return.names <- myData.state[numrank, ]$Hospital.Name
    
    # now row-bind them to the dataframe. Again, I have to imagine 
    # python handles this much more naturally.
    result.df <- rbind(result.df, list(return.names[1], state))
  }
  
  # Now tell R to be extra-extra sure this is a dataframe. because...reasons.
  result.df <- as.data.frame(x = result.df)
  colnames(x = result.df) <- c('hospital', 'state')
  
  result.df
}