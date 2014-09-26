rankall <- function(outcome, num = "best") {
        ## Read outcome data
        data <- read.csv(file="outcome-of-care-measures.csv", colClasses = 'character')
        ## Check that state and outcome are valid
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
        ## For each state, find the hospital of the given rank
        unique.states <- sort(unique(data$State))
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        result.df <- list()
        
        for(state in unique.states) {
                data.state <- data[data$State == state, ]
                data.state[, i] <- as.numeric(x=data.state[, i])
                data.state <- data.state[complete.cases(data.state), ]
                
                if(num == "best") {
                        numrank = 1
                }
                else if(num == "worst") {
                        numrank = nrow(data.state)
                        # should print the worst state
                       
                }
                else if(is.numeric(x=num)) {
                        # print(num)
                        if(num < 1 || num > nrow(data.state)) {
                                result.df <- rbind(result.df, list(NA, state))
                                print(state)
                                next
                        }
                        else numrank <- num
                        # print(num)
                }
                else {
                        stop('invalid num')
                        # will stop program if invalid
                }
                
               
                data.state <- data.state[order(data.state[,i], data.state$Hospital.Name), ]
                
               
                
                return.names <- data.state[numrank, ]$Hospital.Name
                
              
                
                result.df <- rbind(result.df, list(return.names[1], state))
                
        }
        
        result.df <- as.data.frame(x=result.df)
        colnames(x=result.df) <- c('hospital', 'state')
        
        result.df
}
