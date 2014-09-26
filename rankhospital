rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        data <- read.csv(file="outcome-of-care-measures.csv", colClasses = 'character')
        ## Check that state and outcome are valid
        if(!any(state == data$State)) {
                stop('invalid state')
        }
        
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
        
        data.state <- data[data$State == state, ]
        data.state[, i] <- as.numeric(x=data.state[, i])
        
        
        data.state <- data.state[complete.cases(data.state), ]
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        if(num == "best") {
                num = 1
        }
        else if(num == "worst") {
                num = nrow(data.state)
        }
        else if(is.numeric(x=num)) {
                # print(num)
                if(num<1 || num > nrow(data.state)) {
                        return(NA)
                }
        }
        else {
                stop('invalid num')
        }
        data.state <- data.state[order(data.state[,i], data.state$Hospital.Name), ]
        
        return.names <- data.state[num, ]$Hospital.Name
        return.names[1]
}
