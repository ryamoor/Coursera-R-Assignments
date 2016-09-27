
##Author: Ryan Moore
##Date: 7/22/2016


## 3. Ranking Hospitals in State
#This function ranks the hospitals based on outcomes of diagnosises in the hospital by state.

rankhospital <- function(state, outcome, num="best"){
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses ="character")
        ## Check that state and outcome are valid
        states<-unique(data$State)
        outcomeNames<-c("heart attack","pneumonia","heart failure")
        if(is.na(match(state,states))){
                stop("Invalid state.")
        }
        if(is.na(match(outcome,outcomeNames))){
                stop("Invalid outcome.")
        }
        
        ## Assign outcomenum the correct column number.
        if(outcome=="heart attack"){
                outcomenum<-11
        } else if(outcome=="pneumonia"){
                outcomenum<-23
        } else 
                outcomenum<-17
        
        ##Return hospital name in that state with lowest 30-day death
        data2 <- data[,c(2,outcomenum,7)]
        ##cleanse rows with not avaliable.
        data3<- data2[(!(data2[,2]=="Not Available")) & data2[,3]==state,]
        
        
        ##double sort.
        datafinal<-data3[order(as.numeric(data3[,2]),data3[,1],decreasing = FALSE),]
  
        
        ## rate/evaluating the input from num
        if (num=="best"){
                num<-1
        } else if (num=="worst") {
                num<-length(datafinal$Hospital.Name)
        } else if (num < 1 | num > length(datafinal$Hospital.Name)) {
                stop("NA")
        } else
                num<-num
        
        ##adding the rank and returning results.
        datafinal<-cbind(datafinal,Rank=rank(datafinal[,2],ties.method = "first"))
        datafinal[num,c(1,2,4)]
}










