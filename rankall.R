
##Author: Ryan Moore
##Date: 7/22/2016


#This function ranks the hospitals based on outcomes of diagnosises in the hospital.

rankall<-function(outcome,num="best"){
        data <- read.csv("outcome-of-care-measures.csv", colClasses ="character")
        
        if(outcome=="heart attack"){
                outcomenum<-11
        } else if(outcome=="pneumonia"){
                outcomenum<-23
        } else 
                outcomenum<-17
        
        data2<- data[,c(2,7,outcomenum)]
        ##splits data by state
        s<-split(data2, data2$State)
        
        ##gets rid of all Not Availables from each of the lists
        data3<-lapply(s, function(x) x[(!(x[,3]=="Not Available")),])
        
        ##Orders each of the lists.
        data4<- lapply(data3, function(x) x[order(as.numeric(x[,3]),x[,1],decreasing = FALSE),])
      
        datafinal<-data.frame(hospital=character(),state=character(), listlength=numeric())
        
        ##find the length of each list
        finaloutput<-
        
        ## rate/evaluating the input from num
        if (num=="best"){
                num<-1
                finaloutput<-lapply(data4, function(x) x[num,c(1,2)])
        } else if (num=="worst") {
                finaloutput<-lapply(data4, function(x) x[length(x$Hospital.Name),c(1,2)])
        } else {
                num<-num
                finaloutput<-lapply(data4, function(x) x[num,c(1,2)])
        }
        
        test<-unlist(finaloutput)
        test2<-names(finaloutput)
        
        test4<-test[seq(1,length(test),2)]
        finaloutput2<-data.frame(hospital=test4, state=test2,row.names=test2)
        finaloutput2
       }














