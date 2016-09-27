
##Author: Ryan Moore
##Date: 7/22/2016

#This script reads in multiple .csv files and allows you to get the mean for values of user inputted number of pollution monitors.

specdata<-"C:/Users/Alaroc/Downloads/Coursera/R/rprog%2Fdata%2Fspecdata/specdata"

pollutantmean<- function(directory, pollutant, id=1:332){
        setwd(directory)
        datafinal<- data.frame(Date=as.Date(character()),sulfate=numeric(),nitrate=numeric())
        
        for (input in id){
                filename<-paste(str_pad(input,3,pad="0"), ".csv", sep="")
                data_add<-read.csv(filename)
                datafinal<- rbind(datafinal,data_add)
        }
        mean(datafinal[,pollutant], na.rm=TRUE)
}





