# pad voor yuri: setwd("/Users/iehkaatee/Documents/UU/AI/jaar\ 3/exp\ method/Rstudio/EMS")
# pad voor vicky: setwd ("~/GitHub/EMS")

#set working directory
setwd("/Users/iehkaatee/Documents/UU/AI/jaar\ 3/exp\ method/Rstudio/EMS")

#load .csv file 
resultTable <- read.csv("participant52data.csv", header=TRUE,sep="")
resultTable

###
### Assignment 1.
###

#1 + 2.
# - levels(): the levels function takes in a "factor" data structure therefore this function call
# only works in the following way: levels(factor(resultTable[,2])). Once the levels function is called
# it returns the different values that are stored in the given column.

# - summary(): summary returns an overview of some of the attributes stored under each column. These attributes 
# differ per datastructe stored in the column e.g. if the column contains only numerical values it will return
# the min, 1st quadrant, median, mean, 3rd quadrant and the max. Else if the column contains a word, like easy, 
# medium, hard, it returns the number of occurances.

# - unique(): simmiliar like the levels-function unique() returns all the uniquely different values found in 
# a given column

#3 + 4.
# nominal, ordinal,binary/dichotomous, interval, and ratio.
#A. LocalTime - interval
#B. partOfExperiment - nominal
#C. scrabbleWindowVisible - binary
#D. scrabbleCondition - ordinal
#E. nrCorrectScrabbleWords - interval/ratio
#F. Eventmessage1 - nominal
#G. Eventmessage2 - nominal !! misschien ratio? !!

#6. 
# levels(), summary(), unique()
#A. LocalTime - interval
#B. partOfExperiment - nominal
#C. scrabbleWindowVisible - binary
#D. scrabbleCondition - ordinal
#E. nrCorrectScrabbleWords - interval/ratio
#F. Eventmessage1 - nominal
#G. Eventmessage2 - nominal !! misschien ratio? !!

