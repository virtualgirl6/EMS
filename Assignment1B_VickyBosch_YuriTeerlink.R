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
#E. nrCorrectScrabbleWords - ratio
#F. Eventmessage1 - nominal
#G. Eventmessage2 - nominal

#6. 
# levels(), summary(), unique()
# in general if the data stored is a factor it is usefull to use levels() to give an idea of the different values
# stored, if we want more information, for instance the number of occurances of each of these values
# we would use summary(). If the data stored is an int or numerical values it is usefull to use summary(),
# this will give a general overview of the range of values.
#A. LocalTime - summary() : gives an overview of the time that has passed during the experiment
#B. partOfExperiment - levels() : shows what are the different parts of experiment, summary(): shows occurances of those
#C. scrabbleWindowVisible - levels()
#D. scrabbleCondition - levels()
#E. nrCorrectScrabbleWords - summary()
#F. Eventmessage1 - levels()
#G. Eventmessage2 - summary()


tableTypeTask <- resultTable[resultTable$partOfExperiment == "practiceTyping",]
tableTypeTask

unique(tableTypeTask2$Eventmessage1)

library(gdata)
tableTypeTask2 <- drop.levels(tableTypeTask)

unique(tableTypeTask2$Eventmessage1)
summary(tableTypeTask2$Eventmessage1)
# #-correct: 291, #-wrong: 4

tableTypeTask3 <- tableTypeTask2[tableTypeTask2$Eventmessage1 == "keypress" &  tableTypeTask2$LocalTime <= 150,]

nrow(tableTypeTask3[tableTypeTask3$Eventmessage1 =="keypress",])
ikiCor <- 150/nrow(tableTypeTask3[tableTypeTask3$Eventmessage1 =="keypress",])
ikiCor

ikiCornNcor <- 150/ length(tableTypeTask2$Eventmessage1)
ikiCornNcor


ikisCorrectOnly <- with(tableTypeTask2[tableTypeTask3$Eventmessage1 =="keypress",],diff(c(0,LocalTime)))
ikisCorrectOnly

ikisCorrectAndIncorrect <- with(tableTypeTask$Eventmessage1,diff(c(0,LocalTime)))
ikisCorrectAndIncorrect


