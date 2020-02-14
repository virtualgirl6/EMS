# pad voor yuri: setwd("/Users/iehkaatee/Documents/UU/AI/jaar\ 3/exp\ method/Rstudio/EMS")
# pad voor vicky: setwd ("~/GitHub/EMS")

#set working directory
rm(list = ls())
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


###
###SINGLE TYPE TASK
###

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
ikiCor <- 150/nrow(tableTypeTask3)
ikiCor

ikiCornNcor <- 150/ length(tableTypeTask2$Eventmessage1)
ikiCornNcor


ikisCorrectOnly <- with(tableTypeTask3,diff(c(0,LocalTime)))
ikisCorrectOnly

ikisCorrectAndIncorrect <- with(tableTypeTask2,diff(c(0,LocalTime)))
ikisCorrectAndIncorrect


summary(sort(ikisCorrectOnly)) ;summary(sort(ikisCorrectAndIncorrect))


library(ggplot2)
allikis <- c(ikisCorrectOnly, ikisCorrectAndIncorrect)
labels <-  c(rep("onlyCorrectAnalyzed",length(ikisCorrectOnly)),rep("CorrectAndIncorrectAnalyzed",length(ikisCorrectAndIncorrect)))
frameIkis <- data.frame(allikis,labels)
iki_plot <- ggplot(frameIkis, aes(x=labels, y=allikis)) +
  geom_boxplot(stat = "boxplot")
print(iki_plot)

###
###SCRABBLE TYPE TASK
###

scrabbleTypeTask <- resultTable[resultTable$partOfExperiment == "practiceScrabble",]
scrabbleTypeTask
unique(scrabbleTypeTask$Eventmessage1)
unique(scrabbleTypeTask$Eventmessage2)

tableScrabbleTask2 <- drop.levels(scrabbleTypeTask)
sort(summary(tableScrabbleTask2[tableScrabbleTask2$Eventmessage1 == "keypressScrabble",]$Eventmessage2))

unique(tableScrabbleTask2[tableScrabbleTask2$Eventmessage2 == "correctNewWord",]$currentScrabbleLettersTyped)

tableScrabbleTask3 <- tableScrabbleTask2[tableScrabbleTask2$LocalTime <= 150,]
tableScrabbleTask3$LocalTime


library(ggplot2) ## check for more info: https://ggplot2.tidyverse.org/reference/ library(cowplot) ## for putting multiple graphs in one figure
library(cowplot) ## for plotting multiple graphs in one figure

## basic plot using ggplots' quickplot expression
qplot(tableScrabbleTask2$LocalTime,tableScrabbleTask2$nrCorrectScrabbleWords, xlab = "Time (s)", ylab = "Number of Words", xlim = c(0,150), ylim = c(0,20))

## same basic plot in standard ggplot expression
plot1 <- ggplot(tableScrabbleTask3, aes(x=LocalTime, y=nrCorrectScrabbleWords)) +
  geom_point() + 
  xlab("Time (s)") + 
  ylab("Number of Words") + 
  ggtitle("Basic plot")
print(plot1)

## Clear and simple plot in which I have influenced what values are shown on the x- and y-axes
plot2 <- ggplot(tableScrabbleTask2, aes(x=LocalTime, y=nrCorrectScrabbleWords)) +
  geom_point(shape=21,colour="black", fill="white", size = 2, stroke = 1) + #geom_line() +
  xlab("Time (s)") +
  ylab("Number of Words") +
  ggtitle("Clear plot") +
  scale_x_continuous(limits=c(0,150), breaks=seq(0,150, 25), minor_breaks = seq(0,150, 5)) +
  scale_y_continuous(limits=c(0,20), breaks=seq(0,20,5)) +
  theme_minimal() +
  theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid"))
print(plot2)

## to put them next to each other in one figure
plot_grid(plot1, plot2) ## for this function you need the cowplot package






