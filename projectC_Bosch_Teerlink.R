## Yuri Teerlink 5987148
## Vicky Bosch 5934389

# pad voor yuri: setwd("/Users/iehkaatee/Documents/UU/AI/jaar\ 3/exp\ method/Rstudio/EMS")
# pad voor vicky: setwd ("~/GitHub/EMS")

#set working directory
rm(list = ls())
setwd("~/GitHub/EMS")

#load .csv file 
resultTable <- read.csv("participant7data.csv", header=TRUE,sep="")
resultTable

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

library(ggplot2) 
library(cowplot) 

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
plot3 <- ggplot(tableScrabbleTask3, aes(x=LocalTime, y=nrCorrectScrabbleWords)) +
  geom_point(shape=21,colour="black", fill="white", size = 2, stroke = 1) + #geom_line() +
  xlab("Time (s)") +
  ylab("Number of Words") +
  ggtitle("Clear plot") +
  scale_x_continuous(limits=c(0,600), breaks=seq(0,600, 25), minor_breaks = seq(0,600, 5)) +
  scale_y_continuous(limits=c(0,35), breaks=seq(0,35,5)) +
  theme_minimal() +
  theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid"))
print(plot3)

## to put them next to each other in one figure
plot_grid(plot1, plot2) ## for this function you need the cowplot package

### compare to Payne's data
time <- seq(1,600,6)  ## time steps of 6 seconds
PayneTable <- data.frame(time = time)
PayneTable $unit <- PayneTable $time / 6
PayneTable $wordsPerUnitEasy <- 0.65* 0.981^(PayneTable $unit-1) 
PayneTable $wordsPerUnitDifficult <- 0.51* 0.965^(PayneTable $unit-1)
PayneTable $EasyWords <- cumsum(PayneTable $wordsPerUnitEasy) 
PayneTable $DifficultWords <- cumsum(PayneTable $wordsPerUnitDifficult)

plot4 <- plot3 + geom_point(data=PayneTable, aes(x = time,y=EasyWords, colour="easy")) 
plot5 = plot4 + geom_point(data=PayneTable, aes(x = time,y=DifficultWords, colour="hard"))
print (plot5)

#################

allData <- read.csv("allData.Rdata", header=TRUE,sep="")
summary(allData)

head(allData)


### hypothesis 
#  1. What is the independent variable? What "levels" does this variable have?
#  2. What is the dependent variable?
#  3. Based on literature (e.g., the Payne et al article that was discussed in class): what
# would you expect to happen if there is a significant effect for this dependent and independent variable? Stated differently: what do you expect that the H0 is, and what is the alternative hypothesis (H1)?
  





