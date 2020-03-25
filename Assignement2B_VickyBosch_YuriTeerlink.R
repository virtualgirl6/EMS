# load libraries 
library(ggplot2) 
library(cowplot)
library(gdata)
# clear plot
dev.off() 

# clear variables 
rm(list = ls()) 

#set working directory
# pad voor yuri: setwd("/Users/iehkaatee/Documents/UU/AI/jaar\ 3/exp\ method/Rstudio/EMS")
# pad voor vicky: setwd ("~/GitHub/EMS")
setwd("/Users/iehkaatee/Documents/UU/AI/jaar\ 3/exp\ method/Rstudio/EMS")

# load data from experiment 
load("/Users/iehkaatee/Documents/UU/AI/jaar\ 3/exp\ method/Rstudio/EMS/allData.Rdata")
summary(allData)

colnames(allData)
length(unique(score28Data$SubjectNr)) #deelnemers:=  score:aantal, 7:59, 14:48, 28:48

unique(allData$SubjectNr)
unique(score7Data$WordScore)

#data gesplitst per wordscore
score7Data <- allData[allData$WordScore == 7,]
score14Data <- allData[allData$WordScore == 14,]
score28Data <- allData[allData$WordScore == 28,]

###
###SINGLE TYPE TASK 
###

tableTypeTask14 <- score14Data[score14Data$partOfExperiment == "practiceTyping",]
tableTypeTask14

unique(tableTypeTask14$Eventmessage1)
test <- allData[allData$Eventmessage1 == "scoreGiven",]
unique(allData$SubjectNr)
tableTypeTask141 <- tableTypeTask14[tableTypeTask14$Eventmessage1 == "trailStop",]

nrow(tableTypeTask143[tableTypeTask143$Eventmessage1 =="keypress",])
nrow(tableTypeTask14[tableTypeTask14$Eventmessage1 =="keypress",])
nrow(tableTypeTask142[tableTypeTask142$Eventmessage1 =="keypress",])
ikiCor <- 150/nrow(tableTypeTask14)
ikiCor

ikiCornNcor <- 150/ length(tableTypeTask14$Eventmessage1)
ikiCornNcor

ikisCorrectOnly <- with(tableTypeTask14,diff(c(0,LocalTime)))
ikisCorrectOnly

ikisCorrectAndIncorrect <- with(tableTypeTask14,diff(c(0,LocalTime)))
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

scrabbleTypeTask <- allData[allData$partOfExperiment == "practiceScrabble" & allData$WordScore == 28 &
                              allData$Eventmessage2 == "correctNewWord",]
tableScrabbleTask2 <- drop.levels(scrabbleTypeTask)

#summary(tableScrabbleTask2[tableScrabbleTask2$Eventmessage1 == "scoreGiven",])

#unique(tableScrabbleTask2[tableScrabbleTask2$Eventmessage2 == "correctNewWord",]$currentScrabbleLettersTyped)

tableScrabbleTask3 <- tableScrabbleTask2[tableScrabbleTask2$LocalTime <= 150,]
tableScrabbleTask3$unit <- tableScrabbleTask3$LocalTime / 6
unique(tableScrabbleTask2$scrabbleCondition)

## Clear and simple plot in which I have influenced what values are shown on the x- and y-axes
plot3 <- ggplot(tableScrabbleTask3, aes(x=unit, y=CurrentScore)) +
  geom_point(shape=21,colour="black", fill="black", size = 1) + #geom_line() +
  xlab("Time (unit)") +
  ylab("Score") +
  ggtitle("Clear plot") +
  scale_x_continuous(limits=c(0,25), breaks=seq(0,25, 25), minor_breaks = seq(0,150, 5)) +
  scale_y_continuous(limits=c(0,600), breaks=seq(0,600,50)) +
  theme_minimal() +
  theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid"))
print(plot3)

### compare to Payne's data
time <- seq(1,600,6)  ## time steps of 6 seconds
PayneTable <- data.frame(time = time)
PayneTable $unit <- PayneTable $time / 6
PayneTable $wordsPerUnitEasy <- 0.65* 0.981^(PayneTable $unit-1) 
PayneTable $wordsPerUnitDifficult <- 0.51* 0.965^(PayneTable $unit-1)
PayneTable $EasyWords <- cumsum(PayneTable $wordsPerUnitEasy) 
PayneTable $DifficultWords <- cumsum(PayneTable $wordsPerUnitDifficult)

plot4 <- plot3 + geom_point(data=PayneTable, aes(x = unit,y=EasyWords, colour="easy")) 
plot5 <- plot4 + geom_point(data=PayneTable, aes(x = unit,y=DifficultWords, colour="hard"))
print (plot5)




####
#### step 2. Analyse the experimental data. (zoals in 1C)
####


#data opgedeeld op basis van woordwaarde(score14Data // score28Data) en easy/hard:

#easyscore14Data
#hardscore14Data
#easyscore28Data
#hardscore28Data
length(unique(score28Data$SubjectNr))
score14DataCorrect <- score14Data[score14Data$partOfExperiment=="dualTask" & 
                                    score14Data$Eventmessage1 == "scoreGiven",]

score28DataCorrect <- score28Data[score28Data$partOfExperiment=="dualTask" & 
                                    score28Data$Eventmessage1 == "scoreGiven",]

score14EndOnly<- ddply(score14DataCorrect, c("scrabbleCondition", "SubjectNr"), summarise, 
                          gemiddelde = mean(nrCorrectScrabbleWords, na.rm=TRUE),
                          maximum = max(nrCorrectScrabbleWords),
                          minimum = min(nrCorrectScrabbleWords),
                          eerste = head(nrCorrectScrabbleWords, n=1))

score28EndOnly<- ddply(score28DataCorrect, c("scrabbleCondition", "SubjectNr"), summarise, 
                          gemiddelde = mean(nrCorrectScrabbleWords, na.rm=TRUE),
                          maximum = max(nrCorrectScrabbleWords),
                          minimum = min(nrCorrectScrabbleWords),
                          eerste = head(nrCorrectScrabbleWords, n=1))


source("usefulFunctions.R")

summaryScrabble14 <- summarySEwithin(score14EndOnly , measurevar="gemiddelde", withinvars=c("scrabbleCondition"))
summaryScrabble28 <- summarySEwithin(score28EndOnly , measurevar="gemiddelde", withinvars=c("scrabbleCondition"))

g1 <- ggplot(summaryScrabble14, aes(x=scrabbleCondition, y=gemiddelde)) +
  ylim(0,20) + 
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=gemiddelde-se, ymax=gemiddelde+se), width=.2) +
  ylab("Average Number of Words") +
  xlab("Task Difficulty") 

g2 <- ggplot(summaryScrabble28, aes(x=scrabbleCondition, y=gemiddelde)) +
  ylim(0,20) + 
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=gemiddelde-se, ymax=gemiddelde+se), width=.2) +
  ylab("Average Number of Words") +
  xlab("Task Difficulty") 

print(plot_grid(g1,g2, ncol = 2))


#verder met stap3