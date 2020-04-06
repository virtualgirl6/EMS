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



####
#### SINGLE TYPE TASK 
####
unique(allData$Eventmessage1)
test <- drop.levels(subset(allData, partOfExperiment == "practiceTyping"  & WordScore == 14 & Eventmessage1 == 'trialStop'))
tableTypeTask14 <- drop.levels(subset(allData, partOfExperiment == "practiceTyping" & Eventmessage1 == "keypress" & WordScore == 14))
tableTypeTask28 <- drop.levels(subset(allData, partOfExperiment == "practiceTyping" & Eventmessage1 == "keypress" & WordScore == 28))
nrow(tableTypeTask28)

# Average InterKeypress Interval (IKI)
iki14 <- 150/nrow(tableTypeTask14) * length(unique(tableTypeTask14$SubjectNr))
iki28 <- 150/nrow(tableTypeTask28) * length(unique(tableTypeTask28$SubjectNr))

# Letters per unit(LPU)
lpu14 <- 6/iki14
lpu28 <- 6/iki28

# Compute the actual score
unitTypeTask <- 0:25
typeTask14 = data.frame(unit = unitTypeTask)
typeTask14$points <- lpu14 * unitTypeTask

typeTask28 = data.frame(unit = unitTypeTask)
typeTask28$points <- lpu28 * unitTypeTask

iki_plot <- ggplot(typeTask14, aes(x=unit, y=points))+
  geom_point(shape=21,colour="black", fill='black', size = 1) + #geom_line() +
  xlab("Time (unit)") +
  ylab("Score") +
  ggtitle("Clear plot") 

viki_plot <- iki_plot + geom_point(data=typeTask28, colour="green",aes(x=unit, y=points));viki_plot


####
#### SCRABBLE TYPE TASK
####

# create subset of the allData dataframe needed to estimate parameters for the model.
tableScrabbleTask14 <- drop.levels(subset(allData, partOfExperiment == "practiceScrabble" 
                                        & allData$WordScore == 14  
                                        & Eventmessage2 == "correctNewWord"))

tableScrabbleTask28 <- drop.levels(subset(allData, partOfExperiment == "practiceScrabble" 
                              & allData$WordScore == 28  
                              & Eventmessage2 == "correctNewWord"))

scrabbleTypeTask <- allData[allData$partOfExperiment == "practiceScrabble" & allData$WordScore == 28 &
                              allData$Eventmessage2 == "correctNewWord",]
tableScrabbleTask2 <- drop.levels(scrabbleTypeTask)

# scale Localtime to units 
tableScrabbleTask14$unit <- tableScrabbleTask14$LocalTime / 6
tableScrabbleTask28$unit <- tableScrabbleTask28$LocalTime / 6

# creat plots for scrabble 14/28 datapoints
dataScrabbleTask14_plot <- ggplot(tableScrabbleTask14, aes(x=unit, y=nrCorrectScrabbleWords * 14)) +
  geom_point(shape=21,colour="black", fill="black", size = 1) + 
  xlab("Time (unit)") +
  ylab("Score") +
  ggtitle("Practise scrabble task (wordscore=14") +
  theme_minimal() +
  theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid"))

dataScrabbleTask28_plot <- ggplot(tableScrabbleTask28, aes(x=unit, y=nrCorrectScrabbleWords * 28)) +
  geom_point(shape=21,colour="black", fill="black", size = 1) + 
  xlab("Time (unit)") +
  ylab("Score") +
  ggtitle("Practise scrabble task (wordscore=14") +
  theme_minimal() +
  theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid"))

# create model
time <- seq(0,150,6)  ## time steps of 6 seconds
unit <- time / 6

# model: easy wordscore 14
scrabbleModel14 <- data.frame(unit = unit)
scrabbleModel14$wordsPerUnitEasy <- (1.55 * 14) * 0.89^(scrabbleModel14$unit)
scrabbleModel14$EasyWords <- cumsum(scrabbleModel14$wordsPerUnitEasy)
scrabble14Easy <- dataScrabbleTask14_plot + geom_point(data=scrabbleModel14, aes(x = unit,y=EasyWords, colour="easy"));scrabble14Easy

# model: hard wordscore 14
scrabbleModel14$wordsPerUnitHard <- (0.55 * 14) * 0.95^(scrabbleModel14$unit)
scrabbleModel14$HardWords <- cumsum(scrabbleModel14$wordsPerUnitHard)
scrabble14EasyHard <- scrabble14Easy + geom_point(data=scrabbleModel14, aes(x = unit,y=HardWords, colour="hard"));scrabble14EasyHard

# model: easy wordscore 28
scrabbleModel28 <- data.frame(unit = unit)
scrabbleModel28$wordsPerUnitEasy <- (1.35 * 28) * 0.905^(scrabbleModel28$unit)
scrabbleModel28$EasyWords <-cumsum(scrabbleModel28$wordsPerUnitEasy)
scrabble28Easy <- dataScrabbleTask28_plot + geom_point(data=scrabbleModel28, aes(x = unit,y=EasyWords, colour="easy"))

# model: hard wordscore 28
scrabbleModel28$wordsPerUnitHard <- (0.49 * 28) * 0.95^(scrabbleModel28$unit)
scrabbleModel28$HardWords <- cumsum(scrabbleModel28$wordsPerUnitHard)
scrabble28EasyHard <- scrabble28Easy + geom_point(data=scrabbleModel28, aes(x = unit,y=HardWords, colour="hard"));scrabble28EasyHard

# $PercentageHard <- PayneTable$DifficultWords + rev(PayneTable$EasyWords)

####
#### Combining scrabble with type taske to determine optimal scores
####
scrabbleModel14$PercentageScrabbleEasy <- scrabbleModel14$EasyWords + rev(typeTask14$points)
scrabbleModel14$PercentageScrabbleHard <- scrabbleModel14$HardWords + rev(typeTask14$points)
scrabbleModel28$PercentageScrabbleEasy <- scrabbleModel28$EasyWords + rev(typeTask28$points)
scrabbleModel28$PercentageScrabbleHard <- scrabbleModel28$HardWords + rev(typeTask28$points)

optimalScoresPlot <- ggplot(scrabbleModel14, aes(x=unit, y=PercentageScrabbleEasy)) +
  geom_point(shape=21,colour="black", fill="black", size = 1) + 
  xlab("Time (percentage)") +
  ylab("Score") +
  ggtitle("Percentage spent on scrabble vs type task") +
  theme_minimal() +
  theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid"))

optimalScoresPlot1 <- optimalScoresPlot + geom_point(data=scrabbleModel14, aes(x = unit,y=PercentageScrabbleHard, colour="hard14"))

optimalScoresPlot2 <- optimalScoresPlot1 + geom_point(data=scrabbleModel28, aes(x = unit,y=PercentageScrabbleEasy, colour="easy28"))
optimalScoresPlot3 <- optimalScoresPlot2 + geom_point(data=scrabbleModel28, aes(x = unit,y=PercentageScrabbleHard, colour="hard28"))





####
#### step 2. Analyse the experimental data. (zoals in 1C)
####


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