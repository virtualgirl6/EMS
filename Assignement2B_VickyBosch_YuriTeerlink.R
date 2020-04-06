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

# create two practicetype subsets of allData one for each wordscore
tableTypeTask14 <- drop.levels(subset(allData, partOfExperiment == "practiceTyping" & Eventmessage1 == "keypress" & WordScore == 14))
tableTypeTask28 <- drop.levels(subset(allData, partOfExperiment == "practiceTyping" & Eventmessage1 == "keypress" & WordScore == 28))

# compute time between two correct keypresses
ikisCorrectOnly14 <- with(tableTypeTask14, diff(c(0,LocalTime)))
ikisCorrectOnly28 <- with(tableTypeTask28, diff(c(0,LocalTime)))
# remove time less then 0 
ikisPositive14 <- ikisCorrectOnly14[ikisCorrectOnly14 > 0]
ikisPositive28 <- ikisCorrectOnly28[ikisCorrectOnly28 > 0]

# Average InterKeypress Interval (IKI)
iki14 <- mean(ikisPositive14)
iki28 <- mean(ikisPositive28)

# Letters per unit(LPU)
lpu14 <- 6/iki14
lpu28 <- 6/iki28

# SD for MonteCarlo
sdIki14 <- sd(ikisPositive14)
sdIki28 <- sd(ikisPositive28)

# Compute the actual score
unitTypeTask <- 0:25
typeTask14 = data.frame(unit = unitTypeTask)
typeTask14$points <- lpu14 * unitTypeTask
typeTask28 = data.frame(unit = unitTypeTask)
typeTask28$points <- lpu28 * unitTypeTask

iki_plot <- ggplot(typeTask14, aes(x=unit, y=points))+
  geom_point(shape=21,colour="black", fill='black', size = 1) + 
  xlab("Time (unit)") +
  ylab("Score") +
  ggtitle("Clear plot") 

viki_plot <- iki_plot + geom_point(data=typeTask28, colour="green",aes(x=unit, y=points));viki_plot

percentage <- seq(0,25,0.25)
test = data.frame(percentage = 0:100)
test$score <- lpu14 * percentage


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

# put everything together in a graph
optimalScoresPlot <- ggplot() +
  xlab("Time (percentage)") +
  ylab("Score") +
  ggtitle("Percentage spent on scrabble vs type task") +
  theme_minimal() +
  theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid"))
optimalScoresPlot0 <- optimalScoresPlot + geom_point(data=scrabbleModel14, aes(x = seq(0,100,4),y=PercentageScrabbleEasy, colour="easy14"))
optimalScoresPlot1 <- optimalScoresPlot0 + geom_point(data=scrabbleModel14, aes(x = seq(0,100,4),y=PercentageScrabbleHard, colour="hard14"))
optimalScoresPlot2 <- optimalScoresPlot1 + geom_point(data=scrabbleModel28, aes(x = seq(0,100,4),y=PercentageScrabbleEasy, colour="easy28"))
optimalScoresPlot3 <- optimalScoresPlot2 + geom_point(data=scrabbleModel28, aes(x = seq(0,100,4),y=PercentageScrabbleHard, colour="hard28"));optimalScoresPlot3



####
#### Adding variability using MonteCarlo-simulation
####

dataEasy14 = data.frame(unit = 0:25)
dataHard14 = data.frame(unit = 0:25)
dataEasy28 = data.frame(unit = 0:25)
dataHard28 = data.frame(unit = 0:25)
dataType14 = data.frame(unit = 0:25)
dataType28 = data.frame(unit = 0:25)
for (i in 1:150)
{
  dataEasy14[ , i] <- cumsum((rnorm(1, 1.55, 1.07/10) * 14)* 0.89^(unitTypeTask)) # note: SD from paper is in words/minute --> divide by 10 to get words/unit
  dataHard14[ , i] <- cumsum((rnorm(1, 0.55, 0.69/10) * 14)* 0.95^(unitTypeTask))
  dataEasy28[ , i] <- cumsum((rnorm(1, 1.35, 1.07/10) * 28)* 0.905^(unitTypeTask)) # note: SD from paper is in words/minute --> divide by 10 to get words/unit
  dataHard28[ , i] <- cumsum((rnorm(1, 0.49, 0.69/10) * 28)* 0.95^(unitTypeTask))
  dataType14[ , i] <- rnorm(1, lpu14, sdIki14) * unitTypeTask
  dataType28[ , i] <- rnorm(1, lpu28, sdIki28) * unitTypeTask
} 


dataEasy14$rowMean <- apply(dataEasy14,1, mean)
dataEasy14$rowSD <- apply(dataEasy14, 1, sd)
#dataEasy$cumSum <- cumsum(dataEasy$rowMean)

dataHard14$rowMean <- apply(dataHard14,1, mean)
dataHard14$rowSD <- apply(dataHard14, 1, sd)
#dataHard$cumSum <- cumsum(dataHard$rowMean)

dataEasy28$rowMean <- apply(dataEasy28,1, mean)
dataEasy28$rowSD <- apply(dataEasy28, 1, sd)
#dataEasy$cumSum <- cumsum(dataEasy$rowMean)

dataHard28$rowMean <- apply(dataHard28,1, mean)
dataHard28$rowSD <- apply(dataHard28, 1, sd)
#dataHard$cumSum <- cumsum(dataHard$rowMean)

dataType14$rowMean <- apply(dataType14,1, mean)
dataType14$rowSD <- apply(dataType14, 1, sd)

dataType28$rowMean <- apply(dataType28,1, mean)
dataType28$rowSD <- apply(dataType28, 1, sd)

plotScrabbleWithError14 <- ggplot(dataEasy14) +
  xlab("Time (m)") +
  ylab("Number of Words") +
  ggtitle("Clear plot") +
  theme_minimal() +
  theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid"))

plotScrabbleWithError14 <-  plotScrabbleWithError14 + geom_point(data=dataEasy14, aes(x=0:25, y=rowMean, colour="easy")) +
  geom_errorbar(data=dataEasy14, aes(x=0:25, ymin=rowMean - rowSD , ymax=rowMean + rowSD)) 

plotScrabbleWithError14 <- plotScrabbleWithError14 + geom_point(data=dataHard14, aes(x = 0:25, y=rowMean, colour="hard")) + 
  geom_errorbar(data=dataHard14, aes(x=0:25, ymin=rowMean - rowSD , ymax=rowMean + rowSD))
plotScrabbleWithError14

plotScrabbleWithError28 <- ggplot(dataEasy28) +
  xlab("Time (m)") +
  ylab("Number of Words") +
  ggtitle("Clear plot") +
  theme_minimal() +
  theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid"))

plotScrabbleWithError28 <-  plotScrabbleWithError28 + geom_point(data=dataEasy28, aes(x=0:25, y=rowMean, colour="easy")) +
  geom_errorbar(data=dataEasy28, aes(x=0:25, ymin=rowMean - rowSD , ymax=rowMean + rowSD)) 

plotScrabbleWithError28 <- plotScrabbleWithError28 + geom_point(data=dataHard28, aes(x = 0:25, y=rowMean, colour="hard")) + 
  geom_errorbar(data=dataHard28, aes(x=0:25, ymin=rowMean - rowSD , ymax=rowMean + rowSD))
plotScrabbleWithError28

plotTypeWithError <- ggplot(dataType14) +
  xlab("Time (m)") +
  ylab("Number of Words") +
  ggtitle("Clear plot") +
  theme_minimal() +
  theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid"))

plotTypeWithError <-  plotTypeWithError + geom_point(data=dataType14, aes(x=0:25, y=rowMean, colour="easy")) +
  geom_errorbar(data=dataType14, aes(x=0:25, ymin=rowMean - rowSD , ymax=rowMean + rowSD)) 

plotTypeWithError <- plotTypeWithError + geom_point(data=dataType28, aes(x = 0:25, y=rowMean, colour="hard")) + 
  geom_errorbar(data=dataType28, aes(x=0:25, ymin=rowMean - rowSD , ymax=rowMean + rowSD))
plotTypeWithError


combinedEasy14 = data.frame(unit = 0:25)
combinedHard14 = data.frame(unit = 0:25)
combinedEasy28 = data.frame(unit = 0:25)
combinedHard28 = data.frame(unit = 0:25)
for (i in 1:150)
{
  for(j in 1:150)
  {
    combinedEasy14 <- cbind(combinedEasy14, rev(dataType14[,i]) + dataEasy14[,j])
    combinedHard14 <- cbind(combinedHard14, rev(dataType14[,i]) + dataHard14[,j])
    combinedEasy28 <- cbind(combinedEasy28, rev(dataType28[,i]) + dataEasy14[,j])
    combinedHard28 <- cbind(combinedHard28, rev(dataType28[,i]) + dataHard14[,j])
  }
}


combinedEasy14$rowMean <- apply(combinedEasy14[,2:22501],1, mean)
combinedEasy14$rowSD <- apply(combinedEasy14[,2:22501], 1, sd)

combinedHard14$rowMean <- apply(combinedHard14[,2:22501],1, mean)
combinedHard14$rowSD <- apply(combinedHard14[,2:22501], 1, sd)

combinedEasy28$rowMean <- apply(combinedEasy28[,2:22501],1, mean)
combinedEasy28$rowSD <- apply(combinedEasy28[,2:22501], 1, sd)

combinedHard28$rowMean <- apply(combinedHard28[,2:22501],1, mean)
combinedHard28$rowSD <- apply(combinedHard28[,2:22501], 1, sd)

combinedDataPlot <- ggplot(combinedEasy14) +
  xlab("Time spent on hard Task") +
  ylab("Number of Words") +
  ggtitle("Aver") +
  theme_minimal() +
  theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid"))

print(combinedDataPlot)

combWithLine <- combinedDataPlot + geom_point(data=combinedEasy14[,22501:22503], aes(x = 0:25, y=rowMean, colour='combinedEasy14'))  + 
                geom_errorbar(data=combinedEasy14[,22501:22503], aes(x=0:25, ymin=rowMean - rowSD , ymax=rowMean + rowSD))
combWithLine <- combWithLine + geom_point(data=combinedHard14[,22501:22503], aes(x = 0:25, y=rowMean, colour='combinedHard14'))  + 
                geom_errorbar(data=combinedHard14[,22501:22503], aes(x=0:25, ymin=rowMean - rowSD , ymax=rowMean + rowSD))
combWithLine <- combWithLine + geom_point(data=combinedEasy28[,22501:22503], aes(x = 0:25, y=rowMean, colour='combinedEasy28'))  + 
                geom_errorbar(data=combinedEasy28[,22501:22503], aes(x=0:25, ymin=rowMean - rowSD , ymax=rowMean + rowSD))
combWithLine <- combWithLine + geom_point(data=combinedHard28[,22501:22503], aes(x = 0:25, y=rowMean, colour='combinedHard28'))  + 
                geom_errorbar(data=combinedHard28[,22501:22503], aes(x=0:25, ymin=rowMean - rowSD , ymax=rowMean + rowSD))
combWithLine

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