####
#### R code for Assignment 2B
#### Vicky Bosch 5934389
#### Yuri Teerlink 5987148
####

# load libraries 
library(ggplot2) 
library(cowplot)
library(gdata)
source("usefulFunctions.R")

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

# plot for typetask
iki_plot <- ggplot(typeTask14, aes(x=unit, y=points))+
  geom_point(shape=21,colour="black", fill='black', size = 1) + 
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

####
#### create model for scrabble task
####

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
scrabbleModel28$wordsPerUnitEasy <- (1.51 * 28) * 0.89^(scrabbleModel28$unit)
scrabbleModel28$EasyWords <-cumsum(scrabbleModel28$wordsPerUnitEasy)
scrabble28Easy <- dataScrabbleTask28_plot + geom_point(data=scrabbleModel28, aes(x = unit,y=EasyWords, colour="easy"))

# model: hard wordscore 28
scrabbleModel28$wordsPerUnitHard <- (0.49 * 28) * 0.95^(scrabbleModel28$unit)
scrabbleModel28$HardWords <- cumsum(scrabbleModel28$wordsPerUnitHard)
scrabble28EasyHard <- scrabble28Easy + geom_point(data=scrabbleModel28, aes(x = unit,y=HardWords, colour="hard"));scrabble28EasyHard

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
optimalScoresPlot3 <- optimalScoresPlot2 + geom_point(data=scrabbleModel28, aes(x = seq(0,100,4),y=PercentageScrabbleHard, colour="hard28"))

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
  dataEasy14[ , i] <- cumsum((rnorm(1, 1.55, 1.07/10) * 14)* 0.89^(unitTypeTask)) 
  dataHard14[ , i] <- cumsum((rnorm(1, 0.55, 0.69/10) * 14)* 0.95^(unitTypeTask))
  dataEasy28[ , i] <- cumsum((rnorm(1, 1.51, 1.07/10) * 28)* 0.89^(unitTypeTask)) 
  dataHard28[ , i] <- cumsum((rnorm(1, 0.49, 0.69/10) * 28)* 0.95^(unitTypeTask))
  dataType14[ , i] <- rnorm(1, lpu14, sdIki14) * unitTypeTask
  dataType28[ , i] <- rnorm(1, lpu28, sdIki28) * unitTypeTask
} 

#Compute means and SDs of model
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


# Plot to show percentage of time spent on scrabble task and score achieved
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

# Combine all possible scores Scrabble + Typing task
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
    combinedEasy28 <- cbind(combinedEasy28, rev(dataType28[,i]) + dataEasy28[,j])
    combinedHard28 <- cbind(combinedHard28, rev(dataType28[,i]) + dataHard28[,j])
  }
}

# Compute mean, sd, se and ci of each of the combinations obtained in the simulation.
# Easy14 Model, 95/ CI = [341.55 , 341.8008], M = 341.6754, SD = 9.596098
combinedEasy14$rowMean <- apply(combinedEasy14[,2:22501],1, mean)
combinedEasy14$rowSD <- apply(combinedEasy14[,2:22501], 1, sd)
meanEasy14Model <- max(combinedEasy14$rowMean)
indexMaxMeanEasy14Model <- which(combinedEasy14$rowMean == meanEasy14Model)
sdEasy14Model <- combinedEasy14$rowSD[indexMaxMeanEasy14Model]
seEasy14Model<- sdEasy14Model/sqrt(22500)
ciEasy14Model <- 1.96 * seEasy14Model
endTableRowEasy14Model <- data.frame(combination='Easy14', dataType='Model', mean=meanEasy14Model, sd=sdEasy14Model, se=seEasy14Model, ci=ciEasy14Model)

# Hard14 Model, 95/ CI = [311.0371 , 311.2745], M = 311.1558, SD = 9.081792
combinedHard14$rowMean <- apply(combinedHard14[,2:22501],1, mean)
combinedHard14$rowSD <- apply(combinedHard14[,2:22501], 1, sd)
meanHard14Model <- max(combinedHard14$rowMean)
indexMaxMeanHard14Model <- which(combinedHard14$rowMean == meanHard14Model)
sdHard14Model <- combinedHard14$rowSD[indexMaxMeanHard14Model]
seHard14Model<- sdHard14Model/sqrt(22500)
ciHard14Model <- 1.96 * seHard14Model
endTableRowHard14Model <- data.frame(combination='Hard14', dataType='Model', mean=meanHard14Model, sd=sdHard14Model, se=seHard14Model, ci=ciHard14Model)

# Easy28 Model, 95/ CI = [466.1798 , 466.7043], M = 466.4421, SD = 20.07147
combinedEasy28$rowMean <- apply(combinedEasy28[,2:22501],1, mean)
combinedEasy28$rowSD <- apply(combinedEasy28[,2:22501], 1, sd)
meanEasy28Model <- max(combinedEasy28$rowMean)
indexMaxMeanEasy28Model <- which(combinedEasy28$rowMean == meanEasy28Model)
sdEasy28Model <- combinedEasy28$rowSD[indexMaxMeanEasy28Model]
seEasy28Model<- sdEasy28Model/sqrt(22500)
ciEasy28Model <- 1.96 * seEasy28Model
endTableRowEasy28Model <- data.frame(combination='Easy28', dataType='Model', mean=meanEasy28Model, sd=sdEasy28Model, se=seEasy28Model, ci=ciEasy28Model)

# Hard28 Model, 95/ CI = [326.8449 , 327.1006], M = 326.9727, SD = 9.78315
combinedHard28$rowMean <- apply(combinedHard28[,2:22501],1, mean)
combinedHard28$rowSD <- apply(combinedHard28[,2:22501], 1, sd)
meanHard28Model <- max(combinedHard28$rowMean)
indexMaxMeanHard28Model <- which(combinedHard28$rowMean == meanHard28Model)
sdHard28Model <- combinedHard28$rowSD[indexMaxMeanHard28Model]
seHard28Model<- sdHard28Model/sqrt(22500)
ciHard28Model <- 1.96 * seHard28Model
endTableRowHard28Model <- data.frame(combination='Hard28', dataType='Model', mean=meanHard28Model, sd=sdHard28Model, se=seHard28Model, ci=ciHard28Model)

# mean, sd, se and ci of each of the combinations obtained in the experiment 
score14DataCorrect <- subset(allData, partOfExperiment=="dualTask" & Eventmessage1 == "scoreGiven" & WordScore == 14)
score28DataCorrect <- subset(allData, partOfExperiment=="dualTask" & Eventmessage1 == "scoreGiven" & WordScore == 28)
score14EndOnly<- ddply(score14DataCorrect, c("scrabbleCondition", "SubjectNr"), summarise, 
                       gemiddelde = mean(CurrentScore, na.rm=TRUE))

score28EndOnly<- ddply(score28DataCorrect, c("scrabbleCondition", "SubjectNr"), summarise, 
                       gemiddelde = mean(CurrentScore, na.rm=TRUE))


summaryScrabble14 <- summarySEwithin(score14EndOnly , measurevar="gemiddelde", withinvars=c("scrabbleCondition"))
summaryScrabble28 <- summarySEwithin(score28EndOnly , measurevar="gemiddelde", withinvars=c("scrabbleCondition"))

# Easy14 Data, 95% CI = [307.613, 364.7828]
meanEasy14Data <- subset(summaryScrabble14, scrabbleCondition == 'easy')$gemiddelde
sdEasy14Data <- subset(summaryScrabble14, scrabbleCondition == 'easy')$sd
seEasy14Data<- subset(summaryScrabble14, scrabbleCondition == 'easy')$se
ciEasy14Data <- subset(summaryScrabble14, scrabbleCondition == 'easy')$ci
endTableRowEasy14Data <- data.frame(combination='Easy14', dataType='Experiment', mean=meanEasy14Data, sd=sdEasy14Data, se=seEasy14Data, ci=ciEasy14Data)

# Hard14 Data, 95% CI = [253.6277, 302.2056]
meanHard14Data <- subset(summaryScrabble14, scrabbleCondition == 'hard')$gemiddelde
sdHard14Data <- subset(summaryScrabble14, scrabbleCondition == 'hard')$sd
seHard14Data<- subset(summaryScrabble14, scrabbleCondition == 'hard')$se
ciHard14Data <- subset(summaryScrabble14, scrabbleCondition == 'hard')$ci
endTableRowHard14Data <- data.frame(combination='Hard14', dataType='Experiment', mean=meanHard14Data, sd=sdHard14Data, se=seHard14Data, ci=ciHard14Data)

# Easy28 Data, 95% CI = [447.3396, 531.5979]
meanEasy28Data <- subset(summaryScrabble28, scrabbleCondition == 'easy')$gemiddelde
sdEasy28Data <- subset(summaryScrabble28, scrabbleCondition == 'easy')$sd
seEasy28Data<- subset(summaryScrabble28, scrabbleCondition == 'easy')$se
ciEasy28Data <- subset(summaryScrabble28, scrabbleCondition == 'easy')$ci
endTableRowEasy28Data <- data.frame(combination='Easy28', dataType='Experiment', mean=meanEasy28Data, sd=sdEasy28Data, se=seEasy28Data, ci=ciEasy28Data)

# Hard28 Data, 95% CI = [317.3341, 374.2075]
meanHard28Data <- subset(summaryScrabble28, scrabbleCondition == 'hard')$gemiddelde
sdHard28Data <- subset(summaryScrabble28, scrabbleCondition == 'hard')$sd
seHard28Data<- subset(summaryScrabble28, scrabbleCondition == 'hard')$se
ciHard28Data <- subset(summaryScrabble28, scrabbleCondition == 'hard')$ci
endTableRowHard28Data <- data.frame(combination='Hard28', dataType='Experiment', mean=meanHard28Data, sd=sdHard28Data, se=seHard28Data, ci=ciHard28Data)

combinedDataPlot <- ggplot(combinedEasy14) +
  xlab("Time spent on hard Task") +
  ylab("Number of Words") +
  ggtitle("Aver") +
  theme_minimal() +
  theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid"))


combWithLine <- combinedDataPlot + geom_point(data=combinedEasy14[,22501:22503], aes(x = 0:25, y=rowMean, colour='combinedEasy14'))  + 
                geom_errorbar(data=combinedEasy14[,22501:22503], aes(x=0:25, ymin=rowMean - rowSD , ymax=rowMean + rowSD))
combWithLine <- combWithLine + geom_point(data=combinedHard14[,22501:22503], aes(x = 0:25, y=rowMean, colour='combinedHard14'))  + 
                geom_errorbar(data=combinedHard14[,22501:22503], aes(x=0:25, ymin=rowMean - rowSD , ymax=rowMean + rowSD))
combWithLine <- combWithLine + geom_point(data=combinedEasy28[,22501:22503], aes(x = 0:25, y=rowMean, colour='combinedEasy28'))  + 
                geom_errorbar(data=combinedEasy28[,22501:22503], aes(x=0:25, ymin=rowMean - rowSD , ymax=rowMean + rowSD))
combWithLine <- combWithLine + geom_point(data=combinedHard28[,22501:22503], aes(x = 0:25, y=rowMean, colour='combinedHard28'))  + 
                geom_errorbar(data=combinedHard28[,22501:22503], aes(x=0:25, ymin=rowMean - rowSD , ymax=rowMean + rowSD))

####
#### Creating the bar plot
####

endTabelAllData <- rbind(endTableRowEasy14Data, endTableRowHard14Data, endTableRowEasy28Data, endTableRowHard28Data,
                         endTableRowEasy14Model, endTableRowHard14Model, endTableRowEasy28Model, endTableRowHard28Model)

# bar plot model + experiment data
finalBarplot <- ggplot(endTabelAllData, aes(fill=dataType , y=mean, x=combination)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2, position=position_dodge(.9))+
  scale_fill_grey() +
  ylab("Average Score") +
  xlab("Task Difficulty and Word Score") +
  theme(legend.title = element_blank()) 
finalBarplot

# bar plot experiment data
summaryScrabble14$wordScore <- 'Word Score 14'
summaryScrabble28$wordScore <- 'Word Score 28'
endTabelExperimentData <- rbind(summaryScrabble14, summaryScrabble28)
endTabelExperimentData$Label <- NA
endTabelExperimentData[endTabelExperimentData$scrabbleCondition == 'easy',]$Label <- 'Easy'
endTabelExperimentData[endTabelExperimentData$scrabbleCondition == 'hard',]$Label <- 'Hard'

finalPlotExperiment <- ggplot(endTabelExperimentData, aes(fill=Label , y=gemiddelde, x=wordScore)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=gemiddelde-sd, ymax=gemiddelde+sd), width=.2, position=position_dodge(.9))+
  scale_fill_grey() +
  ylab("Average Score") +
  xlab("Task Difficulty and Word Score") +
  theme(legend.title = element_blank()) 


####
#### analyses: T-test, RMSE and R^2
####


# T-test
with(score14EndOnly,t.test(gemiddelde~ scrabbleCondition,paired=TRUE))
with(score28EndOnly,t.test(gemiddelde~ scrabbleCondition,paired=TRUE))

# Effectsize
averageSqrt14Data <- sqrt((sdEasy14Data**2 + sdHard14Data**2)/2)
effectSize14Data <- (meanEasy14Data - meanHard14Data)/averageSqrt14Data

averageSqrt28Data <- sqrt((sdEasy28Data**2 + sdHard28Data**2)/2)
effectSize28Data <- (meanEasy28Data - meanHard28Data)/averageSqrt28Data

# RMSE - 22.46352 - 6.21%
RMSE <- sqrt(((meanEasy14Data - meanEasy14Model)^2 + (meanHard14Data - meanHard14Model)^2 + 
                (meanEasy28Data - meanEasy28Model)^2 + (meanHard28Data - meanHard28Model)^2) / 4)
percentageRMSEOfTotal <- (RMSE/((meanEasy14Data + meanEasy14Model + meanHard14Data + meanHard14Model +
                               meanEasy28Data + meanEasy28Model + meanHard28Data + meanHard28Model)/8)) * 100
# R^2: 0.9752878
meanModel <- c(meanEasy14Model, meanHard14Model, meanEasy28Model, meanHard28Model)
meanData <- c(meanEasy14Data, meanHard14Data, meanEasy28Data, meanHard28Data)
RxR <- cor(meanModel, meanData)




