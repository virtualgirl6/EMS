library(ggplot2) 
library(cowplot) 

rm(list = ls())
setwd("~/GitHub/EMS")

time <- seq(1,600,6)  ## time steps of 6 seconds
PayneTable <- data.frame(time = time)
PayneTable $unit <- PayneTable $time / 6
PayneTable $wordsPerUnitEasy <- 0.65* 0.981^(PayneTable $unit-1) 
PayneTable $wordsPerUnitDifficult <- 0.51* 0.965^(PayneTable $unit-1)
PayneTable $EasyWords <- cumsum(PayneTable $wordsPerUnitEasy) 
PayneTable $DifficultWords <- cumsum(PayneTable $wordsPerUnitDifficult)
PayneTable $PercentageHard <- PayneTable$DifficultWords + rev(PayneTable$EasyWords)


plot1 <- ggplot(PayneTable) +
  xlab("Time (m)") +
  ylab("Number of Words") +
  ggtitle("Clear plot") +
  scale_x_continuous(limits=c(0,100), breaks=seq(0,100,10)) +
  scale_y_continuous(limits=c(0,35), breaks=seq(0,35,5)) +
  theme_minimal() +
  theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid"))

ploteasy <-  plot1 + geom_point(data=PayneTable, aes(x = unit,y=EasyWords, colour="easy")) 
plothard <- ploteasy + geom_point(data=PayneTable, aes(x = unit,y=DifficultWords, colour="hard"))

print (plothard)

plot2 <- ggplot(PayneTable) +
  geom_point(data=PayneTable, aes(x = unit,y=PercentageHard, colour="Green")) +
  xlab("Time spent on hard Task") +
  ylab("Number of Words") +
  ggtitle("Aver") +
  scale_x_continuous(limits=c(0,100), breaks=seq(0,100,10)) +
  scale_y_continuous(limits=c(0,40), breaks=seq(0,40,5)) +
  theme_minimal() +
  theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid"))
print(plot2)


#### step 4 ######

#for (i in 1:150)
#{
#  dataEasy[ , i] <- rnorm(1, 0.65, 1.07/10)* 0.981^(dataEasy$unit-1) 
#  dataHard[ , i] <- rnorm(1, 0.52, 0.69/10)* 0.965^(dataHard$unit-1)
#} 
##
#for (i in 1:100)
#{
#  easyRnorm <- rnorm(1, mean = 0.65, sd = 1.07/10/6)
  ##hardRnorm <- rnorm(1,mean = 0.52, sd = 0.69/10/6)
#  dataEasy[0, i] <- data.frame(easyRnorm , PayneTable$EasyWords[i])
 # dataHard[0, i] <- data.frame(hardRnorm, PayneTable$DifficultWords[i])
#}

#ipv iedere keer opnieuw dataframe maken in de for, data toevoegen in de forloop!
dataEasy <- data.frame(unit = PayneTable $unit, "numberz")
dataHard = data.frame(unit = PayneTable $unit, "numbers")

for (i in 1:100)
{
  EasyRnorm <-rnorm(1, mean = 0.65, sd = 1.07/10) * 0.981^(dataEasy$unit-1)
  HardRnorm <- rnorm(1,mean = 0.52, sd = 0.69/10) * 0.965^(dataHard$unit-1)
  #dataEasy[i] <- data.frame(i,  EasyRnorm[i])
  #dataHard[i] <- data.frame(i, HardRnorm[i])
  dataEasy$numberz <- EasyRnorm
  dataHard$numbers <- HardRnorm
}

#??? weet niet waar dit voor diende
print(dataEasy)
dataEasy$EasyWords <- cumsum(dataEasy) 
dataHard$DifficultWords <- cumsum(dataHard)
dataEasy$EasyWords
####

emptyplot <- ggplot(PayneTable) +
  xlab("units") +
  ylab("numberz") +
  geom_errorbar(aes(x = unit, ymin=0.65 - 1.07/10 , ymax=0.65 + 1.07/10), width=.2)  +
  ggtitle("Clear plot") +
  scale_x_continuous(limits=c(0,100), breaks=seq(0,100,10)) +
  scale_y_continuous(limits=c(0,1), breaks=seq(0,1,0.05)) +
  theme_minimal() +
  theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid"))


#ploteasy2 <- ggplot(dataEasy, aes(x=numberz, y=unit, colour = "easy")) +
 # geom_bar(stat="identity") + geom_errorbar(aes(ymin=0.65 - 1.07/10/6 , ymax=0.65 + 1.07/10/6), width=.2) + 
 # ylab("i") + xlab("--") + ylim(0,150) + ggtitle("dataeasy")

ploteasyaaa <- emptyplot + geom_point(data=dataEasy, aes(x=unit, y=numberz, colour = "easy")) 

plothard2 <- ploteasyaaa + geom_point(data=dataHard, aes(x=unit, y=numbers, colour="hard")) 
print (plothard2)