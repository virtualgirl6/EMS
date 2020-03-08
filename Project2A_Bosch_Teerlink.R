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


dataEasy = data.frame(unit = 1:101)
dataHard = data.frame(unit = 1:101)
for (i in 1:150)
{
  dataEasy[ , i] <- cumsum(rnorm(1, 0.65, 1.07/10)* 0.981^(0:100)) # note: SD from paper is in words/minute --> divide by 10 to get words/unit
  dataHard[ , i] <- cumsum(rnorm(1, 0.51, 0.69/10)* 0.965^(0:100))
} 




dataEasy$rowMean <- apply(dataEasy,1, mean)
dataEasy$rowSD <- apply(dataEasy, 1, sd)
#dataEasy$cumSum <- cumsum(dataEasy$rowMean)

dataHard$rowMean <- apply(dataHard,1, mean)
dataHard$rowSD <- apply(dataHard, 1, sd)
#dataHard$cumSum <- cumsum(dataHard$rowMean)



plot3 <- ggplot(dataEasy) +
  xlab("Time (m)") +
  ylab("Number of Words") +
  ggtitle("Clear plot") +
  scale_x_continuous(limits=c(0,100), breaks=seq(0,100,10)) +
  scale_y_continuous(limits=c(0,35), breaks=seq(0,35,5)) +
  theme_minimal() +
  theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid"))

ploteasy3 <-  plot3 + geom_point(data=dataEasy, aes(x=0:100, y=rowMean, colour="easy")) +
  geom_errorbar(data=dataEasy, aes(x=0:100, ymin=rowMean - rowSD , ymax=rowMean + rowSD)) 

plothard3 <- ploteasy3 + geom_point(data=dataHard, aes(x = 0:100, y=rowMean, colour="hard")) + 
  geom_errorbar(data=dataHard, aes(x=0:100, ymin=rowMean - rowSD , ymax=rowMean + rowSD))
plothard3


