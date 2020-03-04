library(ggplot2) 
library(cowplot) 



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

dataEasy = data.frame(unit = PayneTable $unit)
dataHard = data.frame(unit = PayneTable $unit)
for (i in 1:150)
{
  dataEasy[ , i] <- rnorm(1, 0.65, 1.07/10)* 0.981^(dataEasy$unit-1) 
  dataHard[ , i] <- rnorm(1, 0.52, 0.69/10)* 0.965^(dataHard$unit-1)
} 


dataEasy$EasyWords <- cumsum(dataEasy) 
dataHard$DifficultWords <- cumsum(dataHard)


plot3 <- ggplot(PayneTable) +
  xlab("Time (m)") +
  ylab("Number of Words") +
  ggtitle("Clear plot") +
  scale_x_continuous(limits=c(0,100), breaks=seq(0,100,10)) +
  scale_y_continuous(limits=c(0,35), breaks=seq(0,35,5)) +
  theme_minimal() +
  theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid"))

ploteasy3 <-  plot3 + geom_point(data=dataEasy, aes(x = unit,y=EasyWords, colour="easy")) #+
                    #geom_errorbar(aes(ymin=mean150 - sd150 , ymax=mean150 + sd150), width=.2) 

#plothard3 <- ploteasy + geom_point(data=dataHard, aes(x = unit,y=mean150, colour="hard")) + 
#                      geom_errorbar(aes(ymin=mean150 - sd150 , ymax=mean150 + sd150), width=.2)

print (plothard3)
