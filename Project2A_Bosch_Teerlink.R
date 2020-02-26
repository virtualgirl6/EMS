library(ggplot2) 
library(cowplot) 



time <- seq(1,600,6)  ## time steps of 6 seconds
PayneTable <- data.frame(time = time)
PayneTable $unit <- PayneTable $time / 6
PayneTable $wordsPerUnitEasy <- 0.65* 0.981^(PayneTable $unit-1) 
PayneTable $wordsPerUnitDifficult <- 0.51* 0.965^(PayneTable $unit-1)
PayneTable $EasyWords <- cumsum(PayneTable $wordsPerUnitEasy) 
PayneTable $DifficultWords <- cumsum(PayneTable $wordsPerUnitDifficult)

plot1 <- ggplot(PayneTable, aes(x=LocalTime, y=nrCorrectScrabbleWords)) +
  #geom_point(shape=21,colour="black", fill="white", size = 2, stroke = 1) + #geom_line() +
  xlab("Time (s)") +
  ylab("Number of Words") +
  ggtitle("Clear plot") +
  scale_x_continuous(limits=c(0,600), breaks=seq(0,600, 25), minor_breaks = seq(0,600, 5)) +
  scale_y_continuous(limits=c(0,35), breaks=seq(0,35,5)) +
  theme_minimal() +
  theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid"))

ploteasy <-  plot1 + geom_point(data=PayneTable, aes(x = time,y=EasyWords, colour="easy")) 
plothard <- ploteasy + geom_point(data=PayneTable, aes(x = time,y=DifficultWords, colour="hard"))

print (plothard)