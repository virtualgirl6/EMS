library(ggplot2) 
library(cowplot) 

time <- seq(1,600,6)  ## time steps of 6 seconds
PayneTable <- data.frame(time = time)
PayneTable $unit <- PayneTable $time / 6
PayneTable $wordsPerUnitEasy <- 0.65* 0.981^(PayneTable $unit-1) 
PayneTable $wordsPerUnitDifficult <- 0.51* 0.965^(PayneTable $unit-1)
PayneTable $EasyWords <- cumsum(PayneTable $wordsPerUnitEasy) 
PayneTable $DifficultWords <- cumsum(PayneTable $wordsPerUnitDifficult)

ploteasy <-  geom_point(data=PayneTable, aes(x = time,y=EasyWords, colour="easy")) 
plothard <- ploteasy + geom_point(data=PayneTable, aes(x = time,y=DifficultWords, colour="hard"))
print (plothard)