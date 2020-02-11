### Experimentele methoden en statistiek, lab assignment 1A. 
## Assignment by:
## student 1 (Yuri Teerlink, 5987148): 
## student 2 (Vicky Bosch, 5934389):


###
###
### Questions for section 4.1.E "Working with a data.frame'
###
###


# Note how I formatted the text above: the symbol '#' precedes parts of the code
# that should not be executed (i.e., comments for yourself).
# To keep the code legible, I use white lines between assignments 
# and use ### to separate sections clearly.
# Make sure your comments are not too wide, so that they are readable without the
# need to scroll to the right.
# In R-studio you can (un-)comment one or more selected lines using
# "Code" -> Comment/Uncomment Lines. 
# You can also find the keyboard shortcut specified there. 
# Lastly, R does not have specific requirements / conventions for how you format code 
# (i.e., no requirements about indentations, etc).

# It is good practice to clear your variable list before running a script, so that
# old values of variables cannot be accidently used in a new analysis. 
# This is done by the following statement:
# Run it and check what happens to your variable list in the "Environment" panel 
# (in RStudio)
rm(list = ls())

# Execute the following code by selecting it with your mouse and 
# then running it in the console window as you learned before.
# NB: do not select ALL the code from this assignment, 
# only the subsections that you need each time.

int <- 0:9
decimal <- seq(0,0.9,0.1)         
### what does the function seq do?
# Creates a sequence of elements as a list/vector where the first argument is the starting point,
# the second is the end point and the third is the increments from start to end.

index <- rep(c("A","B"),length(int)/2)
indexAlt <- c(rep("nee",length(int)/2),rep("ja",length(int)/2)) 

myFrame <- data.frame(index1=index,index2= indexAlt,int,decimal) 
### what does data.frame do?
# data.Frame constructs a data stucture similar to a table in which the values each argument will
# be stored as a column.

myFrame             ### What is a data.frame? a data structure representing a table/matrix
myFrame[1:5,]       ### [1:5,] means: returns the first 5 rows of myFrame
myFrame[,1:2]			  ### [,1:2]  means: returns the first 2 columns
names(myFrame)      ### names means: returns the names of the columns data.frame used upon initialization
myFrame$index1
myFrame$index2			### in this setting, the $ means: returns the values stored in the column of index1/index2
dim(myFrame)				### dim means: returns number of rows and colums; the dimensions of the data.frame

###
### Questions from the section 4.1.E:
###

# myFrame[myFrame$index1 == "A",]  ### Returns all the values of myFrame for which the value stored
#                                      in index1 is equal to 'A'.
# myFrame[myFrame$index1 == "A" & myFrame$index2 == "nee",] ### Returns all the values of myFrame of which
#                                                               index1 = 'A' and index2 = 'nee'.
myFrame$oddEven <- NA
myFrame[myFrame$index1 == "A",]$oddEven <- "Even"
myFrame[myFrame$index1 != "A",]$oddEven <- "Odd"
names(myFrame)
myFrame

myFrame$rounded <- NA
myFrame[myFrame$index2 == "nee",]$rounded <- "0"
myFrame[myFrame$index2 != "nee",]$rounded <- "1"
names(myFrame)
myFrame


myFrame$modulo2 <- myFrame$int %%2
myFrame

myFrame$myRounded <- NA
myFrame[round(myFrame$decimal) == 0,]$myRounded <- "0"
myFrame[round(myFrame$decimal) == 1,]$myRounded <- "1"
names(myFrame)
myFrame


require(ggplot2)
qplot(c(4.3,5.2,6.9),c(10.1,11.2,11.9),xlab="x label", ylab="ylabel")
### (run only the above lines of code)
### you created a data frame. I also asked some questions after specific commands. 
### You can answer these questions behind the comment section to keep track of 
### your personal notes.


###
###
### Use the section below to address questions for sections 4.1.G and further 
###
###
mean(myFrame[myFrame$index1 == "A",]$int)

result <- with(myFrame[myFrame$index1 == "A",],mean(sum(int+decimal)/sum(int)*exp(decimal)))
result
#the function 'with' allows for applying an expression to the datastructure 
#given in the first parameter

for (i in unique(myFrame$index1))
{
  print(i)
  with(myFrame[myFrame$index1 == i,],print(mean(int))) 
} 
# it loops over every 'unique' item in index1 (in myFrame), namely, "A" and "B" and 
# prints the mean of each respective category.
#if "print" is removed, there is no result to be seen in the console. 
# Therefore we see that the print statement allowes us to show results in the console window

cntr <- 1
summation <- 0 
while(summation < 10) 
  {
    summation <- summation + myFrame[cntr,]$int
    cntr <- cntr + 1 
  }
print(paste("Counter is:", cntr, "Summation is:", summation))
#this code calculates a the summation of the values of the int category in myFrame, until it 
#reaches the value of 10. meanwhile, it ups a counter, that basically tracks how many 
#times the summation is updated (aka, the number of iterations)

myFrameCopy <- myFrame
myFrameCopy$c <- NA

myFrameCopy[myFrameCopy$int < 5 & myFrameCopy$index1 == "A",]$c <- "EvenSmall"
myFrameCopy[myFrameCopy$int > 5 & myFrameCopy$index1 == "A",]$c <- "EvenLarge"
myFrameCopy[myFrameCopy$index1 == "B",]$c <- "Odd"

with(myFrame,tapply(int,index1,mean))
with(myFrame,tapply(int,index1,head,n=6))
#tapply applies a function over all the values in the array of the first parameter for the different values 
#stored in the second parameter. 
#in this case it calculates the mean of the different int values corresponding to 'A' and 'B'
#head returns, depending on the value of n, the first n values of list.
# in this case(n=2) it returns the first two, changing this to 6 will return all the elements
# because there are only 5 elements stored for each of the different values.

with(myFrame,aggregate(int,list(index1=index1,index2=index2),mean) )
with(myFrame,aggregate(int,list(newName1=index1,newName2=index2),mean))
#aggregate combines all possible values of the given parameters(index1 and index2 in this case)
#, performs a given function (mean) on those values, and returns the results as a table.  

samenvatting <- with(myFrame,aggregate(int,list(newName1=index1,newName2=index2),mean))
samenvatting$gemiddelde <- samenvatting$x
samenvatting$maximum <- with(myFrame,aggregate(int,list(newName1=index1,newName2=index2),max))$x
samenvatting$minimum <- with(myFrame,aggregate(int,list(newName1=index1,newName2=index2),min))$x
samenvatting$eerste <- with(myFrame,aggregate(int,list(newName1=index1,newName2=index2),head,n=1))$
print(samenvatting)
require(plyr)
samenvatting2 <- ddply(myFrame, c("index1", "index2"), summarise, gemiddelde = mean(int, na.rm=TRUE),
                       maximum = max(int),
                       minimum = min(int),
                       eerste = head(int, n=1))
print(samenvatting2)

##### 4.1.H

mysteryFunction <- function(digit,add)
{
  result <- digit + add
  result 
  }
slicerFunction <- function(digit,criterion=5) 
  {
  result <- digit[digit<criterion]
  result }

test <- c(4,5,6,7,8,7,6,5,4,3,2,1)
slicerFunction(test)
slicerFunction(test,8)
mysteryFunction(test,5)

#>slicerFunction(mysteryFunction(slicerFunction(test),5),15)
#in this function call '5' is the parameter given to the second slicerfunction, and '15' 
#to the mysteryfunction. 

myCounter <- function(number)
{
  result <- number + (c<-which(number==number))
  result
}
myCounter(c(10:1))
setwd ("~/GitHub/EMS") 

myTable1 <- read.csv("file1.csv", header=TRUE,sep=",")
myTable2 <- read.csv("file2.csv", header=TRUE,sep=",")
#the function header makes sure that the first line is not interpreted as data, instead it sees it 
#as the names/headers of the columns. The values are seperated by commas with the 'sep' function. 

dataFrameOfTwoFiles <- rbind(myTable, myTable2)

myTable1$subject <- "1"
myTable2$subject <- "2"

newDataFrameOfTwoFiles <- rbind(myTable1, myTable2)
