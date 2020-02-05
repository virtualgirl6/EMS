### Experimentele methoden en statistiek, lab assignment 1A. 
## Assignment by:
## student 1 (name, studentID): 
## student 2 (name, studentID):


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

myFrame$myRounded <- NA
myFrame[round(myFrame$decimal) == 0,]$myRounded <- "0"
myFrame[round(myFrame$decimal) == 1,]$myRounded <- "1"
names(myFrame)
myFrame

### (run only the above lines of code)
### you created a data frame. I also asked some questions after specific commands. 
### You can answer these questions behind the comment section to keep track of 
### your personal notes.


###
###
### Use the section below to address questions for sections 4.1.G and further 
###
###


result <- with(myFrame[myFrame$index1 == "A",],mean(sum(int+decimal)/sum(int)*exp(decimal)))
#the function 'with' allows for applying an expression to the datastructure 
#given in the first parameter

for (i in unique(myFrame$index1))
{
  print(i)
  with(myFrame[myFrame$index1 == i,],print(mean(int))) 
} 
# it loops over every 'unique' item in index1 (in myFrame), namely, "A" and "B" and 
# prints the mean of the int category. XXX 
#if "print" is removed, there is no result to be seen in the console.  

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
#times the summation is updated.












