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

index <- rep(c("A","B"),length(int)/2)
indexAlt <- c(rep("nee",length(int)/2),rep("ja",length(int)/2)) 

myFrame <- data.frame(index1= index,index2= indexAlt,int,decimal) 
### what does data.frame do?

myFrame             ### What is a data.frame? 
myFrame[1:5,]       ### [1:5,] means:
myFrame[,1:2]			  ### [,1:2]  means:
names(myFrame)      ### names means:
myFrame$index1
myFrame$index2			### in this setting, the $ means:
dim(myFrame)				### dim means:

### (run only the above lines of code)
### you created a data frame. I also asked some questions after specific commands. 
### You can answer these questions behind the comment section to keep track of 
### your personal notes.


###
###
### Use the section below to address questions for sections 4.1.G and further 
###
###


