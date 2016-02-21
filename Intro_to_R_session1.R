library('ISLR')
library('MASS')

### vectors
x <- c(2,7,5)
x = c(2,7,5)

## Printing values
x
print(x)

## Generating Sequences
y<-seq(from=4,length=3,by=3)
y
y <- seq(1, 10, 2)
y
length(y)
## Getting help on internal functions
?seq
help(seq)

## Operation on Vectors: Note Vectors must have the same length
## Operations are performed component-wise
x = seq(1,10,2)
y = c(1,3,5,3,2)
x+y
x/y
x^y

## Accessing vector elements
x[2]
x[2:4]
x[-2] ## - means everything but
x[-c(1,2)]

## Creating a Matrix
?matrix
z=matrix(0,nrow = 4,ncol = 3)
z=matrix(seq(1,12),nrow = 4,ncol = 3) ## Elements are ordered in a column-wise fashion
print(z)
z=matrix(seq(1,12),nrow = 4,ncol = 3, byrow = T) ## To order by row, use byrow argument
print(z)

## Accessing matrix elements
z[3:4,2:3]
z[,2:3]
z[,1] ## Note: Will be reduced to a vector
z[,1,drop=FALSE] ## Use drop argument to keep the dimenssion
dim(z)

## Matrix multiplication
A = matrix(seq(1,12),nrow = 4,ncol = 3)
B = matrix(seq(2,7),nrow = 3,ncol = 2)

C = A %*% B ## Matrix multiplication
dim(C)

C = A * A ## Component-wise multiplication (dimentions must match)

## Listing variables
ls()

## Removing Variables
rm(y)
ls()
rm(list = ls())
ls()

## Reading data from files:
?read.table
getwd()
Auto = read.table('../../data/Auto.txt', header=T,sep='\t',quote="")

## Getting some information about data file
names(Auto)
attributes(Auto)
dim(Auto)

summary(Auto)

## Accessing individual columns
Auto$mpg
cbind(Auto[,1], Auto$cylinders) ## binding columns

## Basic Data types
a = 3
typeof(a)
class(a)
s = 'MATH480'
typeof(s)
class(s)
a = numeric(10)
class(a)
typeof(a)

## Factors or categorical variables
a = seq(0,10,by=1)
a = factor(a)
class(a)
typeof(a)


class(Auto) ## Data frames
class(Auto$name) ## Factors or Categorical Variables

Auto = read.table('../../data/Auto.txt', header=T,sep='\t',quote="",stringsAsFactors=F)
class(Auto)
class(Auto$name)

a = TRUE ## or T
typeof(a)

b = F ## or FALSE
typeof(b)

## Tables
a <- factor(c("A","A","B","A","B","B","C","A","C"))
results <- table(a)
results
a
attributes(results)

a <- c("Sometimes","Sometimes","Never","Always","Always","Sometimes","Sometimes","Never")
b <- c("Maybe","Maybe","Yes","Maybe","Maybe","No","Yes","No")
results <- table(a,b)
results

## Logical Operators

# <  less than
# >	great than
# <=	less than or equal
# >=	greater than or equal
# ==	equal to
# !=	not equal to
# |	entry wise or
# ||	or
# !	not
# &	entry wise and
# &&	and
# xor(a,b)	exclusive or

a = c(TRUE,FALSE)
b = c(TRUE,TRUE)

a|b
a||b
xor(a,b)
a & b

## A few Builtin Functions (Most work component-wise)
a = 1:10
summary(a)

summary(Auto)

mean(a)
median(a)
var(a)
sd(a)
max(a)
min(b)

### Generating random data
x=runif(50)
y=rnorm(500)
rnorm(50, mean = 1, sd = 0.1)

## basic plots
plot(x,y)
plot(x,y,xlab="Random Uniform",ylab="Random Normal",pch="*",col="blue")
par(mfrow=c(2,1))
plot(x,y)
hist(y)
par(mfrow=c(1,1))
### Reading in data
plot(Auto$cylinders,Auto$mpg)
plot(Auto$cyl,Auto$mpg)
attach(Auto)
search()
plot(cylinders,mpg)
cylinders=as.factor(cylinders)
plot(cylinders,mpg,xlab="Cylinders",ylab="Mpg",col="red")

pdf(file="../../output/mpg.pdf")
plot(cylinders,mpg,xlab="Cylinders",ylab="Mpg",col="red")
dev.off()

detach(Auto)

pairs(Auto,col="brown")
pairs(mpg~cylinders+weight,Auto, col = "brown")


## Exploring some of the data sets in ISLR package
## 1) Wage
names(Wage)
summary(Wage)

plot(Wage$age, Wage$wage, pch='.', xlab="age",ylab="Wage",col="blue")
plot(Wage$year, Wage$wage, pch='.', xlab="year",ylab="Wage",col="blue")
plot(Wage$education, Wage$wage, xlab="education",ylab="Wage",col="blue")

## 2) Stock Market
names(Smarket)
summary(Smarket)
plot(Smarket$Direction, Smarket$Lag1, xlab="Direction",ylab="Percentage Change",col=c("blue", "red"))
plot(Smarket$Direction, Smarket$Lag2, xlab="Direction",ylab="Percentage Change",col=c("blue", "red"))
plot(Smarket$Direction, Smarket$Lag3, xlab="Direction",ylab="Percentage Change",col=c("blue", "red"))

## Quitting R
q()
