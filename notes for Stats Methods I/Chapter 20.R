###################################
#Ch 20: HT & CI for single mean mu#
###################################

#This script contains:
#1) HT for mu
#2) CI for mu
#3) Sample size for mu


########################
# HT for single mean mu#
########################

# Here is a way enter data in R manually
myData <- c(29.2, 28.5, 28.7, 28.9, 29.1, 29.5)

setwd(doritoswt)
allData <- doritoswt
myData <- allData$weights
myData


# Or you can read the data in from a file by setting your working directory
#setwd("C:/Users/goskyrm/Google Drive/Documents/App State/STT3820/Data Files")
#allData <- read.csv("Class_Experiment_MatchedPairs.csv")
#myData <- allData$Diff_Time

# This will calculate the nessesary descriptive statistics for the variable you are investiging
# You can also manually enter the descriptives here if you already have them

n <- length(myData)

df <- n - 1

ybar <- mean(myData)
s <- sd(myData)

# Before running the hypothesis test it is important to check your assumptions
#You can visually assess nearly normal using this scrpt to create a histogram with a normal overlay
h<-hist(myData, breaks=10, col="red", xlab="Difference in Time(s)", 
        main="Histogram with Normal Curve") 
xfit<-seq(min(myData),max(myData),length=40) 
yfit<-dnorm(xfit,mean=ybar,sd=s) 
yfit <- yfit*diff(h$mids[1:2])*n 
lines(xfit, yfit, col="blue", lwd=2)

#Now do the hypothesis test

#Step 1
mu0 <- 28.3 #This is the value in H0 : mu = mu0

#step 2: Find the test statistic
tstat <- (ybar - mu0)/(s/sqrt(n))

cat("The t-stat is",tstat)


#Step 3: Find the p-value

#less
LT<- pt(tstat,df)

#more
GT<- 1-pt(tstat,df)

#tails
Tail <- 2*min(LT,GT)

cat("P-value will be")
list(LessThan=LT,GreaterThan=GT,TwoTail=Tail)


########################
# CI for single mean mu#
########################

myData<-c(9.9, 9.9, 10, 9.9, 10, 9.7, 9.6, 9.5, 9.6, 9.9, 10, 9.8, 9.7, 10.2, 9.5, 10.1, 9.8, 10.1, 9.8, 9.9)

n <- 6

df <- n-1

ybar <- 8222
s <- 1742

CIlev <- .90

alpha <- 1-CIlev
alphaovertwo <- alpha/2
oneminusalphaovertwo <- 1- alpha/2
tinCI <- qt(oneminusalphaovertwo,df)

tinCI #t in a CI formula

LowMu <- ybar - tinCI * s/sqrt(n)
UpMu <- ybar + tinCI * s/sqrt(n)

CI <- c(LowMu,UpMu)

cat("The ", CIlev*100, " % confidence interval for mu is", CI) #concatenate

###################
# HT as a function#
###################

HTmean <- function(data,mu0){
  n<-length(data)
  df <- n-1
  
  ybar <-mean(data)
  s <- sd(data)
  
  tstat <- (ybar - mu0)/(s/sqrt(n))
  
  #less
  LT<- pt(tstat,df)
  
  #more
  GT<- 1-pt(tstat,df)
  
  #tails
  Tail <- 2*min(LT,GT)
  
  cat("The t-stat is",tstat,"\n P-value will be \n")
  
  list(LessThan=LT,GreaterThan=GT,TailArea=Tail)
}

###################
# CI as a function#
################### 

CImean <- function(data,CIlev){
  n<-length(data)
  df <- n-1
  
  ybar <-mean(data)
  s <- sd(data)
  
  alpha <- 1-CIlev
  alphaovertwo <- alpha/2
  oneminusalphaovertwo <- 1- alpha/2
  tinCI <- qt(oneminusalphaovertwo,df)
  
  LowMu <- ybar - tinCI * s/sqrt(n)
  UpMu <- ybar + tinCI * s/sqrt(n)
  
  CI <- c(LowMu,UpMu)
  
  cat("The ", CIlev*100, " % confidence interval for mu is", CI) #concatenate
}


# Here is an examaple to use the functions defined above
# I am using the same data 

HTmean(myData,mu0 = 10)
CImean(myData,CIlev = .95)


# Here is another example with new data
Battery <- c(321,295,332,351,281,336,311,253,270,326,311,288) #data

HTmean(Battery,300)
CImean(Battery,CIlev = .95)

###########################################
#Sample size estimate mu within a given ME#
###########################################

CIlev <- .95

s <- 6
ME <- 2

alpha <- 1-CIlev
alphaovertwo <- alpha/2
oneminusalphaovertwo <- 1- alpha/2
zinCI <- qnorm(oneminusalphaovertwo)

nprelim <- (zinCI * s/ME)^2
df <- nprelim-1

tinCI <- qt(oneminusalphaovertwo,df)

nfinal <- (tinCI * s/ME)^2
nfinal


#Sample size as a function

sampsizemean <- function(CIlev,s,ME){
  alpha <- 1-CIlev
  alphaovertwo <- alpha/2
  oneminusalphaovertwo <- 1- alpha/2
  zinCI <- qnorm(oneminusalphaovertwo)
  
  nprelim <- (zinCI * s/ME)^2
  df<-nprelim-1
  
  tinCI <-qt(oneminusalphaovertwo,df)
  
  nfinal <- (tinCI * s/ME)^2
  cat("The required sample size is",ceiling(nfinal))
}

# Here is how you would use this function

sampsizemean(CIlev = 0.95, s = 29.31, ME = 0.15)




#example from class 10/16 ~~~~ download time for a movie

qt(.975, 5)

qt()
