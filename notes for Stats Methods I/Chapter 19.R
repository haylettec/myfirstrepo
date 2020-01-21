#############################################
# Ch19 Testing Hypotheses about proportions #
#############################################

#Step 1:  Formulate Hypotheses (PARAMETERS)

p0 <- .42

#Step 2:  Check assumptions 

#Independence
#Randomization
#Less than 10% of the population in the sample
#Success/Failure  (need n*p0 and n*(1-p0) to both be 10 or more)

n <- 81

c(n*p0,n*(1-p0)) > 25    # This is a logical command that compares each value in the vector to 10.
#need this to be TRUE TRUE to continue

#If all conditions met, phat ~ N(p, sqrt(p*(1-p)/n))

#Step 3:  Calculate the standardized test statistic & P-value

phat <- 25/81 #or a proportion if given

zstat <- (phat - p0)/sqrt(p0*(1-p0)/n)

cat("The standardized test statistic is", zstat)


#P-value = P(get what you got or more when Ho is true)

#less than
LTPval<-pnorm(zstat)
#greater than
GTPval<-1-pnorm(zstat)
#not equal
NEPval <- 2*pnorm(-abs(zstat))

cat("P-value is")
list(Less=LTPval, Greater=GTPval, NotEqual=NEPval)

#Statistical conclustion: A) Reject Ho if P-val < alpha (use .05 if none given)
#                         B) Fail to reject Ho if P-val > alpha

#English conclustion: A) There IS evidence of Ha (written to match problem)
#                     B) There is NOT strong enough evidence of Ha in the data to convince us that Ha is true.




################################################################
###############Confidence Interval for p from Ch 18:############
################################################################

x <- 376 #successes
n <- 400 #trials

phat <- x/n

phat

CIlev <- .95

alpha <- 1-CIlev
alphaovertwo <- alpha/2
oneminusalphaovertwo <- 1- alpha/2
zinCI <- qnorm(oneminusalphaovertwo)

zinCI

LowP <- phat - zinCI * sqrt(phat* (1-phat)/n)
UpP <- phat + zinCI * sqrt(phat *(1-phat)/n)

CI <- c(LowP,UpP)

cat("The asymptotic", CIlev*100, " % confidence interval for p is", CI) #co


p0 <- .05

#Step 2:  Check assumptions 

#Independence
#Randomization
#Less than 10% of the population in the sample
#Success/Failure  (need n*p0 and n*(1-p0) to both be 10 or more)

n <- 4960

c(n*p0,n*(1-p0)) > 10    # This is a logical command that compares each value in the vector to 10.
#need this to be TRUE TRUE to continue

#If all conditions met, phat ~ N(p, sqrt(p*(1-p)/n))

#Step 3:  Calculate the standardized test statistic & P-value

phat <- 4960/100000 #or a proportion if given

zstat <- (phat - p0)/sqrt(p0*(1-p0)/n)

cat("The standardized test statistic is", zstat)


#P-value = P(get what you got or more when Ho is true)

#less than
LTPval<-pnorm(zstat)
#greater than
GTPval<-1-pnorm(zstat)
#not equal
NEPval <- 2*pnorm(-abs(zstat))

cat("P-value is")
list(Less=LTPval, Greater=GTPval, NotEqual=NEPval)

0.01308585*2





