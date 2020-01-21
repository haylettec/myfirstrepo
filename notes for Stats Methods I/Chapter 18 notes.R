#Chapter18

#####################################
#Confidence Intervals For proportion#
#####################################

x <- 28 #successes
n <- 138 #trials

phat <- x/n
phat

# Note: if phat is given in the problem directly, 
# just comment out x, n and phat above and input
# it directly in the program below, such as phat <- 0.56.


CIlev <- .90

alpha <- 1 - CIlev
alphaovertwo <- alpha/2
oneminusalphaovertwo <- 1 - alpha/2
zinCI <- qnorm(oneminusalphaovertwo)
zinCI

LowP <- phat - zinCI * sqrt(phat * (1 - phat)/n)
UpP <- phat + zinCI * sqrt(phat * (1- phat)/n)

CI <- c(LowP,UpP)
cat("The asymptotic", CIlev*100, " % confidence interval for p is", CI) #concatenate

#to find the margin of error and n
ME <- zinCI * sqrt(phat * (1 - phat)/n)
nME <- (zinCI/ME)^2 * (phat * (1 - phat))

(1.645/.03)^2 *(.21 * (1-.21))

cat("The margin of error is", ME, "n would have to be", nME)

prop.test(x,n,conf.level=CIlev,correct=TRUE) #preferred CI with correction...use professionally



###############################
#Example Problem from the Book#
###############################
#A Gallup Poll in 2011 found 76% of Americans believe high achieving 
#HS Students should be recruited to become teachers (random sample of 1002 Americans).
#Create a 90% CI for p = proportion of Americans who all agree.

#x <- 144 #successes *diregard x for this problem
n <- 1002 #trials

phat <- 0.76
phat

# Note: if phat is given in the problem directly, 
# just comment out x, n and phat above and input
# it directly in the program below, such as phat <- 0.56.


CIlev <- .90

alpha <- 1 - CIlev
alphaovertwo <- alpha/2
oneminusalphaovertwo <- 1 - alpha/2
zinCI <- qnorm(oneminusalphaovertwo)
zinCI

LowP <- phat - zinCI * sqrt(phat * (1 - phat)/n)
UpP <- phat + zinCI * sqrt(phat * (1 - phat)/n)

CI <- c(LowP,UpP)

cat("The asymptotic", CIlev*100, " % confidence interval for p is", CI) #concatenate




### SAMPLE SIZE CALCULATIONS:

target.margin.error <- 0.03
CIlev <- 0.95
# Use 0.50 if you have no other information.  Use an estimate of p-hat from the problem if it is given.
p.hat.est <- 0.50

alpha <- 1 - CIlev
alphaovertwo <- alpha/2
oneminusalphaovertwo <- 1 - alpha/2
zinCI <- qnorm(oneminusalphaovertwo)

n <- ((zinCI/target.margin.error)^2) * p.hat.est * (1 - p.hat.est)
n



