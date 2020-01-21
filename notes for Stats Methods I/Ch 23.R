# Ch 23: Paired Data

# We examine how paired data can be examined.  Paired data refers to cases where the two different
# Samples are not indepdendent of each other.

# Example from book.  An insurance agent compared prices for car insurance for 10 clients using
# diff2 carriers versus a diff1 carrier.  Thequestion of interest is whether diff2 carriers
# offer cheaper coverage on average, and if so, how much.

diff1 <- c(820, 557, 1408, 665, 685, 1258, 640, 1205, 688, 932)
diff2 <- c(876, 544, 1052, 658, 940, 1076, 610, 923, 881, 879)
diff <- diff1 - diff2

hist(diff)

# or

# Before running the hypothesis test it is important to check your assumptions
#You can visually assess nearly normal using this scrpt to create a histogram with a normal overlay
h<-hist(diff, breaks=10, col="red", xlab="Difference in Insurance Prices", 
        main="Histogram with Normal Curve") 
xfit<-seq(min(diff),max(diff),length=40) 
yfit<-dnorm(xfit,mean=mean(diff),sd=sd(diff)) 
yfit <- yfit*diff(h$mids[1:2])*n 
lines(xfit, yfit, col="blue", lwd=2)

# CI for mu_difference.

conf.level <- 0.95
n <- length(diff)
df <- n - 1
tstar <- qt((1+conf.level)/2, df)

LCL <- mean(diff) - tstar*sd(diff)/sqrt(n)
UCL <- mean(diff) + tstar*sd(diff)/sqrt(n)

CI <- c(LCL, UCL)

cat("The ", conf.level*100, " % confidence interval for mu_difference is", CI) #concatenate


# Hypothesis Test:

tobs <- mean(diff)/(sd(diff)/sqrt(n))

# mu_difference less than 0
LT<- pt(tobs,df)

#mu_difference greater than 0
GT<- 1-pt(tobs,df)

# 2 tails
Tail <- 2*min(LT,GT)

cat("The t-stat is",tobs,"\n P-value will be \n")

list(LessThan=LT,GreaterThan=GT,TailArea=Tail)


####### CI for with mean, sd, and n given.#######
mu <- 7.27
sigma <- 2.52
n <- 646


conf.level <- 0.95
n
df <- n - 1
tstar <- qt((1+conf.level)/2, df)

LCL <- mu - tstar*sigma/sqrt(n)
UCL <- mu + tstar*sigma/sqrt(n)

CI <- c(LCL, UCL)

cat("The ", conf.level*100, " % confidence interval for mu_difference is", CI) #concatenate


# Hypothesis Test:

tobs <- mu/(sigma/sqrt(n))

# mu_difference less than 0
LT<- pt(tobs,df)

#mu_difference greater than 0
GT<- 1-pt(tobs,df)

# 2 tails
Tail <- 2*min(LT,GT)

cat("The t-stat is",tobs,"\n P-value will be \n")

list(LessThan=LT,GreaterThan=GT,TailArea=Tail)

