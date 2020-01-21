#########################################
#Ch 22: Comparing two independent means #
#########################################

######
# HT #
######

ybar1 <- 57.1
s1 <- 32.5
n1 <- 318

ybar2 <- 54.2
s2 <- 28.1
n2 <- 264

df <- (s1^2/n1 + s2^2/n2)^2/(1/(n1 - 1)*(s1^2/n1)^2 + 1/(n2 - 1)*(s2^2/n2)^2)

df

tstat <- (ybar1 - ybar2)/sqrt(s1^2/n1 + s2^2/n2)

cat("The t-stat is",tstat)

#less
LT <- pt(tstat,df)

#more
GT <- 1 - pt(tstat,df)

#tails
Tail <- 2*min(LT,GT)

cat("P-value will be")
list(LessThan = LT, GreaterThan = GT, TailArea = Tail)


######
# CI #
######

ybar1 <- 14.7
s1 <- 8.4
n1 <- 27

ybar2 <- 8.5
s2 <- 6.1
n2 <- 27

df <- (s1^2/n1 + s2^2/n2)^2/(1/(n1 - 1)*(s1^2/n1)^2 + 1/(n2 - 1)*(s2^2/n2)^2)
df

CIlev <- .95

alpha <- 1 - CIlev
alphaovertwo <- alpha/2
oneminusalphaovertwo <- 1 - alpha/2
tinCI <- qt(oneminusalphaovertwo,df)

tinCI  #t in a CI formula

LowP <- ybar1 - ybar2 - tinCI * sqrt(s1^2/n1 + s2^2/n2)
UpP <- ybar1 - ybar2 + tinCI * sqrt(s1^2/n1 + s2^2/n2)

CI <- c(LowP, UpP)

cat("The ", CIlev*100, " % confidence interval for mu is", CI) #concatenate

