# Use this Rscript if you have summary stats (e.g. not raw data) in your problem
# This also contains 2 examples from HW that use DATA...

##################################
# CI for regression coefficients #
##################################
# Here you can test for either b1 or for b0.

bj <- 0.492
SEbj <- .057

CIdf <- 117
CIlev <- .95

alpha <- 1 - CIlev
tCI <- qt(1 - alpha/2,CIdf)

Low <- bj - tCI*SEbj
Up <- bj + tCI*SEbj

cat("The ",CIlev*100,"% CI is (",Low,", ",Up,").")

##################################
# HT for regression coefficients #
##################################

#If < or > in hypothesis, p-val = given p-val/2

TablePval <- .1446
RealPval <- TablePval/2
RealPval

#Slope
b1 <- .702507
SEb1 <- .5784
tstat <- b1/SEb1
tstat

n <- 287

dfHT <- n - 2

#less
LT <- pt(tstat,dfHT)

#more
GT <- 1-pt(tstat,dfHT)

#tails
TwoTail <- 2*min(LT,GT)

list(LessThan = LT, GreaterThan = GT, TwoTailArea = TwoTail)

########################################
########### End of HT ##################
########################################


#######################################
########## For new observations #######
#######################################
b0 <- -49.5277
b1 <- 1.8715

SEb1 <- .3052

xbar <- 38.3
xnew <- 38

se <- 4.53 # s in the output
n <- 20

df <- n-2

yhatnew <- b0 + b1*xnew

CIlev <-.95
alpha <-1-CIlev

tCI <- qt(1-alpha/2,df)

#------------------------------------------------
# AVERAGE (CI)? then use next 4 lines of commands
#------------------------------------------------

SEmuNEW <- sqrt(SEb1^2*(xnew-xbar)^2 + se^2/n)

LowCI <- yhatnew - tCI*SEmuNEW
UpCI <- yhatnew + tCI*SEmuNEW

cat("The ",CIlev*100,"% confidence interval for average at x=",xnew," is ","(",LowCI,", ",UpCI,").")

#------------------------------------------------
# Single new observation (Prediction Interval)? then use next 4 lines of commands
#------------------------------------------------

SEyhatNEW <- sqrt(SEb1^2*(xnew-xbar)^2 + se^2/n +se^2)

LowPI <- yhatnew - tCI*SEyhatNEW
UpPI <- yhatnew + tCI*SEyhatNEW

cat("The ",CIlev*100,"% prediction interval for x=",xnew," is ","(",LowPI,", ",UpPI,").")

############End new observations################


# The followings can be helpful for some HW problems

#t-critical values
df<-22

#for not equal
alpha <- .05

tcritneg <- qt(alpha/2,df)
tcritneg

tcritpos <- qt(1-alpha/2,df)
tcritpos

#for less than
tcritNEG <- qt(alpha,df)
tcritNEG

#for greater than
tcritPOS <-qt(1-alpha,df)
tcritPOS