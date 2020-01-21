###############################################
# Ch 24: Chi square tests on Categorical Data #
###############################################

# Contents
# Intro to Chi-square distribution
# Test 1: Goodness of fit
# Test 2: Homogeneity
# Test 3: Independence


# Intro: How does Chi-square distribution looks like.

df <- 8
x <- seq(0,3*df,.1) #sequence from, to, by
fx <- dchisq(x,df) #density of a chisquare
plot(x, fx, type = "l") #type = lines

# Critical Values of a Chi Square distribution test
alpha <- .05
df <- 9
critval <- qchisq(1 - alpha, df)
critval


##############################################
# Test 1
# Goodness of fit = table matches what you expect...one variable Ho:distribution matches stated
##############################################

face <- c("one", "two", "three", "four", "five", "six")
obs <- c(65, 78, 53, 71, 57, 54, 76, 69, 80, 63)

df <- length(obs) - 1

tot <- sum(obs)

exppercent <- c(1/10, 1/10, 1/10, 1/10, 1/10, 1/10, 1/10, 1/10, 1/10, 1/10)
exp <- tot*exppercent

chisqstat <- sum((obs - exp)^2/exp)
chisqsta

pval <- 1 - pchisq(chisqstat,df)
pval

standardresid <- (obs - exp)/sqrt(exp)
standardresid

###################### End of test 1 #####################

# There is function that can do all...

?chisq.test #help menu for the chisq.test function

chisq.test(obs, p = exppercent) #Need a vector of obs, and expected percents

chisq.test(obs, p = exppercent)$expected
chisq.test(obs, p = exppercent)$residuals #the Pearson residuals, (observed - expected) / sqrt(expected).

# Code for when the observations are given as percents

exppercent <- c(.15, .3, .4, .1, .05)
sum(exppercent)
obspercent <- c(.2738, .3723, .2400, .0769, .0369) #NEED "other" at the end
sum(obspercent)

n <- 325
obs <- n*obspercent
chisq.test(obs,p = exppercent)

##########################################
# Test 2
# Homogeneity = one variable, comparing groups to one another  Ho:  distribution same across groups
##########################################

moon.bx <- matrix(c(2, 3, 17, 20, 26, 21, 10, 13, 8 ,6), ncol = 2, byrow = TRUE)
colnames(moon.bx) <- c("full moon", "not full")
rownames(moon.bx) <- c("violent", "property", "drugs/alcohol", "dom ab", "other")

testtable <- as.table(moon.bx)
testtable

chisq.test(testtable)









###################### End of test 2 #####################

# Some optional stuffs related to test 2
## apply & margin.table work similarly to find totals of rows/columns

apply(testtable, 1, sum) #1 is rows
margin.table(testtable, 1)

apply(testtable, 2, sum) #2 is columns
margin.table(testtable, 2)


rowtot <- margin.table(testtable, 1)
coltot <- margin.table(testtable, 2)

rowtot
coltot

nrows <- length(rowtot)
ncols <- length(coltot)

df <- (nrows - 1)*(ncols - 1)
df

tot <- margin.table(testtable)

tot

exp <- rowtot %*% t(coltot)/tot #%*% is matrix multiplication, t is the transpose
exp

chisqcontrib <- (as.array(testtable) - exp)^2/exp
chistat <- sum(chisqcontrib)
chistat

ChiShow <- chisq.test(testtable)

sum((ChiShow$observed - ChiShow$expected)^2/ChiShow$expected) #Chi Sq test of homogeneity stat

stat <- sum((ChiShow$observed - ChiShow$expected)^2/ChiShow$expected)

pval <- 1 - pchisq(stat, df = 2)
pval

# END of Some optional stuffs related to test 2

############################################
# Test 3
# Independence = two variables Ho: no association between row and column
############################################


alivemem <- matrix(c(42, 82, 133, 214, 669, 176, 240, 580), ncol = 4, byrow = TRUE)
colnames(alivemem) <- c("crew","1st", "2nd", "3rd")
rownames(alivemem) <- c("alive", "dead")

obstable <- as.table(alivemem)
obstable

chisq.test(obstable)

ChiSq <- chisq.test(as.table(obstable), correct = FALSE)
ChiSq

names(ChiSq)

ChiSq$observed
ChiSq$expected
ChiSq$residuals
ChiSq$stdres
ChiSq$p.value
ChiSq$stat

sum(ChiSq$residuals^2) #same as the stat