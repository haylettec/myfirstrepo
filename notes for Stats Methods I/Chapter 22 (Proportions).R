# TWO PROPORTIONS


# SECTION 1:  DATA ARE SUMMARIZED AS PROPORTION OF SUCCESSES PER SAMPLE.

# phat = proportion of successes.   n = sample size.  

# EXAMPLE:  A study measured whether coffee consumption before a memorization quiz allowed participants to pass the memorization quiz at higher rates.
# 68 subjects were assigned to the coffee group and 73% of them passed the quiz, while in the control group, only 62% of the 75 members of that group passed the quiz.



phat <- c(42/152, 8/80)
n <- c(152,80)


# CONFIDENCE INTERVAL FOR p1 - p2.

conf.level <- 0.99

# Check conditions.
n*phat > 10    # At least 10 successes in each sample.  Should read TRUE TRUE if conditions are met.
n-n*phat > 10  # At least 10 failures in each sample.  Should read TRUE TRUE if conditions are met.

pdiff <- phat[1] - phat[2]
pdiff


se.pdiff <- sqrt(sum(phat*(1-phat)/n))
se.pdiff

zstar <- qnorm((1 + conf.level)/2)
zstar

lcl <- pdiff - zstar*se.pdiff
ucl <- pdiff + zstar*se.pdiff

CI <- c(lcl,ucl)
CI

cat("The", conf.level*100, "% confidence interval for p1 - p2 mu is", CI) #concatenate



# HYPOTHESIS TEST OF Ho: p1 = p2.

pooled.proportion <- sum(n*phat)/sum(n)
pooled.proportion

se.pdiff.pooled <- sqrt(pooled.proportion*(1-pooled.proportion)*(1/n[1] + 1/n[2]))
se.pdiff.pooled

zobs <- pdiff/se.pdiff.pooled

cat("The z-stat is", zobs)

# p-value for Ha: p1 < p2.
LT <- pnorm(zobs)

# p-value for Ha: p1 > p2.
GT <- 1-pnorm(zobs)

#tails
Tail <- 2*min(LT,GT)

cat("P-value will be")
list(p1Less=LT,p1Greater = GT, NotEqual=Tail)





# SECTION 2:  DATA ARE SUMMARIZED AS NUMBER OF SUCCESSES PER SAMPLE.

# X = number of successes.   n = sample size.  

# EXAMPLE:  A study looked at whether xylitol (a sweetner) provided some prevention for ear infections in preschool children.
#  In a randomized experiment, 165 children took five daily doses of a placebo, and 68 of them got an ear infection during the study.
# For the Xylitol group, 159 children took five daily doses of xylitol, and 46 of them got an ear infection during the study.
# Let's see how we can analyze this data.


x <- c(68,46)
n <- c(165,159)


# CONFIDENCE INTERVAL FOR p1 - p2.

conf.leve. <- 0.95

# Check conditions.
x > 10    # At least 10 successes in each sample.  Should read TRUE TRUE if conditions are met.
n-x > 10  # At least 10 failures in each sample.  Should read TRUE TRUE if conditions are met.

phat <- x/n
phat

pdiff <- phat[1] - phat[2]
pdiff


se.pdiff <- sqrt(sum(phat*(1-phat)/n))
se.pdiff

zstar <- qnorm((1 + conf.level)/2)
zstar

lcl <- pdiff - zstar*se.pdiff
ucl <- pdiff + zstar*se.pdiff

CI <- c(lcl,ucl)
CI

cat("The", conf.level*100, "% confidence interval for p1 - p2 mu is", CI) #concatenate



# HYPOTHESIS TEST OF Ho: p1 = p2.

pooled.proportion <- sum(x)/sum(n)
pooled.proportion

se.pdiff.pooled <- sqrt(pooled.proportion*(1-pooled.proportion)*(1/n[1] + 1/n[2]))
se.pdiff.pooled

zobs <- pdiff/se.pdiff.pooled

cat("The z-stat is", zobs)

# p-value for Ha: p1 < p2.
LT <- pnorm(zobs)

# p-value for Ha: p1 > p2.
GT <- 1-pnorm(zobs)

#tails
Tail <- 2*min(LT,GT)

cat("P-value will be")
list(p1Less=LT,p1Greater = GT, NotEqual=Tail)

