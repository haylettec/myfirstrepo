library(readr)

#########################################
# Ch 25: Inference for Linear Regresion #
#########################################

# Read in Data
age <- read_csv("~/git_projects/notes for Stats Methods I/data-11_15_2019-5_07 PM.csv")
View(age)

# Define what your x and y are
x <- age$Age
y <- age$`Prices Advertised`

# Create a scatterplot of x vs. y
plot(x, y, xlab="Yesterday's High Temperature", ylab="Today's High Temperature", main = "Scatterplot of Hi Temps on Successive Boone 2018 Summer days", col = "red")

# Find the correlation between x and y
cor(x,y)

# Find the R^2 value
Rsquared <- cor(x,y)^2
Rsquared

# Everything looks good? Then fit the linear model using lm.
model <- lm(y~x) #linear model

# We can add the line to the plot as a visual aid NOTE: you need to run plot before running this for both to appear.
abline(lm(y~x))

summary(model)

# Now let's look at the model we created
names(model)

model$coefficients  #[1] is the intercept, [2] is the slope
model$fitted.values
model$residuals

#residual standard deviation
resSD<-sqrt(sum((model$residuals)^2)/(length(model$residuals)-2))
resSD
#--------------------------------------------------
# Optional: Creating an easily read table of obsreved values, fits and residuals:

numbers <- cbind(weather[,3], model$fitted.values, model$residuals)
colnames(numbers) <- c("Observed", "Fits", "Residuals")
rownames(numbers) <- weather[,1]

output <- as.table(numbers)
output
#--------------End Table---------------------------

# It is time to examine the residual

plot(x, model$residuals) # residuals vs x - Residual plot

# standardized residuals
library(MASS)
stdresvals <- stdres(model)
plot(x, stdresvals) # This residual plot is easier to read than the previous

cor(x, model$residuals) # should be closer to 0

hist(model$residuals) # histogram of residuals - To check if residuals are normal

qqnorm(model$residuals) # QQplot- To check if residuals are normal


################
# HT for betas #
################

summary(model)

################
# CI for betas #
################

confint(lm(y~x), level=.95) #95% CI for betas

#########################
# CI and PI for x value #
#########################

predict(lm(y~x), data.frame(x=70),interval="conf",level=.95) #confidence interval for x=70
predict(lm(y~x), data.frame(x=70),interval="pred",level=.95) #prediction interval for x=70

#####################
# Ploting CI and PI #
#####################

#Plot with regression model and confidence and prediction intervals
plot(x, y, xlab="Calories", ylab="Carbohydrates", main = "Scatterplot of Calories vs. Carbohydrates", col = "red")
abline(lm(y~x))

model <- lm(y~x)

newx<-seq(min(x), max(x), by=0.05)
conf_interval <- predict(model, newdata=data.frame(x=newx), interval="confidence",
                         level = 0.95)
lines(newx, conf_interval[,2], col="blue", lty=2)
lines(newx, conf_interval[,3], col="blue", lty=2)

pred_interval <- predict(model, newdata=data.frame(x=newx), interval="prediction",
                         level = 0.95)
lines(newx, pred_interval[,2], col="orange", lty=2)
lines(newx, pred_interval[,3], col="orange", lty=2)








# OPTIONAL: Logistic Regression
shuttle<-read.csv("C:\\Susie\\STT3820\\Data\\Shuttle.csv")
shuttle
glm(shuttle$Damage ~ shuttle$Temp, family=binomial(logit)) #logistic regression

glm(shuttle$Damage ~ shuttle$Temp, family=binomial(logit))$coefficients

sort(shuttle$Temp)
x <-seq(50,85,.1)
phat <- 1/(1+exp(-(15.0429016-.2321627*x)))
plot(x,phat,type="l")
points(shuttle$Temp,shuttle$Damage,pch=16,col="red")




qt((.05/2), 18)
1-qt((.05/2), 18)






### Hypothesis test for Linear regression