library(readr)
cfb <- read_csv("~/git_projects/notes for Stats Methods I/CFB2018complete.csv")
head(cfb)
tail(cfb)
options(scipen = 999)

plot(cfb$z_lysagarin, cfb$Zsagarin, 
     xlab = "last year's performance", 
     ylab = "this year's performance")

plot(cfb$Fravg, cfb$Zsagarin,
     xlab = "average star rating of freshman",
     ylab = "this year's performance",
     main = "freshman class and performance")

lastyear <- lm(cfb$Zsagarin ~ cfb$z_lysagarin)
frclass <- lm(cfb$Zsagarin ~ cfb$Fravg)
multiple <- lm(cfb$Zsagarin ~ cfb$z_lysagarin + cfb$Fravg)

summary(lastyear)
summary(frclass)
summary(multiple)


################# showing collinearity ######################

#looking at the significance individually
summary(lm(cfb$Zsagarin ~ cfb$coachexp_total)) 
summary(lm(cfb$Zsagarin ~ cfb$coachexp_school))

#putting both models together
coachexp <- lm(cfb$Zsagarin ~ cfb$coachexp_school + cfb$coachexp_total)
summary(coachexp)

plot(cfb$coachexp_total ~ cfb$coachexp_school)

