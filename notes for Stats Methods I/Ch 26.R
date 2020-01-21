################# UNDERSTABLE ANOVA ###############

#must be a character vector to assign levels #make factor variable
type <- factor((c(rep("compact", 3), rep("midsize", 3), rep("fullsize", 3))))
type
typeof(type)

pressure <- c(643, 655, 702, 469, 427, 525, 484, 456, 402)
pressure

ntsb <- data.frame(type, pressure)
ntsb

boxplot(pressure ~ type,
        data = ntsb,
        col = rainbow(5),
        main = "pressure by car type",
        xlab = "car type",
        ylab = "pressure")
myaov <- aov(pressure ~ type, data = ntsb)
summary(myaov)

#checking normality
qqnorm(ntsb$pressure)


tukey.test <- TukeyHSD(myaov)
tukey.test

plot(tukey.test)