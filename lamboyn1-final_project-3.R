#set your working directory 
setwd("/courses/STA145/lamboyn1")

#upload data
library(haven)
governordata <- read.csv("governordata.csv")
View(governordata)

#Two variables are gender and military

#View qualitative variable
table(governordata$gender)

#View qualitative variable 
table(governordata$military)

#Descriptive statistics for gender variable
mean(governordata$gender)

sd(governordata$gender)

table(governordata$gender)

govdata <- data.frame(
gender = c(1, 2),
military = c(1, 2, 3, 4, 5, 6, 7))
Hmisc::describe(governordata)

summary(governordata$gender)

#Descriptive statistics for military variable
mean(governordata$military)

sd(governordata$military)

table(governordata$military)

govdata <- data.frame(
gender = c(1, 2),
military = c(1, 2, 3, 4, 5, 6, 7))
Hmisc::describe(governordata)

summary(governordata$gender)

#Contingency table
gender <- c(1,2)
military <- c(1,2,3,4,5,6,7)

contingency_table <- table(governordata$gender, governordata$military)
print(contingency_table)

#Chi square test
result <- chisq.test(contingency_table)
print(result)

#Scatterplot
plot(x = governordata$gender, y = governordata$military,
     main = "gender and military", 
     xlab = "gender",    
     ylab = "military")    

plot <- lm(gender ~ military, data = governordata)
summary(plot)

abline(plot, col = "blue", lwd = 2)

#Residual plot
residualplot <- lm(governordata$military ~ governordata$gender, data = governordata)

residuals <- resid(residualplot)

fitted <- fitted(residualplot)

plot(fitted, residuals,
     xlab = "gender",
     ylab = "military",
     main = "gender and military",
     pch = 16) 

abline(h = 0, col = "blue", lty = 2, lwd = 2) 






