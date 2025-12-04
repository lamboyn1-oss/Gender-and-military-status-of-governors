#set your working directory 
setwd("/courses/STA145/lamboyn1")

#upload data
library(haven)
data <- read.csv("governordata.csv")
View(data)

#Two variables are gender and military

#View qualitative variable
table(data$gender)

#View qualitative variable 
table(data$military)

#Descriptive statistics for gender variable
mean(data$gender)

sd(data$gender)

table(data$gender)

data <- data.frame(
gender = c(0,1),
military = c(0,1))
Hmisc::describe(data)

summary(data$gender)

#Descriptive statistics for military variable
mean(data$military)

sd(data$military)

table(data$military)

data <- data.frame(
gender = c(0,1),
military = c(0,1))
Hmisc::describe(data)

summary(data$gender)

#Contingency table
gender <- c(0,1)
military <- c(0,1)

contingency_table <- table(data$gender, data$military)
print(contingency_table)

#Chi square test
result <- chisq.test(contingency_table)
print(result)

#Scatterplot
plot(x = data$gender, y = data$military,
     main = "gender and military", 
     xlab = "gender",    
     ylab = "military")    

plot <- lm(gender ~ military, data = data)
summary(plot)

abline(plot, col = "blue", lwd = 2)

#Residual plot
residualplot <- lm(data$military ~ data$gender, data = data)

residuals <- resid(residualplot)

fitted <- fitted(residualplot)

plot(fitted, residuals,
     xlab = "gender",
     ylab = "military",
     main = "gender and military",
     pch = 16) 

abline(h = 0, col = "blue", lty = 2, lwd = 2) 
