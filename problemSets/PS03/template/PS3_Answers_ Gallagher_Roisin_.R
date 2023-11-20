#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2023/main/datasets/incumbents_subset.csv")



## Questiun 1.1
# Single linear regression 

 model1 <- lm(voteshare ~ difflog, data = inc.sub)
 summary(model1)
 #R-squared is 0.3673, which means voteshare could only explain 36% of the variation in difflog

# question 1.2
library(ggplot2)

# scatterplot
ggplot(inc.sub, aes(x = difflog, y = voteshare)) + geom_point() +
  geom_smooth(method = "lm", se = FALSE)


#question 1.3
residuals1 <- residuals(model1)

#question 1.4
coeff1 <- coefficients(model1)
intercept1 = coeff1[1]
slope1 <- coeff1[2]
voteshare_1 = slope1 * inc.sub$difflog + intercept1

## Question 2.1
# Single linear regression

model2 <- lm(presvote ~ difflog, data = inc.sub)
summary(model2)

#question 2.2

#scatterplot
ggplot(inc.sub, aes(x = difflog, y = presvote)) + geom_point() +
  geom_smooth(method = "lm", se = FALSE)

#question 2.3

residuals2 <- residuals(model2)

# question 2.4 

coeff2 <- coefficients(model2)
intercept2 = coeff2[1]
slope2 <- coeff2[2]
presvote_2 = slope2 * inc.sub$difflog + intercept2


# question 3.1
# Single linear regression

model3 <- lm(voteshare ~ presvote, data = inc.sub)
summary(model3)
# R-squared is 0.2058, which means presvote could only explain 20% of the variation in voteshare


#question 3.2

#scatterplot
ggplot(inc.sub, aes(x = presvote, y = voteshare)) + geom_point() +
  geom_smooth(method = "lm", se = FALSE)


# question 3.3

coeff3 <- coefficients(model3)
intercept3 = coeff3[1]
slope3 <- coeff3[2]
voteshare_3 = slope3 * inc.sub$presvote + intercept3

# question 4.1


model4 <- lm(residuals1 ~ residuals2)
summary(model4)

#question 4.2

ggplot(inc.sub, aes(x = residuals1, y = residuals2)) + geom_point() +
  geom_smooth(method = "lm", se = FALSE)

#question 4.3

coeff4 <- coefficients(model4)
intercept4 = coeff4[1]
slope4 <- coeff4[2]
residuals1_4 = slope4 * residuals2 + intercept4

#question 5.1

model5 <- lm(voteshare ~ difflog + presvote, data = inc.sub)
summary(model5)

#question 5.2

coeff5 <- coefficients(model5)
intercept5 = coeff5[1]
slope_diff <- coeff5[2]
slope_pres <- coeff5[3]
voteshare_5 = slope_diff * inc.sub$difflog + slope_pres * inc.sub$presvote + 
  intercept5

#question 5.3 

summary(model4)
summary(model5)

# residual standard error for both models is the same: 0.07339
# p value for both models is the same: p-value: < 2.2e-16


