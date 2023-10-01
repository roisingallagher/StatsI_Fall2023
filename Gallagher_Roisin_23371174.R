# Question 1.1
# Given data
IQ_scores <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90,
               94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98) #reading the vector

# Calculate sample mean
sample_mean <- mean(IQ_scores) # calculating the average IQ

# Calculate sample standard deviation
sample_std <- sd(IQ_scores) # calculating the standard deviation of IQ

# Sample size
n <- length(IQ_scores) # number of the students in the dataset

# Confidence level (90%)
confidence_level <- 0.90 

# Calculate the critical t-value
critical_t <- 1.71 # we can find this critical t-value from a t-table by knowing the degrees of freedom and the confidence interval
# found at https://www.scribbr.com/statistics/students-t-table/

# Calculate the margin of error
margin_of_error <- critical_t * (sample_std / sqrt(n)) # calculating the error margin 

# Calculate the confidence interval
lower_bound <- sample_mean - margin_of_error
upper_bound <- sample_mean + margin_of_error

################
# Question 1.2

# National average IQ score
national_average <- 100 

# Perform one-sample t-test
t_test_result <- t.test(IQ_scores, mu = national_average, alternative = "greater")

# Print the results
cat("Test Statistic:", t_test_result$statistic, "\n")
cat("p-value:", t_test_result$p.value, "\n")

# Check the decision based on the p-value and alpha level
alpha <- 0.05
if (t_test_result$p.value < alpha) {
  cat("Reject the null hypothesis (H0)\n")
  cat("Average student IQ in the school is greater than 100.\n")
} else {
  cat("Fail to reject the null hypothesis (H0)\n")
  cat("There is not enough evidence to conclude that the average student IQ in the school is greater than 100.\n")
}

################################################################
#Question 2.1
library(dplyr)
library(ggplot2)

# reading the expenditure dataset
expenditure <- read.table("expenditure.txt", header = TRUE, sep = "\t")

# scatterplot for seein the relationship between Y and X1
ggplot(expenditure, aes(X1, Y)) + geom_point() # according to the plot there is a positive correlation between these two variables
corx1_y <- cor(expenditure$Y, expenditure$X1) # the correlation between these two is 0.53 

# scatterplot for seein the relationship between Y and X2
ggplot(expenditure, aes(X2, Y)) + geom_point() # according to the plot there is a non-linear relationship between these two variables, but if we focus on the correlation value only it is positive
corx2_y <- cor(expenditure$Y, expenditure$X2) # the correlation between these two is 0.44 

# scatterplot for seein the relationship between Y and X3
ggplot(expenditure, aes(X3, Y)) + geom_point() # according to the plot there is a positive correlation between these two variables
corx3_y <- cor(expenditure$Y, expenditure$X3) # the correlation between these two is 0.46 

# Question 2.2


expenditure$Region <- as.factor(expenditure$Region) # becaiuse we wanted to seperate regions from each other, we converted the type of the Region column from numeric to factor

# Create a box plot to visualize the relationship
ggplot(expenditure, aes(x = Region, y = Y)) +
  geom_boxplot() +
  labs(x = "Region", y = "Housing Assistance Expenditure (Y)")  #on average region 4 has the highest

# Question 2.3
# I have already plotted the first part of this question in the answer of question 2.1
# Create an extended scatterplot with Region
ggplot(expenditure, aes(x = X1, y = Y, color = Region, shape = Region)) +
  geom_point(size = 3) +
  labs(x = "X1", y = "Y") 


