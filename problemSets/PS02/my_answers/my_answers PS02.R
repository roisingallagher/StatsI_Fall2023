
#googled how to read CSV file in r
#installed library(readr)
install.packages("readr")
iibrary(readr)

# question 1 
# a) I googled the chi sqaured test statistic formula (see PDF)

# the expected frequency for Upper class and Not Stopped = 13.5:
14 + 6 + 7
14 + 7
#grand total = 42
27 * 21
567 / 42

# the expected frequency for Upper class and Bribe requested = 8.375:
14 + 6 + 7
6 + 7
#grand total = 42
27 * 13
351 / 42

# the expected frequency for Upper class and Stopped/given warning = 5.142:
14 + 6 + 7
7 + 1
#grand total = 42
27 * 8
216 / 42

# the expected frequency for Lower class and Not stopped = 7.5:
7 + 7 + 1
14 + 7
# grand total = 42
15 * 21
315 / 42

# the expected frequency for Lower class and Bribe requested = 4.642:
7 + 7 + 1
6 + 7
# grand total = 42
15 * 13
195 / 42

# the expected frequency for Lower class and Stopped/given warning = 2.857:5
7 + 7 + 1
7 + 1
# grand total = 42
15 * 850
120 / 42

# The expected frequencies for the six cells in the contingency table are 
# stated above. I plugged these frequencies into chi squared test formula and 
# chi^2 = 0.0185 + 0.6735 + 0.6713 + 0.0333 + 1.1977 + 1.207
# chi^2 = 3.8013

p_value <- 1 - pchisq(3.8013, df = 2)


# Question 2
# a) See PDF 

# b) run a bivariate regression to test the hypothesis, 
# used "help" to find out formula for fitting linear models

women <- read_csv("women.csv")
model <- lm(reserved ~ water, data = women)
model

reserved = 0.302861 + 1 * 0.001824
reserved

# If I increase the reservation policy by 1 then water will increase by 0.304





























