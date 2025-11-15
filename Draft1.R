library(dplyr)
library(ggplot2)
library(readr)
data <- read.csv("ScanRecords.csv") #load data
View(data)

data <- mutate(data, Duration = Duration * 60 , Time = as.numeric(Time))


split_data <- split(data, data$PatientType) #split up the data for each type
View(split_data)
type1 <- split_data$"Type 1"  #type 1
type2 <- split_data$"Type 2"  #type 2

data_1d <- type1$Duration
data_2d <- type1$Duration

m_d_1 <- mean(data_1d)
m_d_1

m_d_2 <- mean(data_2d)
m_d_2

sd_d_1 <- sd(data_1d)
sd_d_1

sd_d_2<- sd(data_2d)
sd_d_2

ci_m_d_1 <- t.test(data_1d)$conf.int
ci_m_d_1

#AIIIII!!!!!!


# Calculate sample standard deviation
s <- sd(data_1d)
n <- length(data_1d)

# Confidence level (e.g., 95%)
conf.level <- 0.95
alpha <- 1 - conf.level

# Chi-squared critical values
chi2_lower <- qchisq(alpha/2, df = n - 1)
chi2_upper <- qchisq(1 - alpha/2, df = n - 1)

# Confidence interval for standard deviation
ci_sd_lower <- sqrt((n - 1) * s^2 / chi2_upper)
ci_sd_upper <- sqrt((n - 1) * s^2 / chi2_lower)

# Result
ci_sd <- c(ci_sd_lower, ci_sd_upper)
print(ci_sd)

