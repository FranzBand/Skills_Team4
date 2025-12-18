library(dplyr)
library(ggplot2)
library(readr)
data <- read.csv("ScanRecords.csv") #load data
View(data)

data_n <- mutate(data, Duration = Duration * 60 , Time = as.numeric(Time)) #mutate 
#the data so everything is shown in minutes
View(data_n)

split_data <- split(data_n, data$PatientType) #split up the data for each type
View(split_data)
type1 <- split_data$"Type 1"  #type 1
type2 <- split_data$"Type 2"  #type 2

data_1d <- type1$Duration #load duration of type 1
data_2d <- type2$Duration #load duration of type 2
View(data_1d)
m_d_1 <- mean(data_1d)  #get the mean of type 1
m_d_1

m_d_2 <- mean(data_2d)  #get the mean of type 2
m_d_2

sd_d_1 <- sd(data_1d)   #get the sd of type 1 for duration
sd_d_1

sd_d_2<- sd(data_2d)    #get the sd of type 2
sd_d_2

sd_t_1 <- sd(data_1t)   #get the sd of type 1 for time
sd_t_1

ci_m_d_1 <- t.test(data_1d)
ci_m_d_1

ci_m_d_3 <- t.test(data_1d, mu=25)
ci_m_d_3

ci_m_d_2 <- t.test(data_2d)
ci_m_d_2

ci_m_d_4 <- t.test(data_2d, mu= 41)
ci_m_d_4


#AIIIII!!!!!!
n_d1 <- length(data_1d)
n_d2 <- length(data_2d)

# Confidence level (e.g., 95%)
conf.level <- 0.95
alpha <- 1 - conf.level

# Chi-squared critical values
chi2_lower_d1 <- qchisq(alpha/2, df = n_d1 - 1)
chi2_upper_d1 <- qchisq(1 - alpha/2, df = n_d1 - 1)

# Confidence interval for standard deviation of type 1
ci_sd_lower_d1 <- sqrt((n_d1 - 1) * sd_d_1^2 / chi2_upper_d1)
ci_sd_upper_d1 <- sqrt((n_d1 - 1) * sd_d_1^2 / chi2_lower_d1)

# Result
ci_sd_d1 <- c(ci_sd_lower_d1, ci_sd_upper_d1)
print(ci_sd_d1)




#test on saturday

t1 <- data_frame(type1) #get the dataframe of type 1
View(t1)

quant_d1 <- quantile(t1$Duration, na.rm=TRUE) #quantiles for duration of type 1
print(quant_d1)

quant_t1 <- quantile(t1$Time, na.rm=TRUE) #quantiles for times of type 1
print(quant_t1)

#for type 2
t2 <- data_frame(type2)
t2 <- tibble(type2)
View(t2)

quant_d2 <- quantile(t2$Duration, na.rm=TRUE) #quantile for duration of type 2
print(quant_d2)

quant_t2 <- quantile(t2$Time, na.rm=TRUE) #quantile for time of type 2 times
print(quant_t2)

#to do: do all condidence intervals for mean, sd/ var, etc.!!!!!


#for meeting 20.10 CI time type 1
data_1t <- type1$Time #load duration of type 1
n_t1 <- length(data_1t) #length data type 1$Time

# Chi-squared critical values
chi2_lower_t1 <- qchisq(alpha/2, df = n_t1 - 1)
chi2_upper_t1 <- qchisq(1 - alpha/2, df = n_t1 - 1)

# Confidence interval for standard deviation
ci_sd_lower_t1 <- sqrt((n_t1 - 1) * sd_t_1^2 / chi2_upper_t1)
ci_sd_upper_t1 <- sqrt((n_t1 - 1) * sd_t_1^2 / chi2_lower_t1)

# Result
ci_sd_t1 <- c(ci_sd_lower_t1, ci_sd_upper_t1)
print(ci_sd_t1) #result CI sd for time Type 1

#confidence interval for means, time type 1
ci_m_t_1 <- t.test(data_1t)
ci_m_t_1

ci_m_t_3 <- t.test(data_1t, mu=12.5)
ci_m_t_3



#wald test
# Extract coefficient and standard error
beta_hat <- 37
se <- sd_d_1

# Hypothesized value (e.g., 0)
beta0 <- m_d_1

# Wald test statistic
W <- (beta_hat - beta0)^2 / se^2

# p-value
p_value <- 1 - pchisq(W, df = 1)

# 95% CI
ci <- beta_hat + c(-1, 1) * qnorm(0.975) * se

# Output
cat("Wald test statistic:", W, "\n")
cat("p-value:", p_value, "\n")
cat("95% CI:", ci, "\n")

#AI!!!!!!!
mean_d1 <- mean(data_1d)  # Mean duration for Type 1
threshold <- mean_d1 + 10  # Threshold: 10 minutes above the mean

above_threshold <- sum(data_1d > threshold, na.rm = TRUE)
n_d1 <- length(data_1d)  # Total number of observations
p_hat <- above_threshold / n_d1  # Sample proportion

# Hypothesized proportion (e.g., 0.5 for no effect, or another value)
p0 <- 0.5

# Standard error of the proportion
se <- sqrt(p_hat * (1 - p_hat) / n_d1)

# Wald test statistic
W <- (p_hat - p0)^2 / se^2

# p-value (chi-squared distribution with 1 df)
p_value <- 1 - pchisq(W, df = 1)

# 95% Wald confidence interval for the proportion
ci <- p_hat + c(-1, 1) * qnorm(0.975) * se

cat("Threshold (mean + 10 minutes):", threshold, "\n")
cat("Number of durations above threshold:", above_threshold, "\n")
cat("Sample proportion:", p_hat, "\n")
cat("Wald test statistic:", W, "\n")
cat("p-value:", p_value, "\n")
cat("95% CI for proportion:", ci, "\n")
