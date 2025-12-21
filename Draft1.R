#----load packages----
library(dplyr)
library(ggplot2)
library(readr)
library(tibble)

#----load data----
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
View(data_2d)

data_1t <- type1$Time #load duration of type 1
data_2t <- type2$Time #load duration of type 2
View(data_1t)
View(data_2t)

#----understand number of patients daily----
# Daily counts of Type 1 patients 
daily_counts_t1 <- summarise(
  group_by(type1, Date),
  n_patients = n(),
  .groups = "drop"
)

daily_n1 <- daily_counts_t1$n_patients
print(daily_n1)
mean_daily_n1 <- mean(daily_n1)
print(mean_daily_n1)
sd_daily_n1 <- sd(daily_n1)
print(sd_daily_n1)

#----parameters for duration----
mean_dur_1 <- mean(data_1d)  #get the mean of type 1
mean_dur_1

mean_dur_2 <- mean(data_2d)  #get the mean of type 2
mean_dur_2

sd_dur_1 <- sd(data_1d)   #get the sd of type 1 for duration
sd_dur_1

sd_dur_2<- sd(data_2d)    #get the sd of type 2
sd_dur_2

ci_mean_dur_1 <- t.test(data_1d)
ci_mean_dur_1

ci_mean_dur_3 <- t.test(data_1d, mu=25)
ci_mean_dur_3

ci_mean_dur_2 <- t.test(data_2d)
ci_mean_dur_2

ci_mean_dur_4 <- t.test(data_2d, mu= 41)
ci_mean_dur_4

#----parameters for time----
mean_time_1 <- mean(data_1t)  #get the mean of type 1
mean_time_1

mean_time_2 <- mean(data_2t)  #get the mean of type 2
mean_time_2

sd_time_1 <- sd(data_1t)   #get the sd of type 1 for duration
sd_time_1

sd_time_2<- sd(data_2t)    #get the sd of type 2
sd_time_2

ci_mean_time_1 <- t.test(data_1t)
ci_mean_time_1

ci_mean_time_3 <- t.test(data_1t, mu=12.5)
ci_mean_time_3

ci_mean_time_2 <- t.test(data_2t)
ci_mean_time_2

ci_mean_time_4 <- t.test(data_2t, mu= 12.5)
ci_mean_time_4

#----visualize distribution of duration----
#histogram to get distribution of duration for type 1
ggplot(type1, aes(x = Duration)) +
  geom_histogram(aes(y = after_stat(density)), bins = 20, fill = "steelblue", color = "black", alpha = 0.8) +
  stat_function(fun = dnorm,
                args = list(mean = mean_dur_1, sd = sd_dur_1),
                color = "red", linewidth = 1.2) +
  labs(title = "Type 1 Scan Duration with Normal Curve",
       x = "Duration (minutes)", y = "Density") +
  theme_minimal()

#----Chi squared test for duration of both types----
#AIIIII!!!!!!
n_d1 <- length(data_1d)
n_d2 <- length(data_2d)

# Confidence level (e.g., 95%)
conf.level <- 0.95
alpha <- 1 - conf.level

# Chi-squared critical values for duration 1
chi2_lower_d1 <- qchisq(alpha/2, df = n_d1 - 1)
chi2_upper_d1 <- qchisq(1 - alpha/2, df = n_d1 - 1)

# Confidence interval for standard deviation of type 1
ci_sd_lower_d1 <- sqrt((n_d1 - 1) * sd_dur_1^2 / chi2_upper_d1)
ci_sd_upper_d1 <- sqrt((n_d1 - 1) * sd_dur_1^2 / chi2_lower_d1)

# Result
ci_sd_d1 <- c(ci_sd_lower_d1, ci_sd_upper_d1)
cat("CI for SD of duration (type1):",ci_sd_d1, "\n")

#type 2
# Chi-squared critical values
chi2_lower_d2 <- qchisq(alpha/2, df = n_d2 - 1)
chi2_upper_d2 <- qchisq(1 - alpha/2, df = n_d2 - 1)

# Confidence interval for standard deviation of type 1
ci_sd_lower_d2 <- sqrt((n_d2 - 1) * sd_dur_2^2 / chi2_upper_d2)
ci_sd_upper_d2 <- sqrt((n_d2 - 1) * sd_dur_2^2 / chi2_lower_d2)

# Result
ci_sd_d2 <- c(ci_sd_lower_d2, ci_sd_upper_d2)
cat("CI for SD of duration (type2):",ci_sd_d2, "\n")


#----quantiles for type 1 and 2----
#type1
t1 <- data_frame(type1) #get the dataframe of type 1
View(t1)

quant_d1 <- quantile(t1$Duration, na.rm=TRUE) #quantiles for duration of type 1
print(quant_d1)

quant_t1 <- quantile(t1$Time, na.rm=TRUE) #quantiles for times of type 1
print(quant_t1)

#for type 2
t2 <- data_frame(type2)
View(t2)

quant_d2 <- quantile(t2$Duration, na.rm=TRUE) #quantile for duration of type 2
print(quant_d2)

quant_t2 <- quantile(t2$Time, na.rm=TRUE) #quantile for time of type 2 times
print(quant_t2)

#to do: do all confidence intervals for mean, sd/ var, etc.!!!!!


#----CI time type 1----
n_t1 <- length(data_1t) #length data type 1$Time
# Confidence level (e.g., 95%)
conf.level <- 0.95
alpha <- 1 - conf.level

# Chi-squared critical values
chi2_lower_t1 <- qchisq(alpha/2, df = n_t1 - 1)
chi2_upper_t1 <- qchisq(1 - alpha/2, df = n_t1 - 1)

# Confidence interval for standard deviation
ci_sd_lower_t1 <- sqrt((n_t1 - 1) * sd_time_1^2 / chi2_upper_t1)
ci_sd_upper_t1 <- sqrt((n_t1 - 1) * sd_time_1^2 / chi2_lower_t1)

# Result
ci_sd_time1 <- c(ci_sd_lower_t1, ci_sd_upper_t1)
print(ci_sd_time1) #result CI sd for time Type 1

#confidence interval for means, time type 1
ci_m_t_1 <- t.test(data_1t)
ci_m_t_1

ci_m_t_3 <- t.test(data_1t, mu=12.5)
ci_m_t_3


#----CI duration type 1----
n_d1 <- length(data_1d) #length data type 1$Time

# Chi-squared critical values
chi2_lower_d1 <- qchisq(alpha/2, df = n_d1 - 1)
chi2_upper_d1 <- qchisq(1 - alpha/2, df = n_d1 - 1)

# Confidence interval for standard deviation
ci_sd_lower_d1 <- sqrt((n_d1 - 1) * sd_dur_1^2 / chi2_upper_d1)
ci_sd_upper_d1 <- sqrt((n_d1 - 1) * sd_dur_1^2 / chi2_lower_d1)

#Result of confidence interval for sd
ci_sd_d1 <- c(ci_sd_lower_d1, ci_sd_upper_d1)
print(ci_sd_d1) 

#confidence interval for means, duration type 1
ci_m_d_1 <- t.test(data_1d)
ci_m_d_1

ci_m_d_3 <- t.test(data_1d, mu=26)
ci_m_d_3


#----test to get probabilities threshold is exceeded----
threshold_type1 <- mean_dur_1 + 10  # Threshold: 10 minutes above the mean
above_threshold_type1 <- sum(data_1d > threshold_type1, na.rm = TRUE)
n_d1 <- length(data_1d)
p_hat_type1 <- above_threshold_type1/ n_d1

#Exact binomial confidence interval
ci_exact_type1 <- binom.test(above_threshold_type1, n_d1)$conf.int

cat("Threshold (mean + 10 minutes:", threshold_type1, "\n")
cat("Empirical probability:", p_hat_type1, "\n")
cat("Exact 95% CI for proportion:", ci_exact_type1, "\n")


p_norm_dur1 <- 1 - pnorm(threshold_type1, mean = mean_dur_1, sd = sd_dur_1)
cat("Probability (normal):", p_norm_dur1, "\n")


#type2 patients
threshold_type2 <- mean_dur_2 + 10  # Threshold: 10 minutes above the mean
above_threshold_type2 <- sum(data_2d > threshold_type2, na.rm = TRUE)

ci_exact_type2 <- binom.test(above_threshold_type2, n_d2)$conf.int
cat("Exact 95% CI for proportion:", ci_exact_type2, "\n")


p_norm_dur2 <- 1 - pnorm(threshold_type2, mean = mean_dur_2, sd = sd_dur_2)
cat("Probability (normal):", p_norm_dur2, "\n")

#----bootstrap----
library(boot)
boot_prob <- boot(data = data_1d,
                  statistic = function(x, idx) {
                    mean(x[idx] > threshold_type1, na.rm = TRUE)
                  },
                  R = 1000)
boot_ci <- boot.ci(boot_prob, type = "bca")
cat("Bootstrap 95% CI:", boot_ci$bca[4:5], "\n")


#----checking for exponential distribution----
library(fitdistrplus)
daily_counts_t1 <- type1 %>% group_by(Date) %>% summarise(n_patients = n())
fit_poisson <- fitdist(daily_counts_t1$n_patients, "pois")
summary(fit_poisson)
plot(fit_poisson)

