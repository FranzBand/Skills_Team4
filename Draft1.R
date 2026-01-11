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
mean_daily_n1 <- round(mean_daily_n1, digits= 0)
print(mean_daily_n1)
cat("Mean of number of patients of type 1 daily is:", mean_daily_n1,"\n")
sd_daily_n1 <- sd(daily_n1)
sd_daily_n1 <- round(sd_daily_n1, digits= 0)
print(sd_daily_n1)
cat("Standard deviance of number of patients of type 1 daily is:", sd_daily_n1,"\n")

#confidence interval for number patients type 1
ci_mean_daily_t1 <- t.test(daily_n1, mu=17)
confint_mean_daily_t1 <- round(ci_mean_daily_t1$conf.int, digits=0)
print(ci_mean_daily_t1)
print(confint_mean_daily_t1)

#quantile daily patients type 1
quant_daily_n1 <- quantile(daily_n1)
print("Quantiles for daily patients:")
print(quant_daily_n1)

#histogram to see it better
hist(daily_n1, xlab= "number of patients per day",
     main= "Daily patients frequency",
     col= "blue")

# Daily counts of Type 2 patients 
daily_counts_t2 <- summarise(
  group_by(type2, Date),
  n_patients = n(),
  .groups = "drop"
)

daily_n2 <- daily_counts_t2$n_patients
print(daily_n2)

mean_daily_n2 <- mean(daily_n2)
mean_daily_n2 <- round(mean_daily_n2, digits= 0)
print(mean_daily_n2)

sd_daily_n2 <- sd(daily_n2)
sd_daily_n2 <- round(sd_daily_n2, digits= 0)
print(sd_daily_n2)

#quantile daily patients type 2
quant_daily_n2 <- quantile(daily_n2)
print("Quantiles for daily patients:")
print(quant_daily_n2)

#histogram to see it better
hist(daily_n2, xlab= "number of patients per day",
     main= "Daily patients frequency",
     col= "red")

#----parameters for duration----
mean_dur_1 <- mean(data_1d)  #get the mean of type 1
mean_dur_1 <- round(mean_dur_1, digits= 2)
mean_dur_1

mean_dur_2 <- mean(data_2d)  #get the mean of type 2
mean_dur_2 <- round(mean_dur_2, digits= 2)
mean_dur_2

sd_dur_1 <- sd(data_1d) #get the sd of type 1 for duration
sd_dur_1 <- round(sd_dur_1, digits= 2)
sd_dur_1

sd_dur_2<- sd(data_2d)    #get the sd of type 2
sd_dur_2 <- round(sd_dur_2, digits= 2)
sd_dur_2

#confidence intervals for durations
ci_mean_dur_t1_mu0 <- t.test(data_1d)
ci_mean_dur_t1_mu0

ci_mean_dur_t1_mu25 <- t.test(data_1d, mu=25)
ci_mean_dur_t1_mu25

ci_mean_dur_t2_mu0 <- t.test(data_2d)
ci_mean_dur_t2_mu0

ci_mean_dur_t2_mu41 <- t.test(data_2d, mu= 41)
ci_mean_dur_t2_mu41

#----parameters for time----
mean_time_1 <- mean(data_1t)  #get the mean of type 1
mean_time_1 <- round(mean_time_1, digits= 2)
mean_time_1

mean_time_2 <- mean(data_2t)  #get the mean of type 2
mean_time_2 <- round(mean_time_2, digits= 2)
mean_time_2

sd_time_1 <- sd(data_1t)   #get the sd of type 1 for time
sd_time_1  <- round(sd_time_1 , digits= 2)
sd_time_1

sd_time_2<- sd(data_2t)    #get the sd of type 2
sd_time_2  <- round(sd_time_2 , digits= 2)
sd_time_2

#confidence interval times
ci_mean_time_t1_mu0 <- t.test(data_1t)
ci_mean_time_t1_mu0

ci_mean_time_t1_mu12.5 <- t.test(data_1t, mu=12.5)
ci_mean_time_t1_mu12.5

ci_mean_time_t2_mu0 <- t.test(data_2t)
ci_mean_time_t2_mu0

ci_mean_time_t2_mu12.5 <- t.test(data_2t, mu= 12.5)
ci_mean_time_t2_mu12.5

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

#histogram to get distribution of duration for type 2
ggplot(type2, aes(x = Duration)) +
  geom_histogram(aes(y = after_stat(density)), bins = 20, fill = "salmon", color = "black", alpha = 0.8) +
  stat_function(fun = dnorm,
                args = list(mean = mean_dur_2, sd = sd_dur_2),
                color = "red", linewidth = 1.2) +
  labs(title = "Type 2 Scan Duration with Normal Curve",
       x = "Duration (minutes)", y = "Density") +
  theme_minimal()

#


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

cat("Threshold (mean + 10 minutes):", threshold_type1, "\n")
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

#----monte carlo simulation with bootstrap----
set.seed(515)													# Set the seed
nr.sim <- 2000													# Number of simulations
B <- 499														# Number of bootstrap replications
n <- 100														# Sample size
alpha <- 0.05													# Nominal level of the test
mu <- 0															# Set the true value of the mean
reject <- rep(0, times = nr.sim)								# Vector to store rejections

for (i in 1:nr.sim){											# Start the simulations
  ## Step 1: Simulate ##
  X <- rnorm(n, mean = mu)									# Draw X
  
  ## Step 2: Apply ##
  X.bar <- mean(X)											# Sample mean of X
  St.Dev <- sd(X)												# Standard deviation of X
  Q <- sqrt(n)*X.bar / St.Dev									# Test statistic
  
  Q.star <- rep(NA, times = B)								# Vector for bootstrap quantities
  for (b in 1:B) {
    J <- sample.int(n, size = n, replace = TRUE)			# Draw the indices J
    X.star <- X[J]											# Draw the bootstrap sample
    X.bar.star <- mean(X.star)								# Bootstrap sample mean
    St.Dev.star <- sd(X.star)								# Bootstrap standard deviation
    Q.star[b] <- sqrt(n)*(X.bar.star - X.bar) / St.Dev.star	# Bootstrap test statistic
  }
  cv.star <- quantile(Q.star, probs = 1-alpha)				# Bootstrap critical value
  
  ## Step 3: Evaluate ##
  if (Q > cv.star) {reject[i] <- 1}							# Check if the null hypothesis is rejected
}

## Step 4: Summarize ##
ERF <- mean(reject)												# Empirical rejection frequency

## Give output on screen ##
if (mu == 0) {
  print(paste("Size using bootstrap:", ERF))
} else if (mu > 0) {
  print(paste("Power using bootstrap:", ERF))
}

#----MC Sim for duration type 1 patients----
set.seed(515)                           # Set the seed for the random number generator
nr.sim <- 5000                          # Number of simulations
n <- n_d1                            # Sample size
alpha <- 0.05                           # Nominal level of the test
mu_true <- mean_dur_1                   # Set the true value of the mean: 25.71 for size, > 0 for power
mu_null <- 25                           #null hypothesis mean
reject.n <- rep(0, times = nr.sim)      # Initialise a vector of 0s to store rejections
reject.t <- rep(0, times = nr.sim)      # Initialise a vector of 0s to store rejections

for (i in 1:nr.sim){                    # Start the simulations
  ## Step 1: Simulate ##
  X <- rnorm(n, mean = mu_true, sd= sd_dur_1)    # Draw X under alternative
  ## Step 2: Apply ##
  X.bar <- mean(X)                    # Sample mean of X
  St.Dev <- sd(X)                     # Standard deviation of X
  t.stat <- sqrt(n) * (X.bar- mu_null)/St.Dev         # Test statistic
  cv.t <- qt(1-alpha, n-1)            # one sided t critical value
  ## Step 3: Evaluate ##
  if (t.stat > cv.t) {reject.t[i] <- 1}  # Check if the null hypothesis is rejected
}

## Step 4: Summarize ##
ERF.t <- mean(reject.t)                 # Empirical rejection frequency (t cv)
## Give output on screen ##
if (mu_true == 25.71) {
  print(paste("Size using t cv:",ERF.t))
} else if (mu > 25.71) {
  print(paste("Power using t cv:",ERF.t))
}

power <- ERF.t
cat("power using t cv:", power, "\n")

#----probabilities of thresholds are exceeded----

# Example: Probability that a scan exceeds 40 minutes
threshold <- 40
empirical_prob <- mean(type2$Duration > threshold)
cat("Empirical probability that a scan exceeds", threshold, "minutes:", round(empirical_prob, 3), "\n")

empirical_prob_cases_abs <- empirical_prob* 100
cat("A scan exceeds", threshold, "minutes in:", round(empirical_prob_cases_abs, 0),"out of 100 cases","\n")



# Define a function to calculate the probability of exceeding the threshold
prob_exceed <- function(data, indices) {
  resampled_data <- data[indices]
  mean(resampled_data > threshold)
}

# Run the bootstrap
set.seed(123)
n_boot <- 1000
boot_results_prob <- boot(
  data = type2$Duration,
  statistic = prob_exceed,
  R = n_boot
)

# Calculate the 95% confidence interval
boot_ci_prob <- boot.ci(boot_results_prob, type = "bca", conf = 0.95)
cat("95% Confidence Interval for probability of exceeding", threshold, "minutes:",
    round(boot_ci_prob$bca[4:5], 3), "\n")


thresholds <- c(30, 40, 50)
for (t in thresholds) {
  empirical_prob <- mean(type2$Duration > t)
  boot_results_prob <- boot(type2$Duration, prob_exceed, R = n_boot)
  boot_ci_prob <- boot.ci(boot_results_prob, type = "bca", conf = 0.95)
  cat("Threshold:", t, "minutes\n")
  cat("Empirical probability:", round(empirical_prob, 3), "\n")
  cat("95% CI:", round(boot_ci_prob$bca[4:5], 3), "\n\n")
}