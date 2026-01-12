#----load packages----
library(dplyr)
library(ggplot2)
library(readr)
library(tibble)
library(boot)

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

#----Patients type 1----
#----number daily patients (t1)----
#number of patients is a Poisson distribution (given in the case)

#Daily counts of Type 1 patients 
daily_counts_t1 <- summarise(
  group_by(type1, Date),
  n_patients = n(),
  .groups = "drop"
)

daily_n1 <- daily_counts_t1$n_patients
print(daily_n1)

lambda_hat <- mean(daily_n1)  #find the mean of the number of daily patients
lambda_hat <- round(lambda_hat, digits= 2)  #round up to have simple number for hospital
print(lambda_hat)
cat("Mean of number of patients of type 1 daily is:", lambda_hat,"\n")

sd_daily_n1 <- sd(daily_n1) #get the standard deviation for number of patients daily
sd_daily_n1 <- round(sd_daily_n1, digits= 2)  #round up number
print(sd_daily_n1)
cat("Standard deviance of number of patients of type 1 daily is:", sd_daily_n1,"\n")

#----bootstrap the uncertainty of daily patients (t1) parameters----
lambda_estimate <- function(data, indices) {
  resampled_data <- data[indices] # get new resampled data
  mean(resampled_data)  # calculate the new mean
}

sigma_estimate <- function(data, indices) {
  resampled_data <- data[indices] # get new resampled data
  sd(resampled_data)  # calculate the new standard deviation
}

# Bootstrapping part
n_boot <- 1000  # number of times for bootstrap replicates

# Bootstrap for mean
boot_results_daily1 <- boot(data = daily_n1,
                            statistic = lambda_estimate,
                            R = n_boot)

# Calculate CI for bootstrap at 0.95 for mean
boot_ci_daily_n1 <- boot.ci(boot_results_daily1, conf = 0.95, type = "bca")
results_boot_ci_d1 <- round(boot_ci_daily_n1$bca[4:5], digits = 2)
cat("Estimated lambda (mean daily arrivals for Type 1):", round(boot_results_daily1$t0, digits = 2), "\n")
cat("95% Confidence Interval for lambda:", results_boot_ci_d1, "\n")

# Bootstrap for standard deviation
boot_results_daily1_sd <- boot(data = daily_n1,
                               statistic = sigma_estimate,
                               R = n_boot)

# Calculate CI for bootstrap at 0.95 for standard deviation
boot_ci_daily_n1_sd <- boot.ci(boot_results_daily1_sd, conf = 0.95, type = "bca")
results_boot_ci_d1_sd <- round(boot_ci_daily_n1_sd$bca[4:5], digits = 2)
cat("Estimated sigma (standard deviation of daily arrivals for Type 1):", round(boot_results_daily1_sd$t0, digits = 2), "\n")
cat("95% Confidence Interval for sigma:", results_boot_ci_d1_sd, "\n")


#----scan duration patients type 1 ----
#----parameters for duration----
mean_dur_1 <- mean(data_1d)  #get the mean of the duration for the scans of type 1
mean_dur_1 <- round(mean_dur_1, digits= 2)  #round number of minutes
mean_dur_1
cat("Mean of duration for scans for patients type 1 is:", mean_dur_1, "\n")

sd_dur_1 <- sd(data_1d) #get the sd of type 1 for duration
sd_dur_1 <- round(sd_dur_1, digits= 2)
sd_dur_1
cat("SD of duration for scans for patients type 1 is:", sd_dur_1, "\n")


#----visualize distribution of duration type 1----
#histogram to get distribution of duration for type 1
ggplot(type1, aes(x = Duration)) +
  geom_histogram(aes(y = after_stat(density)), bins = 20, fill = "blue", color = "black", alpha = 0.8) +
  stat_function(fun = dnorm,
                args = list(mean = mean_dur_1, sd = sd_dur_1),
                color = "red", linewidth = 1.2) +
  labs(title = "Type 1 Scan Duration with Normal Curve",
       x = "Duration (minutes)", y = "Density") +
  theme_minimal()

#the line over the histogram fits the normal distribution with mu = 25.71 (mean_dur_1)
#and sigma = 5.84 (sd_dur_1)

#----bootstrap for duration parameters----
duration_estimate <- function(data, indices) {
  resampled_data <- data[indices] #get new resampled data of scan durations
  c(mean= mean(resampled_data), sd= sd(resampled_data))  #calculate always the new mean
}

#bootstraping part
n_boot <- 1000  #number of times for bootstrap replicates

boot_results_duration1 <- boot(data= data_1d,
                               statistic = duration_estimate,
                               R= n_boot)

#calculate CI for bootstrap at 0.95
boot_ci_duration1_mean <- boot.ci(boot_results_duration1, conf = 0.95, type = "bca", index=1)
print(boot_ci_duration1_mean)

boot_ci_duration1_sd <- boot.ci(boot_results_duration1, conf = 0.95, type = "bca", index=2)
print(boot_ci_duration1_sd)

cat("Estimated mean duration for Type 1:",
    round(boot_results_duration1$t0[1], digits=2), "\n")
cat("95% Confidence Interval for mean duration for Type 1",round(boot_ci_duration1_mean$bca[4:5],digits= 2),"\n")
cat("Estimated sd duration for Type 1:",
    round(boot_results_duration1$t0[2], digits=2), "\n")
cat("95% Confidence Interval for sd duration for Type 1",
    round(boot_ci_duration1_sd$bca[4:5], digits= 2),"\n")

#----Patients type 2----
#----number daily patients (t2)----
#number of patients is a Poisson distribution (given in the case)

#Daily counts of Type 2 patients 
daily_counts_t2 <- summarise(
  group_by(type2, Date),
  n_patients = n(),
  .groups = "drop"
)

daily_n2 <- daily_counts_t2$n_patients
print(daily_n2)

lambda_hat_t2 <- mean(daily_n2)  #find the mean of the number of daily patients
lambda_hat_t2 <- round(lambda_hat_t2, digits= 2)  #round up to have simple number for hospital
print(lambda_hat_t2)
cat("Mean of number of patients of type 2 daily is:", lambda_hat_t2,"\n")

sd_daily_n2 <- sd(daily_n2) #get the standard deviation for number of patients daily
sd_daily_n2 <- round(sd_daily_n2, digits= 2)  #round up number
print(sd_daily_n2)
cat("Standard deviance of number of patients of type 2 daily is:", sd_daily_n2,"\n")

#----bootstrap the uncertainty of daily patients (t2) parameters----
lambda_estimate_t2 <- function(data, indices) {
  resampled_data <- data[indices] #get new resampled data
  mean(resampled_data)  #calculate always the new mean
}

sigma_estimate_t2 <- function(data, indices) {
  resampled_data <- data[indices] # get new resampled data
  sd(resampled_data)  # calculate the new standard deviation
}

#bootstraping part
n_boot <- 1000  #number of times for bootstrap replicates

#bootstrap
boot_results_daily2 <- boot(data= daily_n2,
                            statistic = lambda_estimate_t2,
                            R= n_boot)

#calculate CI for bootstrap at 0.95
boot_ci_daily_n2 <- boot.ci(boot_results_daily2, conf = 0.95, type = "bca")
print(boot_ci_daily_n2)
results_boot_ci_d2 <- round(boot_ci_daily_n2$bca[4:5], digits=2)
results_boot_ci_d2
cat("Estimated lambda_t2 (mean daily arrivals for Type 2):", round(boot_results_daily2$t0, digits=2), "\n")
cat("95% Confidence Interval for lambda_t2:", results_boot_ci_d2, "\n")


# Bootstrap for standard deviation
boot_results_daily2_sd <- boot(data = daily_n2,
                               statistic = sigma_estimate_t2,
                               R = n_boot)

# Calculate CI for bootstrap at 0.95 for standard deviation
boot_ci_daily_n2_sd <- boot.ci(boot_results_daily2_sd, conf = 0.95, type = "bca")
results_boot_ci_d2_sd <- round(boot_ci_daily_n2_sd$bca[4:5], digits = 2)
cat("Estimated sigma (standard deviation of daily arrivals for Type 2):",
    round(boot_results_daily2_sd$t0, digits = 2), "\n")
cat("95% Confidence Interval for sigma:", results_boot_ci_d2_sd, "\n")

#----distribution inter-arrival times ----
# Plot histogram of times between arrival for Type 2 patients
type2_sorted <- type2 %>% arrange(Time)

inter_arrival_times <- diff(type2_sorted$Time)

ggplot(data.frame(inter_arrival_times), aes(x = inter_arrival_times)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "steelblue", color = "black", alpha = 0.8) +
  labs(title = "Distribution of Inter-Arrival Times for Type 2 Patients",
       x = "Inter-Arrival Time (minutes)",
       y = "Density") +
  theme_minimal()

#checking for exponential distribution
fit_exp <- fitdist(inter_arrival_times, "exp", method = "mle")
plot(fit_exp)

ggplot(data.frame(inter_arrival_times), aes(x = inter_arrival_times)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "blue", color = "black", alpha = 0.8) +
  stat_function(fun = dexp,
                args = (rate = fit_exp$estimate["rate"]),
                color = "red", linewidth = 1.2) +
  labs(title = "Distribution of Inter-Arrival Times for Type 2 Patients",
       x = "Inter-Arrival Time (minutes)",
       y = "Density") +
  theme_minimal()


#checking for gamma distribution for arrival times
fit_gamma_at <- fitdist(inter_arrival_times, "gamma", method = "mle")
plot(fit_gamma_at)


ggplot(data.frame(inter_arrival_times), aes(x = inter_arrival_times)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "blue", color = "black", alpha = 0.8) +
  stat_function(fun = dgamma,
                args = list(shape= fit_gamma_at$estimate["shape"], rate = fit_gamma_at$estimate["rate"]),
                color = "red", linewidth = 1.2) +
  labs(title = "Distribution of Inter-Arrival Times for Type 2 Patients",
       x = "Inter-Arrival Time (minutes)",
       y = "Density") +
  theme_minimal()

cat("Exponential AIC:", AIC(fit_exp), "\n")
cat("Gamma AIC:", AIC(fit_gamma_at), "\n")

cat("Exponential BIC:", BIC(fit_exp), "\n")
cat("Gamma BIC:", BIC(fit_gamma_at), "\n")

true_mean_exponential <- 1/(fit_exp$estimate["rate"])
cat("The mean of the inter-arrival types for t2 patients
    with the exp distribution is", round((true_mean_exponential), 2), "\n")

true_sd_exponential <- 1/sqrt((fit_exp$estimate["rate"]))
cat("The sd of the inter-arrival types for t2 patients
    with the exp distribution is", round((true_sd_exponential), 2), "\n")


#----scan duration patients type 2 ----
#----parameters for duration----
mean_dur_2 <- mean(data_2d)  #get the mean of the duration for the scans of type 1
mean_dur_2 <- round(mean_dur_2, digits= 2)  #round number of minutes
mean_dur_2

sd_dur_2 <- sd(data_2d) #get the sd of type 1 for duration
sd_dur_2 <- round(sd_dur_2, digits= 2)
sd_dur_2

#histogramm for type 2 arrival patients
ggplot(type2, aes(x = Duration)) +
  geom_histogram(aes(y = after_stat(density)), bins = 20, fill = "salmon", color = "black", alpha = 0.8) +
  stat_function(fun = dnorm,
                args = list(mean = mean_dur_2, sd = sd_dur_2),
                color = "red", linewidth = 1.2) +
  labs(title = "Type 2 Scan Duration with Normal Curve",
       x = "Duration (minutes)", y = "Density") +
  theme_minimal()

#----bootstrap for number patients type 2 ----
# Bootstrap for Type 2 # of patients per day
max_daily_n2 <- max(daily_n2)
print(max_daily_n2)

min_daily_n2 <- min(daily_n2)
print(min_daily_n2)

mean_daily_n2 <- mean(daily_n2)
sd_daily_n2 <- sd(daily_n2)

set.seed(42)
B <- 999
alpha <- 0.05
n_days <- length(daily_n2)

# Initialize vector to store bootstrap mean and standard deviation 
boot_mean_daily <- numeric(B)
boot_sd_daily <- numeric(B)


for (b in 1:B){
  sample_days <- sample.int(n_days, size=n_days, replace = TRUE)
  sample_counts <- daily_n2[sample_days]
  boot_mean_daily[b] <- mean(sample_counts)
  boot_sd_daily[b] <- sd(sample_counts)
}

# Bootstrap percentile confidence intervals 
ci_mean_daily <- quantile(boot_mean_daily, probs = c(alpha/2, 1 - alpha/2))
print(ci_mean_daily)
ci_sd_daily <- quantile(boot_sd_daily, probs = c(alpha/2, 1 - alpha/2))
print(ci_sd_daily)


#----bootstrap for duration type 2----
# Bootstrap for Type 2 Duration (non-parametric)

set.seed(42) #set seed for reproducibility
B <- 999       # number of bootstrap resamples
alpha <- 0.05

X <- type2$Duration   #scan durations for type 2 patients
n <- length(X)
X_bar <- mean(X)
St.Dev <- sd(X)

# Initialize vector for studentized statistics
Q_star <- numeric(B)

for (b in 1:B) {
  # bootstrap sample
  X_star <- sample(X, size = n, replace = TRUE)
  X_bar_star <- mean(X_star)
  St.Dev_star <- sd(X_star)
  
  # studentized statistic
  Q_star[b] <- (X_bar_star - X_bar) / St.Dev_star
}

# Studentized bootstrap CI for the mean

# Percentiles of the studentized stats
q_low <- quantile(Q_star, alpha/2)
q_high <- quantile(Q_star, 1 - alpha/2)

# Transform back to original mean scale
ci_mean <- c(
  X_bar - q_high * St.Dev,
  X_bar - q_low * St.Dev
)

cat("Studentized bootstrap 95% CI for mean Duration:",
    ci_mean,"\n")

# Bootstrap median, 90th percentile, and SD (percentile)

boot_median <- numeric(B)
boot_p90 <- numeric(B)
boot_sd <- numeric(B)

for (b in 1:B){
  samp <- sample(X, size = n, replace = TRUE)
  boot_median[b] <- median(samp)
  boot_p90[b] <- quantile(samp, 0.9)
  boot_sd[b] <- sd(samp)
}

ci_median <- quantile(boot_median, c(alpha/2, 1-alpha/2))
ci_p90 <- quantile(boot_p90, c(alpha/2, 1-alpha/2))
ci_sd <- quantile(boot_sd, c(alpha/2, 1-alpha/2))

cat("Bootstrap 95% CI for median:\n"); print(ci_median)
cat("Bootstrap 95% CI for 90th percentile:\n"); print(ci_p90)
cat("Bootstrap 95% CI for SD:\n"); print(ci_sd)

#----visualize the scan duration distribution ----
ggplot(type2, aes(x=Duration))+
  geom_histogram(aes(y= after_stat(density)), bins=20, fill= "steelblue", color= "black",
                 alpha = 0.8)+
  labs(title= "Type 2 Scan duration (Empirical Distribution)",
       x= "Duration (minutes)", y= "Density")+
  theme_minimal()


#----finding appropriate distribution for type 2 scan durations ----
library(fitdistrplus)

fit_gamma <- fitdist(type2$Duration, "gamma", method = "mle")
fit_lognormal <- fitdist(type2$Duration, "lnorm", method = "mle")


plot(fit_gamma)
plot(fit_lognormal)

#aic/ bic to compare
AIC(fit_gamma)
AIC(fit_lognormal)
BIC(fit_gamma)
BIC(fit_lognormal)

#----MC Sim for gamma dist----
n_d2 <- length(data_2d)

set.seed(42)                           # Set the seed for the random number generator
nr.sim <- 5000                          # Number of simulations
n <- n_d2                               # Size is number of observations of scans for t2 patients
alpha <- 0.05                           # Nominal level of the test

sim_mean_gamma <- rep(0, times = nr.sim)      # Initialise a vector of 0s to store means
sim_sd_gamma <- rep(0, times = nr.sim)      # Initialise a vector of 0s to store sd

for (i in 1:nr.sim){                    # Start the simulations
  ## Step 1: Simulate ##
  X <- rgamma(n, shape = fit_gamma$estimate["shape"], rate = fit_gamma$estimate["rate"])   # Draw X
  
  ## Step 2: Apply ##
  sim_mean_gamma[i] <- mean(X)                    # Sample mean of X
  sim_sd_gamma[i] <- sd(X)                     # Standard deviation of X
  
}

## Step 3: Summarize ##
true_mean_gamma <- fit_gamma$estimate["shape"]/ fit_gamma$estimate["rate"]
true_sd_gamma <- sqrt(fit_gamma$estimate["shape"])/ fit_gamma$estimate["rate"]

cat("True mean:", round(true_mean_gamma, 2), "\n")
cat("Mean of simulated means:", round(mean(sim_mean_gamma), 2), "\n")
cat("True standard deviation:", round(true_sd_gamma,2), "\n")
cat("Mean of simulated standard deviations:", round(mean(sim_sd_gamma), 2), "\n")


#----MC Sim for lognormal dist----
set.seed(42)                           # Set the seed for the random number generator
nr.sim <- 5000                          # Number of simulations
n <- n_d2                               # Size is number of observations of scans for t2 patients
alpha <- 0.05                           # Nominal level of the test

sim_mean_lognorm <- rep(0, times = nr.sim)      # Initialise a vector of 0s to store means
sim_sd_lognorm <- rep(0, times = nr.sim)      # Initialise a vector of 0s to store sds

for (i in 1:nr.sim){                    # Start the simulations
  ## Step 1: Simulate ##
  X <- rlnorm(n, meanlog = fit_lognormal$estimate["meanlog"], sdlog = fit_lognormal$estimate["sdlog"])   # Draw X
  
  ## Step 2: Apply ##
  sim_mean_lognorm[i] <- mean(X)                    # Sample mean of X
  sim_sd_lognorm[i] <- sd(X)                     # Standard deviation of X
}

## Step 3: Summarize ##
mu_l <- fit_lognormal$estimate["meanlog"]
sigma_l <- fit_lognormal$estimate["sdlog"]

true_mean_lognorm <- exp(mu_l + (sigma_l)^2/2)
true_sd_lognorm <- sqrt((exp((sigma_l)^2)-1)*exp(2*mu_l+(sigma_l)^2))

cat("True mean:", round(true_mean_lognorm, 2),"\n")
cat("Mean of simulated means:", round(mean(sim_mean_lognorm),2), "\n")
cat("True standard deviation:", round(true_sd_lognorm,2), "\n")
cat("Mean of simulated standard deviations:", round(mean(sim_sd_lognorm), 2), "\n")

#----visualize log norm dist----
#----visualize the scan duration distribution ----
ggplot(type2, aes(x=Duration))+
  geom_histogram(aes(y= after_stat(density)), bins=20, fill= "darkgreen", color= "black",
                 alpha = 0.8)+
  stat_function(fun = dlnorm,
                args = list(meanlog = fit_lognormal$estimate["meanlog"], sdlog = fit_lognormal$estimate["sdlog"]),
                color = "red", linewidth = 1.2) +
  labs(title = "Type 2 Scan Duration with Lognormal Curve",
       x= "Duration (minutes)", y= "Density")+
  theme_minimal()

#----visualize gamma dist----
#----visualize the scan duration distribution ----
ggplot(type2, aes(x=Duration))+
  geom_histogram(aes(y= after_stat(density)), bins=20, fill= "orange", color= "black",
                 alpha = 0.8)+
  stat_function(fun = dgamma,
                args = list(shape = fit_gamma$estimate["shape"], rate = fit_gamma$estimate["rate"]),
                color = "red", linewidth = 1.2) +
  labs(title = "Type 2 Scan Duration with Gamma Curve",
       x= "Duration (minutes)", y= "Density")+
  theme_minimal()
#----end----