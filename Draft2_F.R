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
#to do: get the mean and then CI with bootstrap

#Daily counts of Type 1 patients 
daily_counts_t1 <- summarise(
  group_by(type1, Date),
  n_patients = n(),
  .groups = "drop"
)

daily_n1 <- daily_counts_t1$n_patients
print(daily_n1)

lambda_hat <- mean(daily_n1)  #find the mean of the number of daily patients
lambda_hat <- round(lambda_hat, digits= 0)  #round up to have simple number for hospital
print(lambda_hat)
cat("Mean of number of patients of type 1 daily is:", lambda_hat,"\n")
  
sd_daily_n1 <- sd(daily_n1) #get the standard deviation for number of patients daily
sd_daily_n1 <- round(sd_daily_n1, digits= 0)  #round up number
print(sd_daily_n1)
cat("Standard deviance of number of patients of type 1 daily is:", sd_daily_n1,"\n")

#----bootstrap the uncertainty of daily patients (t1) parameters----
lambda_estimate <- function(data, indices) {
  resampled_data <- data[indices] #get new resampled data
  mean(resampled_data)  #calculate always the new mean
}

#bootstraping part
n_boot <- 1000  #number of times for bootstrap replicates

#bootstrap
boot_results_daily1 <- boot(data= daily_n1,
                     statistic = lambda_estimate,
                     R= n_boot)

#calculate CI for bootstrap at 0.95
boot_ci_daily_n1 <- boot.ci(boot_results_daily1, conf = 0.95, type = "bca")
print(boot_ci_daily_n1)
results_boot_ci_d1 <- round(boot_ci_daily_n1$bca[4:5], digits=0)
results_boot_ci_d1
cat("Estimated lambda (mean daily arrivals for Type 1):", round(boot_results$t0, digits=0), "\n")
cat("95% Confidence Interval for lambda:", results_boot_ci_d1, "\n")

#----scan duration patients type 1 ----
#----parameters for duration----
mean_dur_1 <- mean(data_1d)  #get the mean of the duration for the scans of type 1
mean_dur_1 <- round(mean_dur_1, digits= 2)  #round number of minutes
mean_dur_1

sd_dur_1 <- sd(data_1d) #get the sd of type 1 for duration
sd_dur_1 <- round(sd_dur_1, digits= 2)
sd_dur_1

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
    round(boot_results_duration1$t0[1], digits=0), "\n")
cat("95% Confidence Interval for mean duration for Type 1",
    round(boot_ci_duration1_mean$bca[4:5]), digits= 0,"\n")
cat("Estimated sd duration for Type 1:",
    round(boot_results_duration1$t0[2], digits=0), "\n")
cat("95% Confidence Interval for sd duration for Type 1",
    round(boot_ci_duration1_sd$bca[4:5]), digits= 0,"\n")

#----Patients type 2----
#----number daily patients (t2)----
#number of patients is a Poisson distribution (given in the case)
#to do: get the mean and then CI with bootstrap

#Daily counts of Type 2 patients 
daily_counts_t2 <- summarise(
  group_by(type2, Date),
  n_patients = n(),
  .groups = "drop"
)

daily_n2 <- daily_counts_t2$n_patients
print(daily_n2)

lambda_hat_t2 <- mean(daily_n2)  #find the mean of the number of daily patients
lambda_hat_t2 <- round(lambda_hat_t2, digits= 0)  #round up to have simple number for hospital
print(lambda_hat_t2)
cat("Mean of number of patients of type 2 daily is:", lambda_hat_t2,"\n")

sd_daily_n2 <- sd(daily_n2) #get the standard deviation for number of patients daily
sd_daily_n2 <- round(sd_daily_n2, digits= 0)  #round up number
print(sd_daily_n2)
cat("Standard deviance of number of patients of type 2 daily is:", sd_daily_n2,"\n")

#----bootstrap the uncertainty of daily patients (t2) parameters----
lambda_estimate_t2 <- function(data, indices) {
  resampled_data <- data[indices] #get new resampled data
  mean(resampled_data)  #calculate always the new mean
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
results_boot_ci_d2 <- round(boot_ci_daily_n2$bca[4:5], digits=0)
results_boot_ci_d2
cat("Estimated lambda_t2 (mean daily arrivals for Type 2):", round(boot_results_daily2$t0, digits=0), "\n")
cat("95% Confidence Interval for lambda_t2:", results_boot_ci_d2, "\n")

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

#----bootstrap (by heloise) for number patients type 2 ----

# Bootstrap for Type 2 # of patients per day
max_daily_n2 <- max(daily_n2)
print(max_daily_n2)

min_daily_n2 <- min(daily_n2)
print(min_daily_n2)

mean_daily_n2 <- mean(daily_n2)
sd_daily_n2 <- sd(daily_n2)

set.seed(999)
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


#----bootstrap (by heloise) for duration type 2----
max_X <- max(X)
print(max_X)

min_X <- min(X)
print(min_X)

# Bootstrap for Type 2 Duration (non-parametric)

set.seed(515) #set seed for reproducibility
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

cat("Studentized bootstrap 95% CI for mean Duration:\n")
print(ci_mean)

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



