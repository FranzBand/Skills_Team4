##Type-2 corrected

## Type-2 Patients analysis

library(dplyr)
library(ggplot2)
library(readr)

# Load data
data <- read_csv("ScanRecords.csv", show_col_types = FALSE)

# Convert Duration (hours -> minutes) and ensure Time is numeric (hours)
data <- mutate(data,
               Duration = Duration * 60,  # minutes
               Time = as.numeric(Time)    # fraction of hours (e.g., 9.5)
)


# Separate patients by type
split_data <- split(data, data$PatientType)
type1 <- split_data$"Type 1"
type2 <- split_data$"Type 2"

# Working hours
start_hour <- 8
end_hour <- 17
overnight_gap <- 0


n2 <- nrow(type2)  # number of Type 2 patients
print(n2)


# Daily counts of Type 2 patients 
daily_counts_t2 <- summarise(
  group_by(type2, Date),
  n_patients = n(),
  .groups = "drop"
)

daily_n2 <- daily_counts_t2$n_patients
print(daily_n2)
mean_daily_n2 <- mean(daily_n2)
print(mean_daily_n2)
sd_daily_n2 <- sd(daily_n2)
print(sd_daily_n2)

# Plot Type 2 Duration
mu_dur2 <- mean(type2$Duration)
sigma_dur2 <- sd(type2$Duration)

ggplot(type2, aes(x = Duration)) +
  geom_histogram(aes(y = after_stat(density)), bins = 20, fill = "salmon", color = "black", alpha = 0.8) +
  stat_function(fun = dnorm,
                args = list(mean = mu_dur2, sd = sigma_dur2),
                color = "red", linewidth = 1.2) +
  labs(title = "Type 2 Scan Duration with Normal Curve",
       x = "Duration (minutes)", y = "Density") +
  theme_minimal()

##DURATION ANALYSIS

max_X <- max(X)
print(max_X)

min_X <- min(X)
print(min_X)

# Bootstrap for Type 2 Duration (non-parametric)

set.seed(515)
B <- 999       # number of bootstrap resamples
alpha <- 0.05

X <- type2$Duration
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



## ANALYSIS : # of patients per day

# Bootstrap for Type 2 # of patients per day
daily_n2 <- daily_counts_t2$n_patients

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


# Summary metrics
summary_metrics <- list(
  Type2 = list(
    mean_duration_min = if (n2 > 0) mu_dur2 else NA,
    ci_duration_min = if (n2 > 0) ci_dur2 else c(NA, NA),
    mean_daily_patients = if (length(daily_n2) > 0) mean(daily_n2) else NA,
    sd_daily_patients = if (length(daily_n2) > 0) sd(daily_n2) else NA,
    ci_mean_daily = if (length(daily_n2) > 0) ci_mean_daily else c(NA, NA),
    ci_sd_daily = if (length(daily_n2) > 0) ci_sd_daily else c(NA, NA)
  )
)

print(summary_metrics)


# Graphs 


