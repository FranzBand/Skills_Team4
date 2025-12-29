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
#----number daily patients----
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

#bootstrap the uncertainty
lambda_estimate <- function(data, indices) {
  resampled_data <- data[indices] #get new resampled data
  mean(resampled_data)  #calculate always the new mean
}

#bootstraping part
n_boot <- 1000  #number of times for bootstrap replicates

boot_results <- boot(data= daily_n1,
                     statistic = lambda_estimate,
                     R= n_boot)

#calculate CI for bootstrap at 0.95
boot_ci_daily_n1 <- boot.ci(boot_results, conf = 0.95, type = "bca")
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

#the line over the histogram fits the normal distribution with mu = mean_dur_1
#and sigma = sd_dur_1