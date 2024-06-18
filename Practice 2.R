# Practice 2


# Question 1

# Read CSV file
speed <- read.csv('D:/UMKC/Spring 2024/CS5590/Assignment 2/speed.data.csv')

View(speed) #data frame

# Generate box plot
boxplot(speed, 
        main = "Vehicular Speeds Before and After",
        names = c("Speed Before", "Speed After"),
        col = c("skyblue", "lightgreen"),
        ylab = "Speed (mph)")

# Question 2

# Calculate the variance
var.sbf = var(speed$speed_before)

# Get the sample size
n = length(speed$speed_before)

# Confidence level
conf.level = 0.95 

# Significance level
alpha = 1 - conf.level 

# Chi-squared values for upper and lower confidence limits
chi.1 = qchisq(1 - alpha/2, df=n-1)
chi.2 = qchisq(alpha/2, df=n-1)

# Calculate confidence interval for the variance of before-speed data.
var.interval = c((n-1)*var.sbf/chi.1, (n-1)*var.sbf/chi.2)

# Print results
cat("95% Confidence Interval for for the variance of before-speed data:", var.interval[1], "-", var.interval[2], "\n")

# Question 3

# Remove NA values from speed_after
speed_after <- speed$speed_after
speed_after_clean <- speed_after[!is.na(speed_after)]

# Calculate sample mean and sample standard deviation
sample_mean <- mean(speed_after_clean)
sample_sd <- sd(speed_after_clean)

# Confidence level
conf_level <- 0.90

# Get the sample size
length_speed_after = length(speed$speed_after)

# Calculate standard error of the mean
standard_error <- sample_sd / sqrt(length_speed_after)

# Find t-value for given confidence level and degrees of freedom
t_value <- qt((1 + conf_level) / 2, df = length_speed_after  - 1)

# Calculate margin of error
margin_of_error <- t_value * standard_error

# Calculate confidence interval
confidence_interval <- c(sample_mean - margin_of_error, sample_mean + margin_of_error)

# Print results
cat("90% Confidence Interval for Mean Vehicular After-Speed Data:", confidence_interval[1], "-", confidence_interval[2], "\n")

# Problem 4

# Set the hypothesized mean
test_mean <- 65

# Perform one-sample t-test
test_result <- t.test(speed_after_clean, mu = test_mean, alternative = "greater", conf.level = 0.99)

# Get the p-value
p_value <- test_result$p.value

# Set significance level
alpha <- 0.01

# Print the test result
cat("p-value:", p_value, "\n")

# Check if the p-value is less than alpha
if (p_value < alpha) {
  cat("The mean speed is greater than 65 mph at the 1% significance level.\n")
} else {
  cat("There is not enough evidence to conclude that the mean speed is greater than 65 mph at the 1% significance level.\n")
}

# Question 5

# Variance to test
var_to_test <- 20

# Degrees of freedom for the numerator
df_numerator <- length_speed_after - 1

# Degrees of freedom for the denominator (fixed at 1 for testing variance)
df_denominator <- 1

# Calculate F statistic
F_statistic <- var(speed_after_clean) / var_to_test

# Calculate p-value
p_value <- pf(F_statistic, df_numerator, df_denominator, lower.tail = FALSE)

# Print the test result
cat("p-value:", p_value, "\n")

# Set significance level
alpha_0 <- 0.05


# Check if the p-value is less than alpha
if (p_value < alpha_0) {
  cat("The variance of the after-speed data is less than 20 mph^2 at the 5% significance level.\n")
} else {
  cat("There is not enough evidence to conclude that the variance of the after-speed data is less than 20 mph^2 at the 5% significance level.\n")
}

# Question 6 

# Perform an F-test for equality of variances
alpha_1 <- 0.10
test_result <- var.test(speed_after_clean,speed$speed_before, conf.level = 1 - alpha_1)
print(test_result)

# Question 7

alpha_2 <- 0.01
# Perform the one-tailed t-test
test_result <- t.test(speed_after_clean,speed$speed_before, alternative = "greater", conf.level = 0.99)
print(test_result)

# Question 8

# Perform the Mann-Whitney-Wilcoxon test
test_result <- wilcox.test(speed$speed_before, speed_after_clean)

# Print the results
print(test_result)

# Draw density plots
par(mfrow=c(1,2))  # Set the layout to have two plots side by side
plot(density(speed$speed_before), main="Density Plot of Speed Before", xlab="Speed", ylab="Density", col="blue")
plot(density(speed_after_clean), main="Density Plot of Speed After", xlab="Speed", ylab="Density", col="orange")

