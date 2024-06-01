### Homework Assignments

### Name:    Roman Tudor
### Group:   X (E23)




## Homework - Part A - Task A1 - Subtask a) (1 point)


# Define the function.

compute_probabilities <- function (distribution, params, k, m)
{
  # Poisson Distribution
  if (distribution == "poisson")
    probabilities <- dpois(k:m, lambda = params)
  
  # Geometric Distribution
  else if (distribution == "geometric")
    probabilities <- dgeom(k:m, prob = params)
  
  # Binomial Distribution
  else if (distribution == "binomial")
    probabilities <- dbinom(k:m, size = params$n, prob = params$p)
  
  return (probabilities)
}


# Define the parameters.

lambda <- 2
p <- 0.3
n <- 10
m <- 5
k <- 0


# Get the results.

prob_poisson <- compute_probabilities("poisson", lambda, k, m)
prob_geometric <- compute_probabilities("geometric", p, k, m)
prob_binomial <- compute_probabilities("binomial", list(n = n, p = p), k, m)



## Homework - Part A - Task A1 - Subtask b) (1 point)


# Define the function.

plot_probabilities <- function (probabilities)
{
  plot(probabilities, type = "h", lwd = 2,
       xlab = "k", ylab = "Probability",
       main = "Probability Distribution")
}


# Display the graphs.

plot_probabilities(prob_poisson)
plot_probabilities(prob_geometric)
plot_probabilities(prob_binomial)



## Homework - Part A - Task A1 - Subtask c) (0.5 points)


# Define the function.

find_k0_poisson <- function (lambda)
{
  k0 <- 0
  
  while (ppois(k0, lambda) <= 1 - 1e-6)
    k0 <- k0 + 1
  
  return(k0)
}


# Test the function.

k0 <- find_k0_poisson(lambda)
cat("Least value of k0 for Poisson(λ) such that P(Y ⩽ k0) > 1 - 1e-6:", k0, "\n")




## Homework - Part A - Task A2 - Subtask a)


# Define the function.

calculate_frequencies_and_expectations <- function (file_path)
{
  # Read data from the file
  data <- read.csv(file_path)
  
  # Extract samples
  sample_P <- data$P
  sample_S <- data$S
  
  # Compute absolute frequencies
  abs_freq_P <- table(sample_P)
  abs_freq_S <- table(sample_S)
  
  # Compute relative frequencies
  rel_freq_P <- prop.table(abs_freq_P)
  rel_freq_S <- prop.table(abs_freq_S)
  
  # Find expectations
  expectation_P <- sum(as.vector(abs_freq_P) *
                         as.numeric(names(abs_freq_P))) /
                      sum(as.vector(abs_freq_P))
  
  expectation_S <- sum(as.vector(abs_freq_S) *
                         as.numeric(names(abs_freq_S))) /
                      sum(as.vector(abs_freq_S))
  
  # Return results
  return(list(abs_freq_P = abs_freq_P,
              abs_freq_S = abs_freq_S,
              rel_freq_P = rel_freq_P,
              rel_freq_S = rel_freq_S,
              expectation_P = expectation_P,
              expectation_S = expectation_S))
}


# Test the function.

file_path <- "note_PS.csv"
results <- calculate_frequencies_and_expectations(file_path)
print("Absolute Frequencies for Sample P:")
print(results$abs_freq_P)
print("Relative Frequencies for Sample P:")
print(results$rel_freq_P)
print("Expectation for Sample P:")
print(results$expectation_P)



## Homework - Part A - Task A2 - Subtask b)


# Define the function.

remove_outliers_and_plot <- function (file_path, sample_name)
{
  # Read data from the file
  data <- read.csv(file_path)
  
  # Extract the specified sample
  sample <- data[[sample_name]]
  
  # Determine outliers using the interquartile range method
  Q1 <- quantile(sample, 0.25)
  Q3 <- quantile(sample, 0.75)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  # Remove outliers
  trimmed_sample <- sample[sample >= lower_bound & sample <= upper_bound]
  
  # Plot frequency distributions
  breaks <- seq(0, 10, 1)
  hist(trimmed_sample, breaks = breaks,
       main = paste("Frequency Distribution of", sample_name),
       xlab = sample_name, ylab = "Frequency")
  
  # Return the trimmed sample
  return(trimmed_sample)
}


# Test the function.

trimmed_sample_P <- remove_outliers_and_plot(file_path, "P")
trimmed_sample_S <- remove_outliers_and_plot(file_path, "S")




## Homework - Part B - Task B3 - Subtask a)


# Define the function.

f <- function (x)
{
  (2*x - 1) / (x^2 - x - 6)
}


# Estimate the integral.

integral <- integrate(f, lower = -1, upper = 1)$value


# Calculate the exact value.

exact_value <- log(3) - log(2)


# Compare the results.

difference <- abs(integral - exact_value)


# Print the results.

print(paste("Estimate value:", integral))
print(paste("Exact value:", exact_value))
print(paste("Difference:", difference))



## Homework - Part B - Task B3 - Subtask b)


# Define the function.

f <- function (x)
{
  (x + 4) / (abs(x - 3) ^ (1 / 3))
}


# Estimate the integral.

integral <- integrate(f, lower = 3, upper = 11)$value


# Get the exact value.

exact_value <- 61.2


# Compare the results.

difference <- abs(integral - exact_value)


# Print the results.

print(paste("Estimate value:", integral))
print(paste("Exact value:", exact_value))
print(paste("Difference:", difference))



## Homework - Part B - Task B3 - Subtask c)


# Define the function.

f <- function (x)
{
  x * exp(-x^2)
}


# Estimate the integral.
integral <- integrate(f, lower = 0, upper = Inf)$value


# Get the exact value.

exact_value <- 1 / 2


# Compare the results.

difference <- abs(integral - exact_value)


# Print the results.

print(paste("Estimate value:", integral))
print(paste("Exact value:", exact_value))
print(paste("Difference:", difference))



## Homework - Part B - Task B4 - Subtask a)


# Define the parameters.

n <- 1000  # Number of trials (users joining)
p <- 0.25  # Probability of success (new user joining)
q <- 0.01  # Probability of user leaving


# Number of simulations

num_simulations <- 1000


# Simulate the growth of the iSocialize network

simulate_growth <- function()
{
  users <- 10000  # Initial number of users
  years <- 0  # Initial number of years
  
  # Simulate growth until the network reaches at least 15000 users
  while (users < 15000)
  {
    # Simulate new users joining
    new_users <- rbinom(1, n, p)
    # Simulate users leaving
    users_left <- sum(rbinom(users, 1, q))
    # Update the total number of users
    users <- users + new_users - users_left
    # Increment the number of years
    years <- years + 1
  }
  
  return(years)
}


# Perform Monte Carlo simulations to estimate average number of years

years_until_15000 <- replicate(num_simulations, simulate_growth())
average_years <- mean(years_until_15000)


# Print the estimated average number of years

print(paste("Estimated average number of years until iSocialize reaches at least 15000 users:", average_years))


# Simulate the growth after 40 years and 10 months

years <- 40 + 10/12
users <- 10000
for (i in 1:years)
{
  new_users <- rbinom(1, n, p)
  users_left <- sum(rbinom(users, 1, q))
  users <- users + new_users - users_left
}


# Check if the network has at least 15000 users

prob_15000 <- ifelse(users >= 15000, 1, 0)


# Print the estimated probability

print(paste("Estimated probability that the network will have at least 15000 users after 40 years and 10 months:", prob_15000))
