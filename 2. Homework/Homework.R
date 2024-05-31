### Homework Assignments

### Name:    Roman Tudor
### Group:   X (E23)




## Homework - Part A - Task A1 - Subtask a)


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



## Homework - Part A - Task A1 - Subtask b)


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



## Homework - Part A - Task A1 - Subtask c)


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