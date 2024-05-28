#### Laboratory 3



### Exercises - Compulsory



## Exercise II.2

LLN_Student <- function (n_values, degrees_freedom)
{
  sample_means <- numeric(length(n_values))
  
  for (i in 1:length(n_values))
  {
    n <- n_values[i]
    samples <- rt(n, degrees_freedom)

    sample_mean <- mean(samples)
    sample_means[i] <- sample_mean
  }
  
  return(sample_means)
}

n_values <- c(1000, 10000, 100000, 1000000)
degrees_freedom <- c(2, 3, 4, 5)
results <- sapply(degrees_freedom, function(df) LLN_Student(n_values, df))

print(results)



## Exercise III.2

CLT_Gamma <- function(N, n, alpha, lambda, z)
{

  sample_means <- numeric(N)
  
  for (i in 1:N)
  {

    samples <- rgamma(n, shape = alpha, rate = lambda)
    sample_mean <- mean(samples)
    sample_means[i] <- sample_mean
  }
  
  mean_sample_means <- mean(sample_means)
  sd_sample_means <- sd(sample_means)
  z_score <- (mean_sample_means - (alpha / lambda)) / (sd_sample_means / sqrt(n))
  
  return(z_score)
}

N_values <- c(5000, 10000, 20000)
n <- 50
alpha <- 2
lambda <- 1
z_values <- c(-1.5, 0, 1.5)

results <- sapply(N_values, function(N) sapply(z_values, function(z) CLT_Gamma(N, n, alpha, lambda, z)))

print(results)



## Exercise IV.2

P_X_greater_than_k <- function(n, p, k)
{
  if (k < 0 || k >= n)
  {
    stop("k must be between 0 and n-1.")
  }

  p_x_less_than_or_equal_to_k <- pbinom(k, size = n, prob = p)
  p_x_greater_than_k <- 1 - p_x_less_than_or_equal_to_k
  
  return(p_x_greater_than_k)
}

n <- 10
p <- 0.5
k <- 3
result <- P_X_greater_than_k(n, p, k)
print(result)