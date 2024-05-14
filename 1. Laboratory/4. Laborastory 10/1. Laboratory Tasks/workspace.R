#### Laboratory 10

### Exercises - Compulsory



## Exercise I.2

f <- function(x) -2*x^2 + 5*x - 2

a <- 0
b <- 2

n_points <- 10000

x_values <- seq(a, b, length.out = n_points)

y_values <- f(x_values)

area_estimate <- sum(diff(x_values) * (y_values[-1] + y_values[-n_points]) / 2)

exact_integral <- integrate(f, a, b)$value

relative_error <- abs(area_estimate - exact_integral) / exact_integral * 100

print(paste("Area Estimate:", area_estimate))
print(paste("Exact Area:", exact_integral))
print(paste("Relative Error (%):", relative_error))



## Exercise II.2

set.seed(42)

N <- 50000

lambda <- 3

samples <- rexp(N, rate = lambda)

integrand <- exp(-2 * samples^2)

integral_estimate <- mean(integrand)

exact_value <- sqrt(pi / 8)

relative_error <- abs(integral_estimate - exact_value) / exact_value * 100

print(paste("Estimated Integral Value:", integral_estimate))
print(paste("Exact Integral Value:", exact_value))
print(paste("Relative Error (%):", relative_error))



## Exercise III.2

set.seed(42)

customers_faster <- 3

customers_slower <- 1

lambda_faster <- 12  # Faster mechanic
lambda_slower <- 4   # Slower mechanic

prob_faster <- customers_faster / (customers_faster + customers_slower)

mechanic_choice <- runif(1)

if (mechanic_choice <= prob_faster)
{
  service_time <- rexp(1, rate = lambda_faster)
} else {
  service_time <- rexp(1, rate = lambda_slower)
}

expectation_estimate <- service_time

print(paste("Service Time (hours):", service_time))
print(paste("Estimated Expectation (hours):", expectation_estimate))



## Exercise IV.2

set.seed(42)

n_simulations <- 10000

n_computers <- 40

p_infection <- 0.2

n_infected_initial <- 1

k_values <- c(4, 6, 8, 10)

simulate_infection <- function(n_infected_initial, k)
{
  p_infect_uninfected <- p_infection * n_infected_initial / n_computers
  
  n_infected_uninfected <- rbinom(n = n_computers - n_infected_initial, size = 1, prob = p_infect_uninfected)
  
  n_infected <- n_infected_initial + sum(n_infected_uninfected)
  
  n_infected <- max(0, n_infected - min(n_infected, k))
  
  return(n_infected)
}

estimate_probabilities <- function(n_simulations, n_infected_initial, k)
{
  n_infected <- replicate(n_simulations, simulate_infection(n_infected_initial, k))
  
  prob_all_infected <- mean(n_infected == n_computers)
  
  prob_at_least_15_infected <- mean(n_infected >= 15)
  
  return(list(prob_all_infected = prob_all_infected, prob_at_least_15_infected = prob_at_least_15_infected))
}

results <- lapply(k_values, function(k) estimate_probabilities(n_simulations, n_infected_initial, k))

for (i in seq_along(k_values))
{
  cat("For k =", k_values[i], ":\n")
  cat("Probability that all computers are infected:", results[[i]]$prob_all_infected, "\n")
  cat("Probability that at least 15 computers are infected:", results[[i]]$prob_at_least_15_infected, "\n")
  cat("\n")
}



### Exercises - During Class

## Exercise ...