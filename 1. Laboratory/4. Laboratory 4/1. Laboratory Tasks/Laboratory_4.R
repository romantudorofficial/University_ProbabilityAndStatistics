#### Laboratory 4



### Exercises - Compulsory



## Exercise I.2

f <- function (x) -2*x^2 + 5*x - 2

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
print(paste("Relative Error:", relative_error))



## Exercise II.1.b)

f <- function (x)
{
  return(exp(x))
}

exact_value <- 51.87987

N <- 100000

samples <- runif(N, 1, 4)
estimated_value <- mean(f(samples)) * (4 - 1)

absolute_error <- abs(estimated_value - exact_value)
relative_error <- absolute_error / abs(exact_value)

cat("Estimated Value: ", estimated_value, "\n")
cat("Exact Value: ", exact_value, "\n")
cat("Absolute Error: ", absolute_error, "\n")
cat("Relative Error: ", relative_error, "\n")



## Exercise II.1.d)

f <- function (x)
{
  return(1 / (4 * x^2 - 1))
}

exact_value <- log(3/4)

N <- 100000
B <- 1000

samples <- runif(N, 1, B)
integral_estimate <- mean(f(samples)) * (B - 1)

integral <- integrate(f, 1, Inf)$value
total_estimate <- integral_estimate + integral

absolute_error <- abs(total_estimate - exact_value)
relative_error <- absolute_error / abs(exact_value)

cat("Estimated Value: ", total_estimate, "\n")
cat("Exact Value: ", exact_value, "\n")
cat("Absolute Error: ", absolute_error, "\n")
cat("Relative Error: ", relative_error, "\n")



## Exercise II.2


# The improved MC integration function.

MC_improved_integration <- function (N)
{
  sum = 0
  
  for (i in 1:N)
  {
    u = rexp(1, 1)
    sum = sum + exp(-u*u) / exp(-u)
  }
  
  return (sum / N)
}


N = 50000 # number of samples


estimated_value = MC_improved_integration(N)
cat("Estimated value: ", estimated_value, "\n")

exact_value = sqrt(pi / 8)
cat("Exact value:", exact_value, "\n")


absolute_error = abs(estimated_value - exact_value)
cat("Absolute error:", absolute_error, "\n")


relative_error = absolute_error / exact_value
cat("Relative error:", relative_error, "\n")



## Exercise III.2


N <- 10000 # number of samples


lambda1 <- 4
lambda2 <- 12


p1 <- 1/4  # Prob. of being served by the first mechanic
p2 <- 3/4  # Prob. of being served by the second mechanic


mechanic_selection <- sample(c(1, 2), N, replace = TRUE, prob = c(p1, p2))
service_times <- ifelse(mechanic_selection == 1, rexp(N, lambda1), rexp(N, lambda2))


expected_service_time <- mean(service_times)
cat("Estimated Service Time: ", expected_service_time, "\n")



## Exercise IV.2

exercise42 = function ()
{
  
}


exercise42()



### Exercises - During Class



## Exercise Solved Exercise

disc_area = function(N)
{
  N_C = 0;
  for (i in 1:N)
  {
    x = runif(1, -1, 1);
    y = runif(1, -1, 1);
    if (x * x + y * y <= 1)
      N_C = N_C + 1;
  }
  estimate -4 * N_C / N;
  print(pi);
  print(abs(pi-estimate));
  print(abs(pi-estimate)/abs(pi));
  return (estimate);
}



# Exercise I.1

sphere_volume = function(N)
{
  N_C = 0;
  for (i in 1:N)
  {
    x = runif(1, -1, 1);
    y = runif(1, -1, 1);
    z = runif(1, -1, 1);
    if (x * x + y * y + z * z <= 1)
      N_C = N_C + 1;
  }
  estimate -4 * N_C / N;
  print(4*pi/3);
  print(abs(4*pi/3-estimate));
  print(abs(4*pi/3-estimate)/abs(4*pi/3));
  return (estimate);
}



# Exercise II.2

area_under_parabola = function(N)
{
  N_C = 0;
  for (i in 1:N)
  {
    x = runif(1, 0.5, 1);
    y = runif(1, 0, 9/8);
    z = runif(1, -1, 1);
    if (y <= -2*x^2+5*x -2)
      N_C = N_C + 1;
  }
  estimate -4 * N_C / N;
  print(4*pi/3);
  print(abs(4*pi/3-estimate));
  print(abs(4*pi/3-estimate)/abs(4*pi/3));
  return (estimate);
} # to be completed



# Exercise 

MC_integration = function(N)
{
  sum = 0;
  for (i in 1:N)
  {
    u = runif(1,0,10);
    sum = sum + exp(-u*u/2);
  }
  return (10 *sum/N);
}

MC_integration_average = function(k, N, a)
{
  estimates = vector();
  for (i in 1:k)
    estimates[i] = MC_integration(N, a);
  print(mean(estimates));
  print(sd(estimates));
}

MC_improved_integration = function(N, lambda) 
{
  sum = 0;
  for(i in 1:N)
  {
    u = rexp(1, lambda);
    sum = sum + exp(-u*u)/(lambda*exp(-lambda*u));
  }
  return(sum/N);
}

MC_imprvd_integr_average = function(k, N, lambda)
{
  estimates = 0;
  for(i in 1:k)
    estimates[i] = MC_improved_integration(N, lambda);
  print(sqrt(pi)/2)
  print(mean(estimates));
  print(sd(estimates));
}



# Exercise II.1.a)

ex_II_1_a = function(N)
{
  sum = 0;
  for (i in 1:N)
  {
    u = runif(1,0,pi);
    sum = sum + sin(u) * sin(u);
  }
  return (pi *sum/N);
}

MC_integration_average = function(k, N)
{
  estimates = vector();
  for (i in 1:k)
    estimates[i] = ex_II_1_a(N);
  print(pi/2);
  print(mean(estimates));
  print(sd(estimates));
}



# Ex II.1.c)

ex_II_1_c = function(N, a)
{
  sum = 0;
  for (i in 1:N)
  {
    u = runif(1,0,a);
    sum = sum + 1/sqrt(1-x^2);
  }
  return (a * sum/N);
}

MC_integration_average = function(k, N, a)
{
  estimates = vector();
  for (i in 1:k)
    estimates[i] = ex_II_1_c(N, a);
  print(pi/2);
  print(mean(estimates));
  print(sd(estimates));
}



# Ex II.1.b is similar to a... 1-4, compute mean, multiply by 3

# d) -> ...

# II.2 - use improved MC



# ...

Nr_days = function()
{
  nr_days = 1;
  last_errors = c(27, 31);
  nr_errors = 27;
  while(nr_errors > 0)
  {
    lambda = min(last_errors);
    nr_errors = rpois(1, lambda);
    last_errors = c(nr_errors, last_errors[1]);
    nr_days = nr_days + 1;
  }
  return(nr_days);
}

MC_nr_days = function(N)
{
  s = 0;
  for(i in 1:N)
    s = s + Nr_days();
  return(s/N);
}



# Exercise III.1

ex_III_1 = function()
{
  nr_days = 1;
  last_errors = c(13, 15, 9);
  nr_errors = 27;
  while(nr_errors > 0)
  {
    lambda = mean(last_errors);
    nr_errors = rpois(1, lambda);
    last_errors = c(nr_errors, last_errors[1], last_errors[2]);
    nr_days = nr_days + 1;
  }
  return(nr_days);
}

ex_III_1_nr_days = function(N)
{
  s = 0;
  for(i in 1:N)
    s = s +  ex_III_1();
  return(s/N);
}



# ...

Nr_days_a = function()
{
  nr_days = 2;
  last_errors = c(18, 22, 28);
  nr_errors = 18;
  while(nr_errors > 0)
  {
    lambda = min(last_errors);
    nr_errors = rpois(1, lambda);
    last_errors = c(nr_errors, last_errors[1:2]);
    nr_days = nr_days + 1;
  }
  return(nr_days);
}

MC_nr_days_21 = function(N)
{
  s = 0;
  for(i in 1:N)
  {
    if(Nr_days_a() > 21)
      s = s+1;
  }
}



# ...

size_sample = function (epsilon, prob)
{
  alfa = 1 - prob;
  z = qnorm(1 - alfa/2);
  p_star = MC_nr_days_21(1000);
  N = p_star*(1 - p_star) * (z/epsilon)^2;
}

epsilon = 0.01;
prob = 0.99;
MC_nr_days_21(size_sample(epsilon, prob))



# present blue exercises next time