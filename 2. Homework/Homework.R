### Homework Assignments

### Name:    Roman Tudor
### Group:   X (E23)




## Homework - Part A - Task A1 - Subtask a) (1 point)


# Define the function.

computeProbabilities <- function (distribution, parameters, k, m)
{
  # Poisson Distribution
  if (distribution == "poisson")
    probabilities <- dpois(k:m, lambda = parameters)
  
  # Geometric Distribution
  else if (distribution == "geometric")
    probabilities <- dgeom(k:m, prob = parameters)
  
  # Binomial Distribution
  else
    probabilities <- dbinom(k:m, size = parameters$n, prob = parameters$p)
  
  return (probabilities)
}


# Define the parameters.

lambda <- 2
p <- 0.3
n <- 10
m <- 5
k <- 0


# Calculate the results.

poissonProbabilities <- computeProbabilities("poisson", lambda, k, m)
geometricProbabilities <- computeProbabilities("geometric", p, k, m)
binomialProbabilities <- computeProbabilities("binomial", list(n = n, p = p), k, m)


# Print the results.

print(poissonProbabilities)
print(geometricProbabilities)
print(binomialProbabilities)



## Homework - Part A - Task A1 - Subtask b) (1 point)


# Define the functions.

drawProbabilites <- function (probabilities, distribution)
{
  plot(probabilities, type = "h", lwd = 10,
       xlab = "k", ylab = "Probability",
       main = distribution)
}


drawAll <- function ()
{
  drawProbabilites(poissonProbabilities, "Poisson")
  drawProbabilites(geometricProbabilities, "Geometric")
  drawProbabilites(binomialProbabilities, "Binomial")
}


# Display the graphs.

drawAll()



## Homework - Part A - Task A1 - Subtask c) (0.5 points)


# Define the function.

findLeastValue <- function (lambda)
{
  result <- 0
  
  while (ppois(result, lambda) <= 1 - 1e-6)
    result <- result + 1
  
  return (result)
}


# Test the function.

lambda <- 2
result <- findLeastValue(lambda)
cat("\n\tResult: ", result, "\n\n")




## Homework - Part A - Task A2 - Subtask a) (1 point)


# Define the function.

calculateFrequenciesExpectations <- function (fileName)
{
  # Read the file.
  data <- read.csv(fileName)
  
  # Extract the samples.
  probabilitySample <- data$P
  statisticsSample <- data$S
  
  # Compute the absolute frequencies.
  absoluteFrequenciesProbability <- table(probabilitySample)
  absoluteFrequenciesStatistics <- table(statisticsSample)
  
  # Compute the relative frequencies.
  relativeFrequenciesProbability <- prop.table(absoluteFrequenciesProbability)
  relativeFrequenciesStatistics <- prop.table(absoluteFrequenciesStatistics)
  
  # Compute the expectations.
  expectationProbability <- sum(as.vector(absoluteFrequenciesProbability) *
                         as.numeric(names(absoluteFrequenciesProbability))) /
                      sum(as.vector(absoluteFrequenciesProbability))
  
  expectationStatistics <- sum(as.vector(absoluteFrequenciesStatistics) *
                         as.numeric(names(absoluteFrequenciesStatistics))) /
                      sum(as.vector(absoluteFrequenciesStatistics))
  
  # Return the results.
  return(list(absoluteFrequenciesProbability = absoluteFrequenciesProbability,
              absoluteFrequenciesStatistics = absoluteFrequenciesStatistics,
              relativeFrequenciesProbability = relativeFrequenciesProbability,
              relativeFrequenciesStatistics = relativeFrequenciesStatistics,
              expectationProbability = expectationProbability,
              expectationStatistics = expectationStatistics))
}


# Define the test function.

testCalculateFrequenciesExpectations <- function ()
{
  fileName <- "note_PS.csv"
  results <- calculateFrequenciesExpectations(fileName)
  
  cat("\n\tAbsolute Frequencies for Probability:\n\n",
      results$absoluteFrequenciesProbability,
      "\n\n\n\tAbsolute Frequencies for Statistics:\n\n",
      results$absoluteFrequenciesStatistics,
      "\n\n\n\tRelative Frequencies for Probability:\n\n",
      results$relativeFrequenciesProbability,
      "\n\n\n\tRelative Frequencies for Statistics:\n\n",
      results$relativeFrequenciesStatistics,
      "\n\n\n\tExpectation for Probability:\n\n",
      results$expectationProbability,
      "\n\n\n\tExpectation for Statistics:\n\n",
      results$expectationStatistics, "\n\n")
}


# Test the function.

testCalculateFrequenciesExpectations()



## Homework - Part A - Task A2 - Subtask b) (1.5 points)


# Define the function.

getOutliers <- function (fileName, sampleName)
{
  # Read the file.
  data <- read.csv(fileName)
  
  # Extract the sample.
  sample <- data[[sampleName]]
  
  # Calculate the outliers (using the interquartile range method).
  Q1 <- quantile(sample, 0.25)
  Q3 <- quantile(sample, 0.75)
  IQR <- Q3 - Q1
  lowerBound <- Q1 - 1.5 * IQR
  upperBound <- Q3 + 1.5 * IQR
  
  # Remove the outliers.
  trimmedSample <- sample[sample >= lowerBound & sample <= upperBound]
  
  # Plot frequency distributions
  breaks <- seq(0, 10, 1)
  sampleName <- ifelse(sampleName == "S", "Statistics", "Probability")
  
  hist(trimmedSample, breaks = breaks,
       main = paste("Frequency Distribution of", sampleName),
       xlab = sampleName, ylab = "Frequency")
  
  # Return the trimmed sample.
  return (trimmedSample)
}


# Define the test function.

testGetOutliers <- function ()
{
  trimmedSampleProbability <- getOutliers(fileName, "P")
  cat("\n\tThe Trimmed Sample for Probability:\n\n",
      trimmedSampleProbability)
  
  trimmedSampleStatistics <- getOutliers(fileName, "S")
  cat("\n\n\n\tThe Trimmed Sample for Statistics:\n\n",
      trimmedSampleProbability, "\n\n")
}


# Test the function.

testGetOutliers()




## Homework - Part B - Task B3 - Subtask a) (1 point)


# Define the function.

estimateAndCompare <- function (functionName)
{
  # Estimate the value.
  estimatedValue <- integrate(functionName, lower = -1, upper = 1)$value
  
  
  # Calculate the exact value.
  exactValue <- log(3) - log(2)
  
  
  # Compare the results.
  difference <- abs(estimatedValue - exactValue)
  
  # Print the results.
  cat("\n\tEstimated Value:\n\n", estimatedValue,
      "\n\n\n\tExact Value:\n\n", exactValue,
      "\n\n\n\tDifference:\n\n", difference, "\n\n")
}


# Define the integral function.

integralFunction <- function (x)
{
  (2*x - 1) / (x^2 - x - 6)
}


# Define the test function.

testEstimateAndCompare <- function ()
{
  estimateAndCompare(integralFunction)
}


# Test the function.

testEstimateAndCompare()



## Homework - Part B - Task B3 - Subtask b) (1 point)


# Define the function.

estimateAndCompare <- function (functionName)
{
  # Estimate the value.
  estimatedValue <- integrate(functionName, lower = 3, upper = 11)$value
  
  # Calculate the exact value.
  exactValue <- 61.2
  
  # Compare the results.
  difference <- abs(estimatedValue - exactValue)
  
  # Print the results.
  cat("\n\tEstimated Value:\n\n", estimatedValue,
      "\n\n\n\tExact Value:\n\n", exactValue,
      "\n\n\n\tDifference:\n\n", difference, "\n\n")
}


# Define the integral function.

integralFunction <- function (x)
{
  (x + 4) / (abs(x - 3) ^ (1 / 3))
}


# Define the test function.

testEstimateAndCompare <- function ()
{
  estimateAndCompare(integralFunction)
}


# Test the function.

testEstimateAndCompare()



## Homework - Part B - Task B3 - Subtask c)


# Define the function.

estimateAndCompare <- function (functionName)
{
  # Estimate the value.
  estimatedValue <- integrate(functionName, lower = 0, upper = Inf)$value
  
  # Calculate the exact value.
  exactValue <- 1 / 2
  
  # Compare the results.
  difference <- abs(estimatedValue - exactValue)
  
  # Print the results.
  cat("\n\tEstimated Value:\n\n", estimatedValue,
      "\n\n\n\tExact Value:\n\n", exactValue,
      "\n\n\n\tDifference:\n\n", difference, "\n\n")
}


# Define the integral function.

integralFunction <- function (x)
{
  x * exp(-x ^ 2)
}


# Define the test function.

testEstimateAndCompare <- function ()
{
  estimateAndCompare(integralFunction)
}


# Test the function.

testEstimateAndCompare()



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




## Homework - Part D - Task D1 (1.5 points)


# Define the function.

getConfidenceIntervals <- function ()
{
  # Read the file.
  data <- read.csv("probabilitati.csv")
  
  # Extract the scores.
  scores <- data$probabilitati
  
  # Calculate the sample mean.
  sampleMean <- mean(scores)
  
  # Calculate the sample size.
  sampleSize <- length(scores)
  
  # Calculate the variance.
  variance <- 92.16
  
  # Calculate the standard deviation.
  standardDeviation <- sqrt(variance)
  
  # Calculate the standard error.
  standardError <- standardDeviation / sqrt(sampleSize)
  
  # Calculate the 95% confidence interval.
  confidenceLevel <- 0.95
  alpha <- 1 - confidenceLevel
  zScore <- qnorm(1 - alpha / 2)
  marginOfError <- zScore * standard_error
  lowerBound95 <- sampleMean - marginOfError
  upperBound95 <- sampleMean + marginOfError
  
  # Calculate the 99% confidence interval.
  confidenceLevel <- 0.99
  alpha <- 1 - confidenceLevel
  zScore <- qnorm(1 - alpha / 2)
  marginOfError <- zScore * standard_error
  lowerBound99 <- sampleMean - marginOfError
  upperBound99 <- sampleMean + marginOfError
  
  # Print the results
  cat("\n\n\t95% Confidence Interval:\n\n", lower_bound_95, "->",
      upper_bound_95,
      "\n\n\n\t99% Confidence Interval:\n\n", lower_bound_99, "->",
      upper_bound_99, "\n\n")
}


# Test the function.

getConfidenceIntervals()



## Homework - Part D - Task D2 (1.5 points)


# Define the function.

getConfidenceIntervals <- function ()
{
  # Read the file.
  data <- read.csv("statistica.csv")
  
  # Extract the scores.
  scores <- data$statistica
  
  # Calculate the sample mean.
  sampleMean <- mean(scores)
  
  # Calculate the sample size.
  sampleSize <- length(scores)
  
  # Calculate the standard deviation.
  standardDeviation <- sd(scores)
  
  # Calculate the standard error.
  standardError <- standardDeviation / sqrt(sampleSize)
  
  # Calculate the 95% confidence interval.
  confidenceLevel <- 0.95
  alpha <- 1 - confidenceLevel
  zScore <- qnorm(1 - alpha / 2)
  marginOfError <- zScore * standard_error
  lowerBound95 <- sampleMean - marginOfError
  upperBound95 <- sampleMean + marginOfError
  
  # Calculate the 99% confidence interval.
  confidenceLevel <- 0.99
  alpha <- 1 - confidenceLevel
  zScore <- qnorm(1 - alpha / 2)
  marginOfError <- zScore * standard_error
  lowerBound99 <- sampleMean - marginOfError
  upperBound99 <- sampleMean + marginOfError
  
  # Print the results
  cat("\n\n\t95% Confidence Interval:\n\n", lower_bound_95, "->",
      upper_bound_95,
      "\n\n\n\t99% Confidence Interval:\n\n", lower_bound_99, "->",
      upper_bound_99, "\n\n")
}


# Test the function.

getConfidenceIntervals()