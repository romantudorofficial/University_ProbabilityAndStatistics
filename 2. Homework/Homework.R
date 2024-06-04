### Homework Assignments

### Name:    Roman Tudor
### Group:   X (E23)




## Homework - Part A - Task A1 - Subtask a) (1 point)


# Define the function.

computeProbabilities <- function (lambda, p, n, m, k)
{
  # Poisson Distribution
  poissonProbabilities <- dpois(k:m, lambda)
  
  # Geometric Distribution
  geometricProbabilities <- dgeom(k:m, p)
  
  # Binomial Distribution
  binomialProbabilities <- dbinom(k:m, n, p)
  
  # Return the results.
  return(list(poissonProbabilities = poissonProbabilities,
              geometricProbabilities = geometricProbabilities,
              binomialProbabilities = binomialProbabilities))
}


# Define the test function.

testComputeProbabilities <- function ()
{
  # Define the parameters.
  lambda <- 2
  p <- 0.3
  n <- 10
  m <- 5
  k <- 0
  
  # Calculate the results.
  poissonProbabilities <- computeProbabilities(lambda, p, n, m, k) $ poissonProbabilities
  geometricProbabilities <- computeProbabilities(lambda, p, n, m, k) $ geometricProbabilities
  binomialProbabilities <- computeProbabilities(lambda, p, n, m, k) $ binomialProbabilities
  
  # Print the results.
  cat("\n\tPoisson Probabilities:\n\n", poissonProbabilities,
      "\n\n\n\tGeometric Probabilities:\n\n", geometricProbabilities,
      "\n\n\n\tBinomial Probabilities:\n\n", binomialProbabilities, "\n\n")
}


# Test the function.

testComputeProbabilities()



## Homework - Part A - Task A1 - Subtask b) (1 point)


# Define the functions.

drawProbabilites <- function (lambda, p, n, m, k)
{
  # Calculate the probabilities.
  probabilities <- computeProbabilities(lambda, p, n, m, k)
  
  # Poisson Distribution
  plot(probabilities $ poissonProbabilities, type = "h", lwd = 10,
       xlab = "k", ylab = "Probability", main = "Poisson")
  
  # Geometric Distribution
  plot(probabilities $ geometricProbabilities, type = "h", lwd = 10,
       xlab = "k", ylab = "Probability", main = "Geometric")
  
  # Binomial Distribution
  plot(probabilities $ binomialProbabilities, type = "h", lwd = 10,
       xlab = "k", ylab = "Probability", main = "Binomial")
}


# Define the test function.

testDrawProbabilites <- function ()
{
  # Define the parameters.
  lambda <- 2
  p <- 0.3
  n <- 10
  m <- 5
  k <- 0
  
  # Get the results.
  drawProbabilites(lambda, p, n, m, k)
}


# Test the function.

testDrawProbabilites()



## Homework - Part A - Task A1 - Subtask c) (0.5 points)


# Define the function.

findLeastValue <- function (lambda)
{
  result <- 0
  
  while (ppois(result, lambda) <= 1 - 1e-6)
    result <- result + 1
  
  return (result)
}


# Define the test function.

testFindLeastValue <- function ()
{
  lambda <- 2
  result <- findLeastValue(lambda)
  cat("\n\tResult:\n\n\t\t", result, "\n\n")
}


# Test the function.

testFindLeastValue()




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
  
  # Plot the frequency distributions.
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
  # Get the results.
  trimmedSampleProbability <- getOutliers("note_PS.csv", "P")
  trimmedSampleStatistics <- getOutliers("note_PS.csv", "S")
  
  # Display the results.
  cat("\n\tThe Trimmed Sample for Probability:\n\n",
      trimmedSampleProbability,
      "\n\n\n\tThe Trimmed Sample for Statistics:\n\n",
      trimmedSampleStatistics, "\n\n")
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



## Homework - Part B - Task B4 - Subtask a) (2 points)


# Define the function.

estimateYears <- function (initialUsers, n, p, q, targetUsers, numberOfSimulations)
{
  simulateGrowth <- function ()
  {
    currentUsers <- initialUsers
    years <- 0
    
    while (currentUsers < targetUsers)
    {
      newUsers <- rbinom(1, n, p)
      leavingUsers <- rbinom(1, currentUsers, q)
      currentUsers <- currentUsers + newUsers - leavingUsers
      years <- years + 1
    }
    
    return (years)
  }
  
  yearsUntilTarget <- replicate(numberOfSimulations, simulateGrowth())
  
  return(mean(yearsUntilTarget))
}


# Define the test function.

testEstimateYears <- function ()
{
  initialUsers <- 10000
  n <- 1000
  p <- 0.25
  q <- 0.01
  targetUsers <- 15000
  numberOfSimulations <- 1000
  
  result <- estimateYears(initialUsers, n, p, q, targetUsers, numberOfSimulations)
  
  cat("\n\tResult:\n\n\t\t", result, "\n\n")
}


# Test the function.

testEstimateYears()



## Homework - Part B - Task B4 - Subtask b) (1 point)


# define the function.

estimateProbability <- function (initialUsers, n, p, q, years, targetUsers, numberOfSimulations)
{
  simulateGrowth <- function ()
  {
    currentUsers <- initialUsers
    months <- years * 12
    
    for (i in 1 : months)
    {
      newUsers <- rbinom(1, round(n / 12), p)
      leavingUsers <- rbinom(1, currentUsers, q / 12)
      currentUsers <- currentUsers + newUsers - leavingUsers
    }
    
    return (currentUsers)
  }
  
  currentUsers <- replicate(numberOfSimulations, simulateGrowth())
  
  return(mean(currentUsers >= targetUsers))
}


# Define the test function.

testEstimateProbability <- function ()
{
  initialUsers <- 10000
  n <- 1000
  p <- 0.25
  q <- 0.01
  years <- 40 + 10 / 12
  targetUsers <- 15000
  numberOfSimulations <- 1000
  
  result <- estimateProbability(initialUsers, n, p, q, years, targetUsers, numberOfSimulations)
  
  cat("\n\tResult:\n\n\t\t", result, "\n\n")
}


# Test the function.

testEstimateProbability()




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




## Homework - Part D - Task D3 (2 points)


# Define the function.

getChangeConclusion <- function ()
{
  # Get the given data.
  proportionUnableBefore <- 0.15
  sampleSize <- 100
  studentsUnableAfter <- 14
  
  # Calculate the after proportion.
  proportionUnableAfter <- studentsUnableAfter / sampleSize
  
  # Calculate the standard error.
  standardError <- sqrt((proportionUnableBefore * (1 - proportionUnableBefore))
                        / sampleSize)
  
  # Calculate the test static.
  testStatic <- (proportionUnableBefore - proportionUnableAfter) / standardError
  
  # Calculate the critical values.
  criticalValue1 <- qnorm(0.99)
  criticalValue5 <- qnorm(0.95)
  
  # Establish the conclusions.
  if (testStatic > criticalValue1)
    change1 = "effective"
  else
    change1 = "not effective"
  
  if (testStatic > criticalValue5)
    change5 = "effective"
  else
    change5 = "not effective"
  
  # Print the results.
  cat("\n\tAt 1% significance level, the change was:\n\n\t\t", change1,
      "\n\n\n\tAt 5% significance level, the change was:\n\n\t\t", change5,
      "\n\n")
}


# Test the function.

getChangeConclusion()