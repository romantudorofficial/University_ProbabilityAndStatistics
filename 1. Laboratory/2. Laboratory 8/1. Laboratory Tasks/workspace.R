#### Laboratory 8

### Exercises



## Exercise I.3


# Read the data from the CSV file.

life_expectancy <- read.csv("life_expect.csv");


# Plot histograms for males and females

par(mfrow = c(2, 1)); # 2 rows, 1 column


# Create the histogram for males.

hist(life_expectancy$male, breaks = 7, main = "Male Life Expectancy", 
     xlab = "Life Expectancy", ylab = "Frequency", col = "skyblue");


# Create the histogram for females.

hist(life_expectancy$female, breaks = 7, main = "Female Life Expectancy", 
     xlab = "Life Expectancy", ylab = "Frequency", col = "pink");



## Exercise III.3

sample_data <- read.table("sample2.txt");


outliers_mean <- function (sample)
{
  mean_val <- mean(sample)
  sd_val <- sd(sample)
  
  lower_threshold <- mean_val - 2 * sd_val
  upper_threshold <- mean_val + 2 * sd_val
  
  outliers <- sample[sample < lower_threshold | sample > upper_threshold]
  
  return (outliers)
}


outliers_iqr <- function (sample)
{
  q1 <- quantile(sample, 0.25)
  q3 <- quantile(sample, 0.75)
  iqr <- q3 - q1
  
  lower_threshold <- q1 - 1.5 * iqr
  upper_threshold <- q3 + 1.5 * iqr
  
  outliers <- sample[sample < lower_threshold | sample > upper_threshold]
  
  return (outliers)
}


summary(sample_data);

outliers_mean(sample_data);

outliers_iqr(sample_data)