#### Laboratory 2

### Exercises



## Exercise I.1

x = c(11, 24, 42, 21) # write the others
x
stem(x)



## Exercise I.2

y = scan("sample1.txt")
y
min(y)
max(y)
interval_bounds = c(40, 50, 60, 70, 80, 90, 100)
hist(y, breaks = interval_bounds, right = r, freq = T)



## Exercise I.3

frecv = c(9, 0 , 12)



## Proposed Exercises

## Exercise I.2

obj = read.csv("unemployment2012.csv", header = T)

rate

limits = c (0, 4, 6, 8, 10, 12, 14, 30)
hist(rate, breaks = limits)
rate = obj[["rate"]]



## Exercise I.3

data = read.csv("life_expect.csv", header = T, sep = ",")
data

m = data[["male"]]
m

f = data[['female']]
f

min(m)
max(m)

limits = seq(65, 85, 5)
limits

hist(m, breaks = limits)

f
min(f)
max(f)

limits = seq(65, 85, 5)
limits

limits = seq(75, 87, 3)
limits
hist(f, breaks = limits)


## Exercise II.1

ex21 = function (filename)
{
  x = scan(filename)
  print(mean(x))
  print(median(x))
}

ex21("sample1.txt")

## Exercise II.2

ex22 = function ()
{
  obj = scan("life_expect.csv")
  m = obj[["male"]]
  print (mean(m))
  print(median(m))
  f = ... #female
}



# Exercise III.

x = scan("sample1.txt")
x
quantile(x)
u = as.vector(quantile("sample1.txt"))
u
u[2]


# Exercise III.1

ex31 = function(x)
{
  n = length(x)
  m = mean(x)
  s = sd(x)
  y = vector();
  k = 0;
  for (i in 1:n)
  {
    if (x[i] > m - 2 * s & m < m + 2 * s)
    {
      k = k + 1
      y[k] = x[i]
    }
    else
    {
      print(x[i]);
    }
  }
  return(y);
}



# Exercise III.2

ex32 = function(x)
{
  n = length(x)
  q1 = as.vector(quantile(x))[2]
  q3 = as.vector(quantile(x))[4]
  iqr = q3 - q1
  y = vector()
  k = 0
  for (i in 1:n)
  {
    if (x[i] > q1 - 1.5 * iqr & x[i] < q3 + 1.5 * iqr)
    {
      k = k + 1
      y[k] = x[i]
    }
    else
    {
      print(x[i])
    }
  }
  return(y)
}



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

if (outliers_mean(sample_data) == outliers_iqr(sample_data))
  print("they are the same")



# homeworks are due to the last week of the semester