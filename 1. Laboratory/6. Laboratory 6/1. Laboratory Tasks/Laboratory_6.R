#### Laboratory 6



### Exercises - Compulsory



## Exercise II.6

# NEVER MIND



## Exercise III.4

confidence_intervalTNew <- function (file_path, alfa)
{
  sample_data = scan(file_path)
  
  sample_mean = mean(sample_data)
  s = sd(sample_data)
  n = length(sample_data)
  
  critical_t = -qt(alfa / 2, n - 1)
  
  left <- sample_mean - critical_t * s / sqrt(n)
  right <- sample_mean + critical_t * s / sqrt(n)
  
  print(paste("Sample Mean:", sample_mean))
  print(paste("Sample Standard Deviation:", s))
  print(paste("Sample Size:", n))
  print(paste("Left: ", left))
  print(paste("Right: ", right))
}


file_path = "history.txt"
confidence_intervalTNew(file_path, 0.05)  # For 95% confidence
confidence_intervalTNew(file_path, 0.01)  # For 99% confidence



## Exercise IV.2

proportion_test = function(n, p_prime, p_0, alfa, type)
{
  z_score = (p_prime - p_0) / sqrt(p_0*(1-p_0)/n)
  print(z_score)
  if(type == '1')
  {
    critical_z = qnorm(alfa)
    print(critical_z)
    if(z_score < critical_z)
    {
      print("H_0 is rejected and H_a is accepted.")
    }
    else
    {
      print("we cannot reject h_0")
    }
  }
  if (type == 'r')
  {
    critical_z = qnorm(1 - alfa)
    print(critical_z)
    if (z_score > critical_z)
    {
      print("H_0 is rejected and H_a is accepted.")
    }
    else
    {
      print("we cannot reject H_0")
    }
  }
  if (type == 's')
  {
    critical_z = -qnorm(alfa/2)
    print(critical_z)
    if (abs(z_score) < critical_z)
    {
      print("H_0 is rejected and H_a is accepted.")
    }
    else
    {
      print("we cannot reject H_0")
    }
  }
}


proportion_test(150, 20/150, 0.10, 0.05, 'r')



### Exercises - During Class



## Exercise II.1



confidence_intervalZ = function (n, sample_mean, sigma, alfa)
{
  critical_z = -qnorm(alpha/2)
  left = sample_mean - critical_z * sigma / sqrt(n);
  right = sample_mean + critical_z * sigma / sqrt(n);
  print(left)
  print(right)
}

confidence_intervalz(100, 20, 3, 0.1)



## Exercise II.2

confidence_intervalz(25, 67.53, 10, 0.1);



## Exercise II.3

confidence_intervalz(50, 5, 0.5, 0.05);



## Exercise II.4

confidence_intervalz(100, 1280, 140, 0.01);



## Exercise II.5

confidence_intervalz(35, 60, 5, 0.1)



## Exercise III.1

confidence_intervalT = function (n, sample_mean, s, alfa)
{
  critical_t = -qt(alfa/2, n - 1)
  left = sample_mean - critical_t * s / sqrt(n);
  right = sample_mean + critical_t * s / sqrt(n);
  print(left)
  print(right)
}

confidence_intervalT(60, 3.3, 0.4, 0.05)



## Exercise III.2

confidence_intervalT(196, 44.65, 1.5, 0.01)



## Exercise III.3

confidence_intervalT(49, 12, 1.75, 0.05)
confidence_intervalT(49, 13.5, 1.25, 0.05)



## Exercise III.5

x = c(12, 11, 12, 10, 11, 12, 13, 12, 11, 11, 13, 14, 10)
confidence_intervalT(length(x), mean(x), sd(x), 0.1)



## Exercise IV.1

proportion_test = function(n, p_prime, p_0, alfa, type)
{
  z_score = (p_prime - p_0) / sqrt(p_0*(1-p_0)/n)
  print(z_score)
  if(type == '1')
  {
    critical_z = qnorm(alfa)
    print(critical_z)
    if(z_score < critical_z)
    {
      print("H_0 is rejected and H_a is accepted.")
    }
    else
    {
      print("we cannot reject h_0")
    }
  }
  if (type == 'r')
  {
    critical_z = qnorm(1 - alfa)
    print(critical_z)
    if (z_score > critical_z)
    {
      print("H_0 is rejected and H_a is accepted.")
    }
    else
    {
      print("we cannot reject H_0")
    }
  }
  if (type == 's')
  {
    critical_z = -qnorm(alfa/2)
    print(critical_z)
    if (abs(z_score) < critical_z)
    {
      print("H_0 is rejected and H_a is accepted.")
    }
    else
    {
      print("we cannot reject H_0")
    }
  }
}


proportion_test(100, 0.63, 0.6, 0, 0.05, 'r')



# Note:
# next time we have to present the homework
# and send the homework to email in one file
# until June 4, 8:00 AM
# email: fe.olariu@gmail.com