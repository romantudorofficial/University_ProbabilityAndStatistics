### Laboratory 7

## Exercises

# Exercise 9

getFirstProbabilitiesPoisson <- function (lambda, number)
{
  numbers <- 0 : number;
  
  probabilities <- dpois(numbers, lambda);
  
  barplot(probabilities, names.arg = numbers, xlab = "Number of Events",
          ylab = "Probability",
          main = paste("Poisson Probability Mass Function"));
}


lambda <- 2.5;
number <- 10;

getFirstProbabilitiesPoisson(lambda, number);



# Exercise 10

getFirstProbabilitiesGeometric <- function (probability, number)
{
  numbers <- 1 : number;
  
  probabilities <- dgeom(numbers, probability);
  
  barplot(probabilities, names.arg = numbers, xlab = "Number of Trials", 
          ylab = "Probability", 
          main = paste("Geometric Probability Mass Function"))
}


probability <- 0.3;
number <- 10;

getFirstProbabilitiesGeometric(probability, number);