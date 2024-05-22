#### Laboratory 11

### Exercises - Compulsory

## Exercise 1


# We suppose that probabilities are strictly positive.
# use function a runif
# first, calculate the probabilities
# first interval is 0 p1, after that p1 + ...







### Exercises - During Class

sample(1:50, 10)
sample(11:500, 15)
sample(20)
x = c(2.3, 1.4, 2.6, 1.7, 3.2, 2.8)
sample(x, 3)
sample(x, 10, replace = T)
runif(5, 3, 4.5)
runif(6)

a = c(1, 0, 1, 1, 1, 0, 0, 0, 1)
A = matrix(a, 3, 3)
A

b = c(1, 1, 1, 0, 1, 0, 1, 0, 0)
B = matrix(b, 3, 3)
B

C = c(0, 1, 0, 1, 1, 0, 1, 0, 1)
c = matrix(C, 3, 3)
C


matrix_product = function(A, B, C)
{
  n = nrow(A);
  r = matrix(, nrow = n, ncol = 1);
  x = matrix( , nrow = n, ncol = 1);
  y = matrix( , nrow = n, ncol = 1);
  r = sample(0:1, n, replace = TRUE);
  
  for (i in 1:n)
  {
    x[i] = 0;
    for (j in 1:n)
    {
      x[i] = (x[i] + B[i, j] * r[j])%%2;
    }
  }
  for (i in 1:n)
  {
    y[i] = 0;
    for (j in 1:n)
    {
      y[i] = (y[i] + A[i, j] * x[j])%%2;
    }
  }
  for (i in 1:n)
  {
    x[i] = 0;
    for (j in 1:n)
    {
      x[i] = (x[i] + C[i, j] * r[j])%%2;
    }
    if(x[i] != y[i])
    {
      return(FALSE);
    }
  }
}

matrix_product(A, B, C)
C[1, 1] = 1

matrix_product_reduce = function (A, B, C, k)
{
  for (i in 1:k)
    if (!matrix_product(A, B, C))
      return(FALSE);
  return(TRUE);
}

tree_eval = function (i, leaves)
{
  a = runif(1, 0, 1);
  len = length(leaves);
  if(log(i,2) >= log(len,2)- 1)
  {
    # the children of node i are leaves
    if(a <= 0.5)
    {
      if (leaves[2*i- len + 1] == 0)
        return (leaves[2*i +1-len + 1]);
      return(1);
    }
    else
    {
      if (leaves[2*i + 1-len + 1] == 0)
        return (leaves[2*i- len + 1];
      return(1);
    }
  }
  if ((floor(log(i,2))%% 2 == 0))
  {
    # the node i is a MIN one
    if (a <= 0.5)
    {
      if (tree_eval (2*i, leaves) == 1)
        return(tree_eval (2*i + 1, leaves));
      return(0);
    }
    else
    {
      if (tree_eval (2*i +1, leaves) == 1)
        return(tree_eval (2*i, leaves));
      return(0);
    }
  }
  if ((floor(log(i,2))%% 2 == 1))
  {
    # the node i is a MIN one
    if (a <= 0.5)
    {
      if (tree_eval (2*i, leaves) == 0)
        return(tree_eval (2*i + 1, leaves));
      return(1);
    }
    else
    {
      if (tree_eval (2*i +1, leaves) == 0)
        return(tree_eval (2*i, leaves));
      return(1);
    }
  }
}

