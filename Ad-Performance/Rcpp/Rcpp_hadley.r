
# Rcpp  -------------------------------------------------------------------

library(Rcpp)
library(microbenchmark) 

# Getting start with cpp ----------------------------------------------------

## intro
cppFunction('int add(int x, int y, int z){
  int sum = x + y + z;
  return sum;
}')

add(1,2,3)

## no inputs, scalar output

one <- function() 1L

cppFunction('int one(){
  return 1;
}')

## scalar input, scalar output

signR <- function(x) {
  if (x > 0) {
    1
  } else if (x == 0) {
    0
  } else {
    -1
  }
}

signR(10)

cppFunction('int signC(int x){
  if (x > 0){
    return 1;
  } else if (x==0){
    return 0;
  } else {
    return -1;
  }
}')

signC(0)
sequence(10)
x <- 1:10
tracemem(x)
x[5] <- 6L
library(pryr)
address(x)

y <- x
y[3] <- 4
address(x)
address(y)

## Vector input, scalar output ###
sumR <- function(x){
  total <- 0
  for (i in seq_along(x)){
    total <- total + x[i]
  }
  total
}

cppFunction('double sumC(NumericVector x){
  int n = x.size();
  double total = 0;
  for(int i = 0; i < n; ++i){
    total += x[i];
    //printf("i : %d \\n",i);
  }
  return total;
}')
sumC(1:10)
x <- runif(1e3)
microbenchmark(
  sum(x),
  sumC(x),
  sumR(x)
)

# Using sourceCpp ---------------------------------------------------------


# Attributes and other classes --------------------------------------------


# Rcpp sugar --------------------------------------------------------------


# The STL -----------------------------------------------------------------


# Case study --------------------------------------------------------------


# putting Rcpp in package -----------------------------------------------------------------



