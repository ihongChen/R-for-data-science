
# using sparse matrix  ----------------------------------------------------
library('Matrix')
library('pryr')

pex <- function(p) {
  dp <- diff(p)
  rep(seq_along(dp), dp)
}

pex(p=c(0,1,2,3))
dp <- diff(c(0,1,2,3))

pex()
# from 
## https://www.r-bloggers.com/using-sparse-matrices-in-r/ -------------



m1 <- matrix(0,nrow=1000,ncol=1000)
m2 <- Matrix(0,nrow=1000,ncol=1000,sparse=TRUE)

object_size(m1)
object_size(m2)

m1[500,500] <- 1
m2[500,500] <- 1

object_size(m1)
object_size(m2)

m2[500,500] <- 1L
m2 %*% rnorm(1000)
m2 + m2
m2 - m2 

m3 <- cBind(m2,m2) 
nrow(m3)
ncol(m3)
m4 <- rbind(m2,m2)


# save/load to file --------------------------------------------------------
writeMM(m4,file = 'sp_m4.txt')
m4 <- readMM(file = 'sp_m4.txt')
m4
# slam package ------------------------------------------------------------

library('slam');


# casting large matrix with GLMNET ----------------------------------------------------
library(glmnet)
n <- 10000
p <- 500

x <- matrix(rnorm(n*p),n,p)
iz <- sample(1:(n*p),size=n*p*0.85,replace=F)
x[iz] <- 0

object_size(x)

sx <- Matrix(x,sparse=T)
object_size(sx)
beta <- rnorm(p)

y <- x%*% beta + rnorm(n)
glmnet.fit <- glmnet(x,y)

# how much more efficient with sparse matrix ?
set.seed(1)
performance <- data.frame()

for (sim in 1:10){
  n <- 10000
  p <- 500
  nzc <- trunc(p/10)
  x <- matrix(rnorm(n * p), n, p)
  iz <- sample(1:(n * p),
               size = n * p * 0.85,
               replace = FALSE)
  x[iz] <- 0
  sx <- Matrix(x, sparse = TRUE)
  
  beta <- rnorm(nzc)
  fx <- x[, seq(nzc)] %*% beta
  
  eps <- rnorm(n)
  y <- fx + eps
  
  sparse.times <- system.time(fit1 <- glmnet(sx, y))
  full.times <- system.time(fit2 <- glmnet(x, y))
  sparse.size <- as.numeric(object.size(sx))
  full.size <- as.numeric(object.size(x))
  
  performance <- rbind(performance, data.frame(Format = 'Sparse',
                                               UserTime = sparse.times[1],
                                               SystemTime = sparse.times[2],
                                               ElapsedTime = sparse.times[3],
                                               Size = sparse.size))
  performance <- rbind(performance, data.frame(Format = 'Full',
                                               UserTime = full.times[1],
                                               SystemTime = full.times[2],
                                               ElapsedTime = full.times[3],
                                               Size = full.size))
}



## plot 
performance
## casting sparse matrix 

set.seed(11)
N = 10 

data <- data.frame(
  row = sample(1:3,N,replace=T),
  col = sample(LETTERS,N,replace=T),
  value = sample(1:3,N,replace=T)
)

## transform data type ##
data2 <- transform(data,
          row = factor(row),
          col = factor(col))

data.sparse <- sparseMatrix(as.integer(data$row),
                            as.integer(data$col),
                            x = data$value)

data.sparse
