install.packages("microbenchmark")
library(microbenchmark)

x <- runif(100)
microbenchmark(
  sqrt(x),
  x ^ 0.5
)

f <- function(x) NULL

s3 <- function(x) UseMethod("s3")
s3.integer <- f

A <- setClass("A", representation(a = "list"))
setGeneric("s4", function(x) standardGeneric("s4"))
setMethod(s4, "A", f)

B <- setRefClass("B", methods = list(rc = f))

a <- A()
b <- B$new()

microbenchmark(
  fun = f(),
  S3 = s3(1L),
  S4 = s4(a),
  RC = b$rc()
)


# memory ------------------------------------------------------------------

# devtools::install_github("hadley/lineprof")
library(pryr)

object_size(1:10)
object_size(mtcars)

sizes <- sapply(0:50, function(n) object_size(seq_len(n)))
plot(0:50, sizes, xlab = "Length", ylab = "Size (bytes)", 
     type = "s")
object_size(numeric()) # 40B
object_size(list()) # 40B
object_size(raw())

plot(0:50, sizes - 40, xlab = "Length", 
     ylab = "Bytes excluding overhead", type = "n")
abline(h = 0, col = "grey80")
abline(h = c(8, 16, 32, 48, 64, 128), col = "grey80")
abline(a = 0, b = 4, col = "grey90", lwd = 4)
lines(sizes - 40, type = "s")

### 
x <- 1:1e6
object_size(x)#4Mb
# point the same memory 
y <- list(x, x, x)
object_size(y) # 4Mb

#
x1 <- 1:1e6
y1 <- list(1:1e6,1:1e6,1:1e6)
object_size(x1)
object_size(y1)
object_size(x1,y1)

# string 
object_size("banana")
object_size(rep("banana",10))

object_size(list(1:5))
object_size(1:5)
object_size(list(1:5))

# Memory used and gc ------------------------------------------------------

mem_used()
mem_change(x <- 1:1e6)
mem_change(rm(x))

mem_change(x <- 1:1e6)
mem_change(y <- x)
mem_change(rm(x))
mem_change(rm(y))

f1 <- function() {
  x <- 1:1e6
  10
}
mem_change(x <- f1())#1.1kB
object_size(x ) # 48B

f2 <- function() {
  x <- 1:1e6
  a ~ b
}
mem_change(y <- f2())
object_size(y)

f3 <- function() {
  x <- 1:1e6
  function() 10
}
mem_change(z <- f3())


# modification in place ---------------------------------------------------

library(pryr)
x <- 1:10
c(address(x), refs(x))
# [1] "0x20106fb0" "1"

y <- x
c(address(y), refs(y))
# [1] "0x20106fb0" "2"
x <- 1:5
y <- x
rm(y)
# Should really be 1, because we've deleted y
refs(x)


x <- 1:5
y <- x
z <- x
# Should really be 3
refs(x)
x <- 1:10
y <- x
c(address(x), address(y))


x[5] <- 6L
c(address(x), address(y))


x <- 1:10
# Prints the current memory location of the object
tracemem(x)
# [1] "<0x7feeaaa1c6b8>"

x[5] <- 6L

y <- x
# Prints where it has moved from and to
x[5] <- 6L
# tracemem[0x7feeaaa1c6b8 -> 0x7feeaaa1c768]:

x[3] <- 3L

f <- function(x) x
{x <- 1:10;f(x);refs(x)}

x <- data.frame(matrix(runif(100*1e4),ncol=100))
medians <- vapply(x, median, numeric(1))
for (i in seq_along(medians)){
  x[,i] <- x[,i] - medians[i]
}
  
for (i in 1:5){
  x[,i] <- x[,i] - medians[i]
  print(c(address(x),refs(x)))
}
refs(x)


y <- as.list(x)

for (i in 1:5){
  y[[i]] <- y[[i]] - medians[i]
  print(c(address(y),refs(y)))
}
  


# from ptt ----------------------------------------------------------------

A_list <- list()
for (i in 1:10){
  A_list[[i]] <- 1
}

List_1 <- list(a=1:5,b=2:4)
tracemem(List_1)
tracemem(List_1[[1]])
tracemem(List_1[[2]])

s1 <- list()
cat(tracemem(s1),'\n')
## s1 memory loc changed
for (i in 1:3){
  s1[[i]] <- rnorm(i)
  cat(tracemem(s1),'\n')
}

## pre allocate 
s2 <- vector('list',3)

for (i in 1:3){
  s2[[i]] <- rnorm(i)
  cat('list loc',tracemem(s2),'\n')
  cat('element',i,tracemem(s2[[i]]),'\n')
}



# pre-allocate example ----------------------------------------------------------------------
f_without_preallocated <- function(){
  a <- list()
  for (i in 1:1e4) a[[i]] <- rnorm(10)
}

f_with_preallocated <- function(){
  a <- vector('list',1e6)
  for (i in 1:1e4) a[[i]] <- rnorm(10)
}

library(microbenchmark)
microbenchmark(
 f_with_preallocated(),
 f_without_preallocated()
)



N = 1e4 
st <- proc.time() 
a12 <- lapply(1:N, 
              function(i) replicate(10, rnorm(1000))) 
b <- rnorm(10) 
a22 <- lapply(1:N, 
              function(i) a12[[i]] %*% b + rnorm(1000)) 
a32 <- lapply(1:N, 
              function(i) lm(a22[[i]] ~ a12[[i]])) 
a42 <- lapply(1:N, function(i) fitted(a32[[i]])) 
a52 <- lapply(1:N, function(i) resid(a32[[i]])) 
proc.time() - st 
proc.time() 
a13 <- vector('list', N) 
a23 <- vector('list', N) 
a33 <- vector('list', N) 
a43 <- vector('list', N) 
a53 <- vector('list', N) 
b <- rnorm(10) 
for (i in 1:N){ 
  a13[[i]] <- replicate(10, rnorm(1000)) 
  a23[[i]] <- a12[[i]] %*% b + rnorm(1000) 
  a33[[i]] <- lm(a22[[i]] ~ a12[[i]]) 
  a43[[i]] <- fitted(a32[[i]]) 
  a53[[i]] <- resid(a32[[i]]) 
} 
proc.time() - st



#  --------------------------------------------------------------
library(Matrix)

jaccard_similarity <- function(x,y) {
  
    # x <- Matrix(as(x,"matrix"),sparse=T)
  # y <- Matrix(as(y,"matrix"),sparse=T)
  
  a <- tcrossprod(x,y)
  
  Matrix(a,sparse = T)
  
  nx <- nrow(x)
  ny <- nrow(y)
  
  c <- Matrix(rowSums(x), nrow = nx, ncol = ny, sparse = TRUE) - a
  b <- Matrix(rowSums(y), nrow = nx, ncol = ny, byrow = TRUE, sparse = TRUE) - a
  
  sim <- a / (a + b + c)
  sim <- replace(sim,is.na(sim),0)
  sim <- Matrix(sim,sparse=T)
  sim
}

nx <- 1e4;
ny <- 1e4;
nn <- 3000
i <- sample(1:nx,nn,replace = T)
j <- sample(1:ny,nn,replace = T)
x <- sample(c(1,0),nn,replace = T)
A <- sparseMatrix(i, j, x = x)

system.time(
  sim <- jaccard_similarity(A,A)  
)

sim
temp <- tcrossprod(A,A)


sim

a <- tcrossprod(A,A)
a[a==0] <- 0
Matrix(a,sparse = T)

c <- Matrix(rowSums(A), nrow = nx, ncol = ny, sparse = TRUE) - a
c
c[c==0] <- 0
b <- Matrix(rowSums(A), nrow = nx, ncol = ny, sparse = TRUE) - a
b[b==0] <- 0
b
temp <- a/(a+b+c)
temp2 <- Matrix(replace(temp,is.na(temp),0),sparse=T)
temp2
