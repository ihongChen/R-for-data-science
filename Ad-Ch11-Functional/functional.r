
# ch11 Functional  --------------------------------------------------------


randomise <- function(f) f(runif(1e3))
randomise(mean)
randomise(sum)

lapply2 <- function(x,f,...){
  out <- vector("list",length(x))
  for (i in seq_along(x)){
    out[[i]] <- f(x[[i]],...)
  }
}

lapply(list(x=c(1,2,3),y=c(1.0,2.3,4.4),z = runif(1e2)),
       mean)
out <- vector("list",3)


## create random data (list)
l <- replicate(20,runif(sample(1:10,1)),simplify = F)
# with for loop
out <- vector("list",length(l))
for (i in seq_along(l)){
  out[[i]] <- length(l[[i]])
}
unlist(out)
## with lapply 
unlist(lapply(l,length))

## data.frame with lapply
head(mtcars)
unlist(lapply(mtcars,class))
mtcars[] <- lapply(mtcars,function(x) x/mean(x))

head(mtcars)
mtcars

# 11.1.1 Looping patterns -------------------------------------------------

# 1. loop over elements for(x in xs)
# 2. loop over the numeric indices for (i in seq_alongs(xs))
# 3. loop over names : for (nm in names(xs))

xs <- runif(1e5)
res <- c()
system.time(
  for (x in xs){
  # This is slow !!
  res <- c(res,sqrt(x))
  }
)
res

# 生成固定長度的向量，減少記憶體複製
res <- numeric(length(xs))
system.time(
  for (i in seq_along(xs)){
  res[i] <- sqrt(xs[i])
  }
)

# different way use lapply
lapply(xs,function(x){})
lapply(seq_along(xs),function(i){})
lapply(names(xs),function(nm){})


# 11.1.2 ex ---------------------------------------------------------------
## 1
trims <- c(0,0.1,0.2,0.5)
x <- rcauchy(100)
lapply(trims,mean,x=x)
lapply(trims,function(tm) mean(x,trim=tm))

## 2.

vec1 <- runif(1e3,min=-10,max=20)
scale01 <- function(x){
  rng <- range(x,na.rm=T)
  (x-rng[1])/(rng[2]-rng[1])
}
scale01(vec1)
lapply(mtcars,scale01)

df1 <- data.frame(x=c(1,2,3),l=c('a','b','c'))
lapply(df1,function(col){
  if (class(col)!= 'numeric') col
  else scale01(col)
})

### 3. 
formulas <- list(
  mpg ~ disp,
  mpg ~ I(1 / disp),
  mpg ~ disp + wt,
  mpg ~ I(1 / disp) + wt
)

lm(mpg~disp,data = mtcars)
lapply(formulas,function(formula) lm(formula,mtcars))

# 11.2 friends of lapply --------------------------------------------------
# 

class(mtcars)
sapply(mtcars,is.numeric)
vapply(mtcars,is.numeric,logical(1))

sapply(list(),is.numeric) #list()
vapply(list(),is.numeric,logical(1)) # logical(0)


df <- data.frame(x=1:10,y=letters[1:10])
sapply(df,class)
vapply(df,class,character(1))

df2 <- data.frame(x=1:10,y=Sys.time()+1:10)
sapply(df2,class)
vapply(df2,class,character(1))
df2$y


# 11.2.2 mapply map -------------------------------------------------------

xs <- replicate(5,runif(10),simplify = F)
ws <- replicate(5,rpois(10,5)+1,simplify = F)
ws
unlist(lapply(xs,mean))

unlist(Map(weighted.mean,xs,ws))

mtmeans <- lapply(mtcars,mean)

mtmeans[] <- Map(`/`,mtcars,mtmeans)

mtmeans

mtcars[] <- lapply(mtcars,function(x) x/mean(x))
mtcars

# 11.2.3 rolling computation ----------------------------------------------

rollmean <- function(x,n){
  out <- rep(NA,length(x))
  offset <- trunc(n/2)
  for (i in (offset+1):(length(x) - n + offset - 1)){
    out[i] <- mean(x[(i - offset):(i + offset - 1)])
  }
  out
}

x <- seq(1,3,length=1e2) + runif(1e2)
plot(x)

lines(rollmean(x,5),col="blue",lwd=2)
lines(rollmean(x, 10), col = "red", lwd = 2)

x <- seq(1,3,length=1e2) + rt(1e2,df=2)/3
plot(x)
lines(rollmean(x, 5), col = "red", lwd = 2)

# instead of copy and paste we could...
rollapply <- function(x,n,f,...){
  out <- rep(NA,length(x))
  
  offset <- trunc(n/2)
  for (i in (offset + 1):(length(x) - n + offset + 1)) {
    out[i] <- f(x[(i - offset):(i + offset)], ...)
  }
  out
}

plot(x)
lines(rollapply(x, 5, median,1), col = "red", lwd = 2)

## rollapply2 

rollapply2 <- function(x,n,f,...){
  offset <- trunc(n/2)
  locs <- (offset + 1):(length(x) - n + offset + 1)
  num <- vapply(
    locs,
    function(i) f(x[(i - offset):(i + offset)], ...),
    numeric(1)
  )
  c(rep(NA, offset), num)
}

rollapply2(x,5,median)


## test ellipsis ... 
my_ellipsis_function <- function(...) {
  input_list <- as.list(list(...))
  str(input_list)
  NULL
}
temp_ellipsis <- my_ellipsis_function(a=1:10,b=11:20,c=21:30)

# 11.2.4 Parallelisation --------------------------------------------------
library(microbenchmark)
lapply3 <- function(x, f, ...) {
  # sequential 
  out <- vector("list", length(x))
  for (i in sample(seq_along(x))) {
    out[[i]] <- f(x[[i]], ...)
  }
  out
}

microbenchmark(
  seq_lapply = lapply3(c(1,3,5,7,9,12),sqrt), # 12 us
  lapply = lapply(c(1,3,5,7,9,12),sqrt) # 6 us
  )

## parallel version in linux (not available in win os)
library(parallel)

microbenchmark(
  seq_lapply = lapply3(c(1,3,5,7,9,12),sqrt), # 12 us
  lapply = lapply(c(1,3,5,7,9,12),sqrt), # 6 us
  parallel = mclapply(c(1,3,5,7,9,12),sqrt,mc.cores = 4) ## 
)


boot_df <- function(x) x[sample(nrow(x),rep=T),]
rsquared <- function(mod) summary(mod)$r.square
boot_lm <- function(i) {
  rsquared(lm(mpg ~ wt + disp, data = boot_df(mtcars)))
}
system.time(lapply(1:500, boot_lm))
system.time(mclapply(1:500,boot_lm))
microbenchmark(
  lapply = lapply(1:500, boot_lm),
  mclapply = mclapply(1:500,boot_lm)
)

# 11.2.5 ex ----------------------------------------------------------------------


sapply(mtcars,class)
vapply(mtcars,class,character(1))

library(nycflights13)
sapply(flights,class)
lapply(flights,class)
