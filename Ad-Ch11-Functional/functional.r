
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
