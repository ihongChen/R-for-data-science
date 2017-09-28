
# Fast R  -----------------------------------------------------------------

## from :https://m-clark.github.io/docs/fastr.html

# Random walk  -----------------------------------------------------------


## random-walk ##
rw2d_loop_nopre <- function(n) {
  xpos = ypos = 0
  xdir = c(T, F)
  pm1 = c(1,-1)
  
  for (i in 2:n)
  {
    if (sample(xdir, 1)) {
      xpos[i] = xpos[i-1] + sample(pm1, 1)
      ypos[i] = ypos[i-1]
    }
    else {
      xpos[i] = xpos[i-1]
      ypos[i] = ypos[i-1] + sample(pm1, 1)
    }
  }
  data.frame(x=xpos, y=ypos) 
}
data <- rw2d_loop_nopre(1e3)
ggplot2::qplot(x,y,data=data)

debugonce(rw2d_loop_nopre)
data <- rw2d_loop_nopre(1000)

# rw2d_loop_pre(1e5) # for profiling
# ggplot2::qplot(x, y, data=rw2d_loop_pre(1000), geom='path')

#########################################################
# preallocation
#########################################################
rw2d_loop_pre <- function(n) {
  xpos = ypos = numeric(n)
  xdir = c(T, F)
  pm1 = c(1,-1)
  
  for (i in 2:n)
  {
    if (sample(xdir, 1)) {
      xpos[i] = xpos[i-1] + sample(pm1, 1)
      ypos[i] = ypos[i-1]
    }
    else {
      xpos[i] = xpos[i-1]
      ypos[i] = ypos[i-1] + sample(pm1, 1)
    }
  }
  data.frame(x=xpos, y=ypos)
}

microbenchmark::microbenchmark(rw2d_loop_pre(1000),rw2d_loop_nopre(1000))

#########################################################
# recursive
#########################################################
rw2d2_recursive <- function(n, x=0, y=0) {
  if(n == 1) return(cumsum(data.frame(x=x, y=y)))
  step = sample(1:4, 1)
  rw2d2_recursive(n=n-1,
                  x=c(x, c(-1, 1, 0, 0)[step]),
                  y=c(y, c(0, 0, -1, 1)[step]))
}
# cannot be profiled
# ggplot2::qplot(x, y, data=rw2d2_recursive(1000), geom='path')

#########################################################
# Double loops --- no good 
#########################################################

doubleLoopDist <- function(m) {
  n = nrow(m)
  dists = matrix(0, ncol=n, nrow=n)
  for (i in 1:n){
    for (j in 1:n){
      dists[i,j] = sqrt(sum((m[i,] - m[j,])^2))
    }
  }
  dists
}
d = matrix(rnorm(10000), ncol=100)
doubleLoopDist(d)


doubleLoopDist2 <- function(m) {
  n = nrow(m)
  dists = matrix(0, ncol=n, nrow=n)
  for (i in 1:(n-1)){
    for (j in (i+1):n){
      dists[i,j] = sqrt(sum((m[i,] - m[j,])^2))
    }
  }
  
  dists = dists + t(dists) + diag(diag(dists))
  diag(dists) = 0
  dists
}
identical(doubleLoopDist(d), doubleLoopDist2(d))


# apply -------------------------------------------------------------------

means_loop = rep(0, ncol(d))  # initialize the means vector
means_loop_f <- function(d){
  for (n in 1:ncol(d)){
    means_loop[n] = mean(d[,n])
  }
}

menas_loop <- means_loop_f(d)
means_apply = apply(d, 2, mean)
identical(means_loop, means_apply)

microbenchmark::microbenchmark(means_loop_f(d),apply(d,2,mean))


