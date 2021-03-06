
# ch10 Functional programming ---------------------------------------------
library(pryr)
library(tidyverse)
set.seed(1014)
df <- data.frame(replicate(6,sample(c(1:10,-99),6, rep = TRUE)))
names(df) <- letters[1:6]


# 10.1 motivation ---------------------------------------------------------


# bad way #
df$a[df$a == -99] <- NA
df$b[df$b == -99] <- NA
df$c[df$c == -99] <- NA
df$d[df$d == -99] <- NA
df$e[df$e == -99] <- NA
df$f[df$f == -99] <- NA

# Dont repeat yourself
fix_missing <- function(x){
  x[x == -99] <- NA
  x
}
df$a <- fix_missing(df$a)
df$b <- fix_missing(df$b)
df$c <- fix_missing(df$c)
df$d <- fix_missing(df$d)
df$e <- fix_missing(df$e)
df$f <- fix_missing(df$f)

# lapply 
df[] <- lapply(df,fix_missing) # return to data.frame
df[1:5] <- lapply(df[1:5],fix_missing) #subset

# closure 
missing_fixer <- function(na_value){
  function(x){
    x[x == na_value] <- NA
    x
  }
}
fix_missing_99 <- missing_fixer(-99)
fix_missing_999 <- missing_fixer(-999)
fix_missing_99(c(-99,-999))
fix_missing_999(c(-99,-999))

##
mean(df$a)
median(df$a)
sd(df$a)
mad(df$a)

summary <- function(x){
  c(mean(x,na.rm = T),
    median(x,na.rm = T),
    sd(x,na.rm = T),
    mad(x,na.rm = T),
    IQR(x,na.rm = T))
} # FRAGILE
lapply(df,summary)

## 
summary <- function(x) {
  funs <- c(mean, median, sd, mad, IQR)
  lapply(funs, function(f) f(x, na.rm = TRUE))
}
lapply(df,summary)



# 10.2 anonymous func -----------------------------------------------------

#ex
lapply(mtcars,function(x) length(unique(x)))
Filter(function(x) !is.numeric(x),mtcars)
integrate(function(x) sin(x)^2,0,pi)

formals(function(x=4) g(x) + h(x))

(function(x) 3)() # function called !
(function(x) x+3)(3) # same as f <- function(x) x+3 , f(3)

# eg
match.fun(mean)
# 
lapply(mtcars,function(x) sd(x,na.rm=T))
# integerate
integrate(function(x) x^2-x, 0, 10)


# 10.3 Closures -----------------------------------------------------------

power <- function(exponent){
  function(x){
    x^exponent
  }
}
square <- power(2)
cube <- power(3)
square(10)
cube(10)

as.list(environment(square))
as.list(environment(cube))
cube(10)

pryr::unenclose(square)

power <- function(exponent){
  print(environment())
  function(x) x^exponent
}
zero <- power(0) ## function capture their enclosing enviroment
environment(zero) ##


# 10.3.1 function factories -----------------------------------------------


# 10.3.2 mutable state ----------------------------------------------------

new_counter <- function(){
  i <- 0
  print(environment())
  function(){
    i <<- i + 1
    i
  }
}

# Modifying values in a parent environment is an important technique
# because it is one way to generate “mutable state” in R.

counter_one <- new_counter()
counter_two <- new_counter()
counter_one()
counter_one()
as.list(environment(counter_one))

counter_two()
counter_two()

### eg 
lapply(mtcars,function(x) x[[5]])

pick <- function(loc){
  function(x) x[[loc]]
}
lapply(mtcars,pick(5))

pick(5)(mtcars$mpg)



# 10.4 Lists of functions -------------------------------------------------

compute_mean <- list(
  base = function(x) mean(x),
  sum = function(x) sum(x)/length(x),
  manual = function(x){
    total <- 0
    n <- length(x)
    for (i in seq_along(x)){
      total <- total + x[i]/n
    }
    total
  }
)


x <- runif(1e5)
system.time(compute_mean$base(x))
system.time(compute_mean$sum(x))
system.time(compute_mean$manual(x))

lapply(compute_mean,function(f) f(x))

call_fun <- function(f,...) f(...)
lapply(compute_mean,call_fun,x)
# time each function in list
lapply(compute_mean,function(f) system.time(f(x)))

#
x <- 1:10 
funs <- list(
  sum = sum,
  mean = mean,
  median = median
)

lapply(funs,function(f) f(x))

# how to remove missing data ?
funs2 <- list( 
  # not that good method #
  sum = function(x) sum(x,na.rm=T),
  mean = function(x) mean(x,na.rm=T),
  median = function(x) median(x,na.rm=T)
)
lapply(funs2,function(f) f(x))

lapply(funs, function(f) f(x,na.rm=T))


# 10.4.1 ------------------------------------------------------------------
## moving lists of functions to the global envir

simple_tag <- function(tag){
  force(tag)
  function(...){
    paste0("<",tag,">",paste0(...),"</",tag,">")
  }
}

simple_tag('p')


tags <- c("p",'b','i')
html <- lapply(setNames(tags,tags),simple_tag)
html$p("This is ",html$b("bold")," text.")

##1 best way 
with(html,p("This is ", b("bold"),"text."))

# 2
attach(html)
p("This is ",b("bold")," text.")
detach(html)


# 3 copy to global environment

list2env(html, environment())
p("This is ",b("bold")," text.")
rm(list = names(html),envir = environment())


### eg. summary function ###

field_names <- c('Min','Mean','Max')

mySummary <- list(
  my_min <- function(x) min(x),
  my_median <- function(x) quantile(x,0.25),
  my_mean <- function(x) mean(x)
)
x = 1:20
lapply(mySummary,function(f) f(x))
mySummary[[2]](1:10)



# 10.5 Case study: numerical integration ----------------------------------

# integrate sin(x) from 0 to pi ~ 2

midpoint <- function(f,a,b){
  (b-a) * f((a+b)/2)
}

trapezoid <- function(f,a,b){
  (b-a) / 2*(f(a) + f(b))
}
# test 
midpoint(sin,0,pi)
trapezoid(sin,0,pi)


midpoint_composite <- function(f,a,b,n=10){
  points <- seq(a,b,length = n+1)
  h <- (b-a) / n
  
  area <- 0
  for (i in seq_len(n)){
    area <- area + h * f((points[i] + points[i + 1])/2)
  }
  area
}
trapezoid_composite <- function(f, a, b, n = 10) {
  points <- seq(a, b, length = n + 1)
  h <- (b - a) / n
  area <- 0
  for (i in seq_len(n)) {
    area <- area + h / 2 * (f(points[i]) + f(points[i + 1]))
  }
  area
}

midpoint_composite(sin,0,pi,n=10)
midpoint_composite(sin,0,pi,n=100)
trapezoid_composite(sin,0,pi,n=10)
trapezoid_composite(sin,0,pi,n=100)
## modify 

composite <- function(f,a,b,n=10,rule){
  points <- seq(a,b,length = n+1)
  
  area <- 0
  for (i in seq_len(n)){
    area <- area + rule(f,points[i],points[i+1])
  }
  area
}

composite(sin,0,pi,n=10,rule = midpoint)
composite(sin,0,pi,n=10,rule = trapezoid)
