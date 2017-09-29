
# 21. Iteration -----------------------------------------------------------

library(tidyverse)
df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

output <- vector("double",ncol(df)) # output <- numeric(ncol(df))
for (i in seq_along(df)){
  output[[i]] <- median(df[[i]])
}


col_summary <- function(df,fun){
  output <- vector("double",ncol(df))
  for (i in seq_along(df)){
    output[[i]] <- fun(df[[i]])
  }
  output
}
col_summary(df,median)

# map ---------------------------------------------------------------------
# purrr::map  ---- a little bit like map, except shortcut .f 

map_dbl(df,mean) ## same as col_summary
map_dbl(df,median)


map_dbl(df,mean)
z <- list(x=1:3,y=4:5)
map_int(z,length)

#### to build multiple lm models  (usefull skill !!! )#### 
models <- 
mtcars %>% 
  split(.$cyl) %>% 
  map(function(df) lm(mpg~wt,data=df))
  
  
models <- 
  mtcars %>% 
  split(.$cyl) %>% 
  map(~lm(mpg~wt, data = .))
# find R^2 in models
models %>% 
  map(summary) %>% 
  map_dbl(~.$r.squared)


models %>% 
  map(summary) %>% 
  map_dbl("r.squared")

x <- list(list(1,2,3),list(4,5,6),list(7,8,9))
x %>% map_dbl(2)


# Base R apply  ------------------------------------------------------------------

x1 <- list(
  c(0.27, 0.37, 0.57, 0.91, 0.20),
  c(0.90, 0.94, 0.66, 0.63, 0.06), 
  c(0.21, 0.18, 0.69, 0.38, 0.77)
)
x2 <- list(
  c(0.50, 0.72, 0.99, 0.38, 0.78), 
  c(0.93, 0.21, 0.65, 0.13, 0.27), 
  c(0.39, 0.01, 0.38, 0.87, 0.34)
)

threshold <- function(x, cutoff = 0.8) x[x > cutoff]
x1 %>% sapply(threshold) %>% str()
#> List of 3
#>  $ : num 0.91
#>  $ : num [1:2] 0.9 0.94
#>  $ : num(0)
x2 %>% sapply(threshold) %>% str()
#>  num [1:3] 0.99 0.93 0.87



# ex with map -------------------------------------------------------------

mtcars %>% head()
## mean each column
mtcars %>% 
  map_dbl(mean,na.rm=TRUE)

## 
str(nycflights13::flights) 


## 
iris %>% 
  map(unique)
typeof(iris$Sepal.Length)

iris %>% 
  map_chr(typeof)


map(1:5,runif)


# Dealing with failure ------------------------------------------------------
#### safely 
safe_log <- safely(log)
str(safe_log(10))
#> List of 2
#>  $ result: num 2.3
#>  $ error : NULL
str(safe_log("a"))

x <- list(1, 10, "a")
y <- x %>% map(safely(log))
str(y)



y <- y %>% transpose()
str(y)
