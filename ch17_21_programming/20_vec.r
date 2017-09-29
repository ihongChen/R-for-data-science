
# 20 vectors --------------------------------------------------------------


library(tidyverse)
typeof(letters)
typeof(1:10)
x <- list("a","b",1:10)
length(x)



# logical -----------------------------------------------------------------

1:10 %%3 ==0

c(TRUE,TRUE,TRUE,NA)


# Numeric -----------------------------------------------------------------

typeof(1) #typeof(1)-->double, class(1) --> numeric
typeof(1L) # typeof(1L)--> interger, class(1L) --> integer

x <- sqrt(2)^2
x -2 # 
dplyr::near(x-2,0)

# integer: NA NaN, double:NaN,NA,Inf,-Inf,

c(1,0,-1)/0

is.finite(0)
is.infinite(Inf)
is.na(NA)
is.nan(0/0)


# character ---------------------------------------------------------------

x <- "This is a reasonably long string."
pryr::object_size(x)
#> 136 B

y <- rep(x, 1000)
pryr::object_size(y)
#> 8.13 kB # pointer 8 byte, 1000 8*1000 + 136 = 8.13 kB


# missing value -----------------------------------------------------------

NA #logical
NA_integer_ # integer 
NA_real_ # typeof(NA_real_) : double
NA_character_ # typeof(NA_character_) : character


# ex ----------------------------------------------------------------------
# how to parse string --> number (readr package)
parse_integer(c("1","2")) 
parse_character(c(1,2))
parse_logical(c("true","false"))
parse_logical(c("t","f"))

## how to name vector? how to pull out elements of interest?
x <- c('1','2','3')
names(x) <-paste0('character_vec',1:3)
x['character_vec1']


# Corecion ... as.integer()... --------------------------------------------

##  implicit coercion !!! 
x <- sample(20,100,replace=TRUE)
y <- x>10 
sum(y)
mean(y)


typeof(c(TRUE, 1L))
#> [1] "integer"
typeof(c(1L, 1.5))
#> [1] "double"
typeof(c(1.5, "a"))
#> [1] "character"

is_logical(TRUE)
is_integer(1)
is_character("2")
is_list(list(a='1',b=c(2,3,4),c=TRUE))
is_vector(c(1,2,3))
is_atomic(c(1,2,3))

## recycling 
sample(100) + 100
runif(10) <0.1

1:10 + 1:2
1:11 + 1:2

## recycling can be conceal and danger 
tibble(x=1:10,y=1:2) # error ,
tibble(x=1:10,y=rep(1:2,5))
tibble(x=1:10,y=rep(1:2,each=5))

# naming ------------------------------------------------------------------


x <- c(x=1,y=2,z=3)
set_names(1:3,c("x","y","z"))

x[c(3,1,2)]
x[c(-1,-2)] # in R -1 remove 1 in vec
x[c(1,-1)] # erroe mix neg,pos


x <- c(10, 3, NA, 5, 8, 1, NA)

# All non-missing values of x
x[!is.na(x)]
#> [1] 10  3  5  8  1

# All even (or missing!) values of x
x[x %% 2 == 0]
#> [1] 10 NA  8 NA


x <- c(abc = 1, def = 2, xyz = 5)
x[c("xyz", "def")]
#> xyz def 
#>   5   2


# Ex ----------------------------------------------------------------------

x <- c(10, 3, NA, 5, 8, 1, NA)
mean(is.na(x))
sum(!is.finite(x)) ## same as : sum(is.na(x))

y <- c(a=1,b=2,c=3)
y[['c']] # 
y['c'] # 

x[1:length(x)%%2==0]



# list --------------------------------------------------------------------

x <- list(1,2,3)
str(x)

x_named <- list(a=1,b=2,c=3)
str(x_named)

# subsetting

a <- list(a = 1:3, b = "a string", c = pi, d = list(-1, -5))
a$a
a[[1]]
a[1]

str(a[[4]])
str(a[4])

a[[4]]


# attributes --------------------------------------------------------------

x <- 1:10
attr(x,"gretting")
attr(x,"gretting") <- "Hi!"
attr(x,"fareware") <- "Bye!"
attributes(x)

## base R ,attributes: name,dim,class
as.Date # generic function
methods(as.Date)
getS3method("as.Date", "default")
getS3method("as.Date", "numeric")



# Auguments vectors -------------------------------------------------------

## factors
x <- factor(c("ab", "cd", "ab"), levels = c("ab", "cd", "ef"))
typeof(x)
#> [1] "integer"
attributes(x)
#> $levels
#> [1] "ab" "cd" "ef"
#> 
#> $class
#> [1] "factor"



## Dates, and datetimes

x <- as.Date('1971-05-01')
unclass(x)
x <- lubridate::ymd_hm("1970-01-01 01:00") # better 
unclass(x)
attributes(x)
attr(x, "tzone") <- "US/Pacific"
x
#> [1] "1969-12-31 17:00:00 PST"

attr(x, "tzone") <- "US/Eastern"
x
#> [1] "1969-12-31 20:00:00 EST"

tb <- tibble::tibble(x=1:5,y=1:5)
typeof(tb)
class(tb)
attributes(tb)
str(tb)


test <- hms::hms(3600)
str(test)
attributes(test)

tibble(x=1:3,y=1:2)
list(x=1:3,y=1:2)
