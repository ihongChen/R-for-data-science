library(tidyverse)
# 19. Functions -----------------------------------------------------------

rescale01 <- function(x){
  rng <- range(x,na.rm=TRUE)
  (x - rng[1]) / (rng[2] - rng)
}
rng <- range(x,na.rm=TRUE)

rescale01


# stop --------------------------------------------------------------------


wt_mean <- function(x, w) {
  if (length(x) != length(w)) {
    stop("`x` and `w` must be the same length", call. = FALSE)
  }
  sum(w * x) / sum(w)
}
wt_mean(c(1,5,3),c(2,3,4))
wt_mean(w=c(1,5,3),x=c(2,3,2))
wt_mean(x=c(1,5,3),w=c(2,3,2))


# no good ...
wt_mean <- function(x, w, na.rm = FALSE) {
  if (!is.logical(na.rm)) {
    stop("`na.rm` must be logical")
  }
  if (length(na.rm) != 1) {
    stop("`na.rm` must be length 1")
  }
  if (length(x) != length(w)) {
    stop("`x` and `w` must be the same length", call. = FALSE)
  }
  
  if (na.rm) {
    miss <- is.na(x) | is.na(w)
    x <- x[!miss]
    w <- w[!miss]
  }
  sum(w * x) / sum(w)
}


# stopifnot 
wt_mean <- function(x, w, na.rm = FALSE) {
  stopifnot(is.logical(na.rm), length(na.rm) == 1)
  stopifnot(length(x) == length(w))
  
  if (na.rm) {
    miss <- is.na(x) | is.na(w)
    x <- x[!miss]
    w <- w[!miss]
  }
  sum(w * x) / sum(w)
}
wt_mean(1:6, 6:1, na.rm = "foo")


# dotdotdot ... -----------------------------------------------------------

# arbitary inputs
stringr::str_c("1","a","b","c")
sum(1,2,3,4,5,6,7)

commas <- function(...) {
  stringr::str_c(...,collapse=',')
}
letters[1:10]
commas(letters[1:10])

rule <- function(...,pad='-'){
  title <- paste(...)
  width <- getOption("width") - nchar(title) - 5
  cat(title," ",stringr::str_dup(pad,width),"\n",sep="")
}

rule("COOL STUFF","COOL","ALWAYS")
rule("Important","Output")

paste0("1","2","3")
stringr::str_c("1","2","3",sep=",")
stringr::str_c(letters," is for","...")

paste0(letters,' is for',"...") # str_c can handle NA properly, and give warn to different length


# lazy evaluation ---------------------------------------------------------


f <- function(x) {
  10
}
f(stop("this is error"))



add <- function(x) {
  function(y) x + y
}
adders <- lapply(1:10, add)
adders[[1]](10)



# write pipe-able function ------------------------------------------------


show_missings <- function(df) {
  n <- sum(is.na(df))
  cat("Missing values: ", n, "\n", sep = "")
  invisible(df)
}

df <- data.frame(a=c(1,2,3),b=c(2,3,NA))
show_missings(df)

x <- show_missings(mtcars)
class(x)

  
mtcars %>% 
  show_missings() %>% 
  mutate(mpg=ifelse(mpg<20,NA,mpg)) %>% 
  show_missings()


# Enviroment --------------------------------------------------------------

`+` <- function(x, y) {
  if (runif(1) < 0.1) {
    sum(x, y)
  } else {
    sum(x, y) * 1.1
  }
}
table(replicate(1000, 1 + 2))
#> 
#>   3 3.3 
#> 100 900
rm(`+`)
