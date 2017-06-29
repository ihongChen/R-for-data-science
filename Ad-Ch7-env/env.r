
# basic -------------------------------------------------------------------

e <- new.env()
e$a <- FALSE
e$b <- "a"
e$c <- 2.3
e$d <- 1:3

e$b <- e$d
e$b

search()
as.environment("package:stats")

## observe env

parent.env(e)
ls(e)
ls(e,all.names=T)
ls.str(e)
# str(e)
e$c
e[['c']]

get("c",envir = e)

## remove 
e <- new.env()
e$a <- 1
e$a <- NULL
ls(e)
rm("a",envir = e)

## exist
x <- 10
exists("x",envir = e)
exists("x", envir = e, inherits = FALSE)
identical(globalenv(), environment())
environment() == globalenv()


# Recursive over env ------------------------------------------------------

library(pryr)
x <- 5
where("x")

where("mean")

#...... not finished ..... #


# Function env ------------------------------------------------------------
# enclosing env
y <- 1
f <- function(x) x + y
environment(f)

# binding ,
e <- new.env()
e$g <- function() 1

#> <environment: R_GlobalEnv>
# execution
# calling 

environment(sd)
where("sd")
