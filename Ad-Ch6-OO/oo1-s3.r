
# OO - S3 -----------------------------------------------------------------

## from : http://www.cyclismo.org/tutorial/R/s3Classes.html




# 1.1 Basic idea ----------------------------------------------------------

bubba <- c(1,2,3)
bubba
class(bubba)

class(bubba) <- append(class(bubba),"Flamboyancy")
class(bubba)

bubba <- list(first="one",second="two",third="third")
class(bubba) <- append(class(bubba),"Flamboyancy")
str(bubba)

GetFirst <- function(x){
  UseMethod("GetFirst",x)
}

GetFirst.Flamboyancy <- function(x){
  return(x$first)
}

GetFirst(bubba)



#### 
