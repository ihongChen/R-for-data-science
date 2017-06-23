
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
j <- list(name="Joe",salary=50000,union=T)
class(j) <- "employee"
attributes(j)

j


print.employee <- function(wrkr) {
  cat(wrkr$name,"\n")
  cat("salary",wrkr$salar,"\n")
  cat("union member",wrkr$union,"\n")
}
print(j)

methods(,"employee")
j


# inheritance -------------------------------------------------------------

k <- list(name="Kate",salary=68000,union=F,hrsthismonth= 2)
class(k) <- c("hrlyemployee","employee")

k


# environment --------------------------------------------------------------

ls()
e <- environment()
e
ls()
assign("budda",3,e)
budda
get("budda",e)


# create s3 class ---------------------------------------------------------

## stright forward approach
NorthAmerican <- function(eatsBreakfast=TRUE,myFavorite="cereal")
{
  
  me <- list(
    hasBreakfast = eatsBreakfast,
    favoriteBreakfast = myFavorite
  )
  
  ## Set the name for the class
  class(me) <- append(class(me),"NorthAmerican")
  return(me)
}

bubba
bubba$hasBreakfast
bubba$favoriteBreakfast

louise <- NorthAmerican(eatsBreakfast=TRUE,myFavorite="fried eggs")

## local env approach

NordAmericain <- function(eatsBreakfast=TRUE,myFavorite="cereal")
{
  
  ## Get the environment for this
  ## instance of the function.
  thisEnv <- environment()
  
  hasBreakfast <- eatsBreakfast
  favoriteBreakfast <- myFavorite
  
  ## Create the list used to represent an
  ## object for this class
  me <- list(
    
    ## Define the environment where this list is defined so
    ## that I can refer to it later.
    thisEnv = thisEnv,
    
    ## The Methods for this class normally go here but are discussed
    ## below. A simple placeholder is here to give you a teaser....
    getEnv = function()
    {
      return(get("thisEnv",thisEnv))
    }
    
  )
  
  ## Define the value of the list within the current environment.
  assign('this',me,envir=thisEnv)
  
  ## Set the name for the class
  class(me) <- append(class(me),"NordAmericain")
  return(me)
}

bubba <- NordAmericain()
get("hasBreakfast",bubba$getEnv())
get("favoriteBreakfast",bubba$getEnv())



#  Inheritance -----------------------------------------------------------




NorthAmerican <- function(eatsBreakfast=TRUE,myFavorite="cereal")
{
  
  me <- list(
    hasBreakfast = eatsBreakfast,
    favoriteBreakfast = myFavorite
  )
  
  ## Set the name for the class
  class(me) <- append(class(me),"NorthAmerican")
  return(me)
}



setHasBreakfast <- function(elObjeto, newValue)
{
  UseMethod("setHasBreakfast",elObjeto)
  print("Note this is not executed!")
}

setHasBreakfast.default <- function(elObjeto, newValue)
{
  print("You screwed up. I do not know how to handle this object.")
  return(elObjeto)
}


setHasBreakfast.NorthAmerican <- function(elObjeto, newValue)
{
  elObjeto$hasBreakfast <- newValue
  return(elObjeto)
}


getHasBreakfast <- function(elObjeto)
{
  UseMethod("getHasBreakfast",elObjeto)
  print("Note this is not executed!")
}

getHasBreakfast.default <- function(elObjeto)
{
  print("You screwed up. I do not know how to handle this object.")
  return(NULL)
}


getHasBreakfast.NorthAmerican <- function(elObjeto)
{
  return(elObjeto$hasBreakfast)
}


setFavoriteBreakfast <- function(elObjeto, newValue)
{
  UseMethod("setFavoriteBreakfast",elObjeto)
  print("Note this is not executed!")
}

setFavoriteBreakfast.default <- function(elObjeto, newValue)
{
  print("You screwed up. I do not know how to handle this object.")
  return(elObjeto)
}


setFavoriteBreakfast.NorthAmerican <- function(elObjeto, newValue)
{
  elObjeto$favoriteBreakfast <- newValue
  return(elObjeto)
}


getFavoriteBreakfast <- function(elObjeto)
{
  UseMethod("getFavoriteBreakfast",elObjeto)
}

getFavoriteBreakfast.default <- function(elObjeto)
{
  print("You screwed up. I do not know how to handle this object.")
  return(NULL)
}


getFavoriteBreakfast.NorthAmerican <- function(elObjeto)
{
  return(elObjeto$favoriteBreakfast)
}

bubba <- NorthAmerican()
bubba <- setHasBreakfast(bubba,FALSE)
bubba <- setFavoriteBreakfast(bubba,"Pork Belly")
getHasBreakfast(bubba)
getFavoriteBreakfast(bubba)


#####################################
## Now define the derived classes
Mexican <- function(eatsBreakfast=TRUE,myFavorite="los huevos")
{
  
  me <- NorthAmerican(eatsBreakfast,myFavorite)
  
  ## Add the name for the class
  class(me) <- append(class(me),"Mexican")
  return(me)
}


USAsian <- function(eatsBreakfast=TRUE,myFavorite="pork belly")
{
  
  me <- NorthAmerican(eatsBreakfast,myFavorite)
  
  ## Add the name for the class
  class(me) <- append(class(me),"USAsian")
  return(me)
}

Canadian <- function(eatsBreakfast=TRUE,myFavorite="back bacon")
{
  
  me <- NorthAmerican(eatsBreakfast,myFavorite)
  
  ## Add the name for the class
  class(me) <- append(class(me),"Canadian")
  return(me)
}

Anglophone <- function(eatsBreakfast=TRUE,myFavorite="pancakes")
{
  
  me <- Canadian(eatsBreakfast,myFavorite)
  
  ## Add the name for the class
  class(me) <- append(class(me),"Anglophone")
  return(me)
}

Francophone <- function(eatsBreakfast=TRUE,myFavorite="crepes")
{
  
  me <- Canadian(eatsBreakfast,myFavorite)
  
  ## Add the name for the class
  class(me) <- append(class(me),"Francophone")
  return(me)
}

makeBreakfast <- function(theObject)
{
  print("Calling the base makeBreakfast function")
  UseMethod("makeBreakfast",theObject)
}

makeBreakfast.default <- function(theObject)
{
  print(noquote(paste("Well, this is awkward. Just make",
                      getFavoriteBreakfast(theObject))))
  return(theObject)
}

makeBreakfast.Mexican <- function(theObject)
{
  print(noquote(paste("Estoy cocinando",
                      getFavoriteBreakfast(theObject))))
  NextMethod("makeBreakfast",theObject)
  return(theObject)
}

makeBreakfast.USAsian <- function(theObject)
{
  print(noquote(paste("Leave me alone I am making",
                      getFavoriteBreakfast(theObject))))
  NextMethod("makeBreakfast",theObject)
  return(theObject)
}

makeBreakfast.Canadian <- function(theObject)
{
  print(noquote(paste("Good morning, how would you like",
                      getFavoriteBreakfast(theObject))))
  NextMethod("makeBreakfast",theObject)
  return(theObject)
}

makeBreakfast.Anglophone <- function(theObject)
{
  print(noquote(paste("I hope it is okay that I am making",
                      getFavoriteBreakfast(theObject))))
  NextMethod("makeBreakfast",theObject)
  return(theObject)
}

makeBreakfast.Francophone <- function(theObject)
{
  print(noquote(paste("Je cuisine",
                      getFavoriteBreakfast(theObject))))
  NextMethod("makeBreakfast",theObject)
  return(theObject)
}


francois <- Francophone()
francois
francois <- makeBreakfast(francois)

# S3 ex -------------------------------------------------------------------

# 建立一個 S3 物件
ironmen_list <- list(
  group = c("Modern Web", "DevOps", "Cloud", "Big Data", "Security", "自我挑戰組"),
  participants = c(51, 8, 18, 14, 6, 64)
)
class(ironmen_list) <- "ironmen"

# 建立方法
count_participants <- function(obj) {
  UseMethod("count_participants")
}
count_participants.ironmen <- function(obj) {
  return(sum(obj$participants))
}

count_participants(ironmen_list)
