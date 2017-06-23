
# OO field ----------------------------------------------------------------


# install.packages("pryr")

f <- function(){}
typeof(f)
is.function(f)
typeof(sum)
is.function(sum)
is.primitive(sum)
is.object(sum) ## check S3,S4,RC or base (False)


# S3 ----------------------------------------------------------------------

## the simplest OO system

library(pryr)
df <- data.frame(x=1:10,y=letters[1:10])
otype(df)

otype(df$x) # int is base
otype(df$y) # factor is S3

### s3, method belong to generic function ###
ftype(mean) # s3, generic
ftype(t.data.frame) # s3,method, data frame method for t
ftype(t.test) # generic function for t test
?methods

methods("mean")
methods(class="ts")

## Define classes and creating objects ##

# create and assign in one step
foo <-structure(list(),class="foo")
attributes(foo)

foo<- list()
class(foo) <- "foo"

# S3 objects are usually built on top of lists, or atomic vectors with attributes
class(foo)
inherits(foo,"foo")
# most s3 classes provide a constructor function
foo <- function(x){
  if (!is.numeric(x)) stop("X must be numeric")
  structure(list(x),class="foo")
}

foo_Object <- foo(c(1L,2L,3L,4L,5L))
str(foo_Object)

## S3 has no check for correctness...!!! ##
mod<-lm(log(mpg)~log(disp),data=mtcars)
class(mod)
print(mod)
class(mod) <- "data.frame"
print(mod)
mod$coefficients # data still here 


#### Creating new Methods and generics #####

# 
f <- function(x) UseMethod("f")
f.a <- function(x) "Class a"
a <- structure(list(),class="a")
f(a)

mean.a <- function(x) "a"
mean(a)


#### Method dispatch #####
f = function(x) UseMethod("f")

f.a <- function(x) "Class a"
f.default <- function(x) "Unknown class"

a <- structure(list(),class="a")
b <- structure(list(),class=c("a","b"))
c <- structure(list(),class="c")
f(b)
f(c)

# Because methods are normal R functions, you can call them directly:
c <- structure(list,class="c")
f.default(c)
f(c)

# Force R to call the wrong method
f.a(c)

## You can also call an S3 generic with a non-S3 object. Non-internal S3 generics will dispatch on the implicit class of base types. (Internal generics don’t do that for performance reasons.) 
#  The rules to determine the implicit class of a base type are somewhat complex,
## but are shown in the function below: 

iclass <- function(x){
  if (is.object(x)){
    stop("x is not a primitive type",call. = F)
  }
  
  c(
    if (is.matrix(x)) "matrix",
    if (is.array(x) && !is.matrix(x)) "array",
    if (is.double(x)) "double",
    if (is.integer(x))"integer",
    mode(x)
  )
}
iclass(matrix(1:5))
iclass(array(1.5))


# S4 ----------------------------------------------------------------------

## identify S4 ##

library(stats4)

isS4(fit)

# Defining classes and creating objects

class?className

setClass("Person",
         slots = list(name="character",
                      age = "numeric"))
setClass("Emploee",
         slots =list(boss="Person"),
         contains = "Person")

alice <- new("Person",name="Alice",age=40)
john <- new("Emploee",name="John",age=20,boss=alice)

alice@age
john@boss
slot(john,"boss")

## If an S4 object contains (inherits from) 
## an S3 class or a base type, 
## it will have a special .Data slot which contains the underlying base type or S3 object: 

setClass("RangedNumeric",
         contains="numeric",
         slots = list(min="numeric",max="numeric"))

rn <- new("RangedNumeric",1:10,min=1,max=10)
rn@min
rn@.Data

### Creating new methods and generics ###
setGeneric("union")
setMethod("union",
          c(x="data.frame",y="data.frame"),
          function(x,y){
            unique(rbind(x,y))
          })

setGeneric("myGeneric",function(x){
  standardGeneric("myGeneric")
})



# S4 ex -------------------------------------------------------------------
## From :http://www.cyclismo.org/tutorial/R/s4Classes.html

######################################################################
# Create the first quadrant class
#
# This is used to represent a coordinate in the first quadrant.
FirstQuadrant <- setClass(
  # Set the name for the class
  "FirstQuadrant",
  
  # Define the slots
  slots = c(
    x = "numeric",
    y = "numeric"
  ),
  
  # Set the default values for the slots. (optional)
  prototype=list(
    x = 0.0,
    y = 0.0
  ),
  
  # Make a function that can test to see if the data is consistent.
  # This is not called if you have an initialize function defined!
  validity=function(object)
  {
    if((object@x < 0) || (object@y < 0)) {
      return("A negative number for one of the coordinates was given.")
    }
    return(TRUE)
  }
)
x<-FirstQuadrant()
isS4(x)

y <- FirstQuadrant(x=5,y=7)
y@x

z <- FirstQuadrant(x=3,y=-2)
z

### create method ##
# create a method to assign the value of a coordinate
setGeneric(name="setCoordinate",
           def = function(theObject,xVal,yVal){
             standardGeneric("setCoordinate")
           })

setMethod(f="setCoordinate",
          signature = "FirstQuadrant",
          definition = function(theObject,xVal,yVal){
            theObject@x <- xVal
            theObject@y <- yVal
            return(theObject)
          })

z <- FirstQuadrant(x=2.5,y=10)
z <- setCoordinate(z,-3,-5)
z
z@x


###### Create S4 Class ######
######################################################################
# Create the base Agent class
#
# This is used to represent the most basic agent in a simulation.
Agents <- setClass(
  # Set the name for the class
  "Agent",
  
  # Define the slots
  slots = c(
    location = "numeric",
    velocity   = "numeric",
    active   = "logical"
  ),
  
  # Set the default values for the slots. (optional)
  prototype=list(
    location = c(0,0),
    velocity = c(0,0),
    active   = TRUE
  ),
  
  # Make a function that can test to see if the data is consistent.
  # This is not called if you have an initialize function defined!
  validity=function(object)
  {
    if(sum(object@velocity^2)>100.0) {
      return("The velocity level is out of bounds.")
    }
    return(TRUE)
  }
)
a <- Agents()
is.object(a)
isS4(a)

slotNames(a)
slotNames("Agent")
getSlots("Agent")
s <- getSlots("Agent")
s[[1]]

###
getClass(a)
getClass("Agent")

slot(a,"location")
a@location

slot(a,"location") <- c(1,5)
a@location <- c(1,3)


###### Creating Methods ######

# create a method to assign the value of the location
setGeneric(name="setLocation",
           def=function(theObject,position)
           {
             standardGeneric("setLocation")
           }
)

setMethod(f="setLocation",
          signature="Agent",
          definition=function(theObject,position)
          {
            theObject@location <- position
            validObject(theObject)
            return(theObject)
          }
)

# create a method to get the value of the location
setGeneric(name="getLocation",
           def=function(theObject)
           {
             standardGeneric("getLocation")
           }
)

setMethod(f="getLocation",
          signature="Agent",
          definition=function(theObject)
          {
            return(theObject@location)
          }
)


# create a method to assign the value of active
setGeneric(name="setActive",
           def=function(theObject,active)
           {
             standardGeneric("setActive")
           }
)

setMethod(f="setActive",
          signature="Agent",
          definition=function(theObject,active)
          {
            theObject@active <- active
            validObject(theObject)
            return(theObject)
          }
)

# create a method to get the value of active
setGeneric(name="getActive",
           def=function(theObject)
           {
             standardGeneric("getActive")
           }
)

setMethod(f="getActive",
          signature="Agent",
          definition=function(theObject)
          {
            return(theObject@active)
          }
)


# create a method to assign the value of velocity
setGeneric(name="setVelocity",
           def=function(theObject,velocity)
           {
             standardGeneric("setVelocity")
           }
)

setMethod(f="setVelocity",
          signature="Agent",
          definition=function(theObject,velocity)
          {
            theObject@velocity <- velocity
            validObject(theObject)
            return(theObject)
          }
)

# create a method to get the value of the velocity
setGeneric(name="getVelocity",
           def=function(theObject)
           {
             standardGeneric("getVelocity")
           }
)

setMethod(f="getVelocity",
          signature="Agent",
          definition=function(theObject)
          {
            return(theObject@velocity)
          }
)


# create a method to reset the velocity and the activity
setGeneric(name="resetActivity",
           def=function(theObject,value)
           {
             standardGeneric("resetActivity")
           }
)

setMethod(f="resetActivity",
          signature=c("Agent","logical"),
          definition=function(theObject,value)
          {
            theObject <- setActive(theObject,value)
            theObject <- setVelocity(theObject,c(0.0,0.0))
            return(theObject)
          }
)

setMethod(f="resetActivity",
          signature=c("Agent","numeric"),
          definition=function(theObject,value)
          {
            theObject <- setActive(theObject,TRUE)
            theObject <- setVelocity(theObject,value)
            return(theObject)
          }
)

a <- Agents()
a
a <- resetActivity(a,F)
a <- resetActivity(a,c(1,3))
getVelocity(a)


######################################################################
# Inheritance 
###################################################################


######################################################################
# Create the Prey class
#
# This is used to represent a prey animal
Prey <- setClass(
  # Set the name for the class
  "Prey",
  
  # Define the slots - in this case it is empty...
  slots = character(0),
  
  # Set the default values for the slots. (optional)
  prototype=list(),
  
  # Make a function that can test to see if the data is consistent.
  # This is not called if you have an initialize function defined!
  validity=function(object)
  {
    if(sum(object@velocity^2)>70.0) {
      return("The velocity level is out of bounds.")
    }
    return(TRUE)
  },
  
  # Set the inheritance for this class
  contains = "Agent"
)



######################################################################
# Create the Bobcat class
#
# This is used to represent a smaller predator
Bobcat <- setClass(
  # Set the name for the class
  "Bobcat",
  
  # Define the slots - in this case it is empty...
  slots = character(0),
  
  # Set the default values for the slots. (optional)
  prototype=list(),
  
  # Make a function that can test to see if the data is consistent.
  # This is not called if you have an initialize function defined!
  validity=function(object)
  {
    if(sum(object@velocity^2)>85.0) {
      return("The velocity level is out of bounds.")
    }
    return(TRUE)
  },
  
  # Set the inheritance for this class
  contains = "Agent"
)

######################################################################
# Create the Lynx class
#
# This is used to represent a larger predator
Lynx <- setClass(
  # Set the name for the class
  "Lynx",
  
  # Define the slots - in this case it is empty...
  slots = character(0),
  
  # Set the default values for the slots. (optional)
  prototype=list(),
  
  # Make a function that can test to see if the data is consistent.
  # This is not called if you have an initialize function defined!
  validity=function(object)
  {
    if(sum(object@velocity^2)>95.0) {
      return("The velocity level is out of bounds.")
    }
    return(TRUE)
  },
  
  # Set the inheritance for this class
  contains = "Bobcat"
)


# create a method to move the agent.
setGeneric(name="move",
           def=function(theObject)
           {
             standardGeneric("move")
           }
)

setMethod(f="move",
          signature="Agent",
          definition=function(theObject)
          {
            print("Move this Agent dude")
            theObject <- setVelocity(theObject,c(1,2))
            validObject(theObject)
            return(theObject)
          }
)

setMethod(f="move",
          signature="Prey",
          definition=function(theObject)
          {
            print("Check this Prey before moving this dude")
            theObject <- callNextMethod(theObject)
            print("Move this Prey dude")
            validObject(theObject)
            return(theObject)
          }
)

setMethod(f="move",
          signature="Bobcat",
          definition=function(theObject)
          {
            print("Check this Bobcat before moving this dude")
            theObject <- setLocation(theObject,c(2,3))
            theObject <- callNextMethod(theObject)
            print("Move this Bobcat dude")
            validObject(theObject)
            return(theObject)
          }
)

setMethod(f="move",
          signature="Lynx",
          definition=function(theObject)
          {
            print("Check this Lynx before moving this dude")
            theObject <- setActive(theObject,FALSE)
            theObject <- callNextMethod(theObject)
            print("Move this Lynx dude")
            validObject(theObject)
            return(theObject)
          }
)

robert <- Bobcat()
robert <- move(robert)
robert

linoel <- Lynx()
linoel
linoel <- move(linoel)
linoel

# Generic function  -------------------------------------------------------
### from https://cos.name/2009/07/studying-notes-on-oop-in-r/

whoAmI <- function(x,...) UseMethod("whoAmI")
whoAmI.foo <- function(x) print("I am foo")
whoAmI.bar <- function(x) print("I am bar")
whoAmI.default <- function(x) print(' I don\'t know who am I.')

a <- 1:20 
whoAmI(a)
attr(a,"class")<-"bar"
whoAmI(a)
attr(a,"class") <- c("baz","bam","bar")
whoAmI(a)
class(a)


## 
whatIs <- function(obj) data.class(obj)
whatIs(1:10)
whatIs(matrix())
whatIs(whatIs)

whatIs <- function(obj) cat('類:',data.class(obj),"\n長度:",length(obj),"\n")
whatIs(1:10)
whatIs(matrix())
whatIs(whatIs)

A <- matrix(1:6,c(2,3))
whatIs(A)

whatIs.function <- function(obj) cat("類:",data.class(obj),"\n")
whatIs.function(whatIs)

whatIs.matrix <- function(obj) cat("類:",data.class(obj),
                                   "\n",nrow(obj),"行\n",ncol(obj),"列")
whatIs.matrix(A)

setMethod("whatIs","function",whatIs.function)
setMethod("whatIs","matrix",whatIs.matrix)

whatIs(1:20)
whatIs(A)
