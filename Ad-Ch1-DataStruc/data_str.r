######## Advanced Ch1 --- Data Structure ---- ##########
## 2.1 atomic vector ,
## 2.2 attributes 
## 2.3 array, matrix
## 2.4 data.frame 

## 2.1 atomic vector , ###
# typeof , length ,attributes

# 2.1.1 atomic vector

dbl_var <- c(1,2.5,4.5)
int_var <- c(1L,2L,3L)
typeof(dbl_var)
typeof(int_var)

cat(paste0('class of int_var : ',class(int_var))) ## integer
cat(paste0('type of int_var: ',typeof(int_var))) ## integer
cat(paste0('class of dbl_var : ',class(dbl_var))) ## numeric
cat(paste0('typeof dbl_var : ', typeof(dbl_var))) ## double
is.atomic(dbl_var) # T
is.atomic(int_var)  # T
is.double(dbl_var) # T

### is.numeric() is a general test for the “numberliness” of a vector and
## returns TRUE for both integer and double vectors


log_var <- c(T,T,F,F)
char_var <- c("there","these","hello")
c(1,c(1,2,c(3,4))) ## atomic vector is always flat 
is.character(log_var)



int_var <-c(1L,6L,10L)
typeof(int_var)
is.integer(int_var)

is.atomic(int_var)
is.double(dbl_var)
is.numeric(int_var)
is.numeric(dbl_var)
str(c("a",1))

### Corection #####
x<-c(F,F,T,T)
str(c("a",1)) # convert to char

as.numeric(x)
sum(x)
mean(x)

### 2.1.2 List #####
x<-list(1:3,"a",c(T,T,F,F,F),c(2.3,2.4))
str(x)

x<-list(list(list(list())))
str(x)
is.recursive(x)

x<-list(list(1,2),c(3,4))  ## 注意!! # c(3,4) combine to a vector
str(x)
length(x) # length = 2
is.recursive(x)

x1 <- list(c(3,4))

## c() will combine several lists into one.
y<-c(list(1,2),c(3,4))
str(y)
y

is.list(mtcars)
mtcars
str(mtcars)
attributes(mtcars)
c(NA_character_,1)

mod <- lm(mpg~wt,data=mtcars)
is.list(mod)


##### 2.2 Attribute #####
## attributes can be though as named list ##
y<-1:10
attr(y,"my_attr")<-"This is a vector"
y
attributes(y+2:11)
str(attributes(y))
# The structure() function returns a new object with modified attributes:
structure(1:10,my_attr="This is a vector too")
attributes(y[1])
attributes(y)
str(y)
attr(y,"my_attr")

## names 
x<-c(a=1,b=2,c=3)
names(x )

x <- 1:3 ;names(x)<-c('a','b','c')
x

y<- c(a=1,2,3)
y
names(y)
v <-c(1,2,3)
names(v) <-'a'
z <- c(1,2,3)
names(z)
unname(x)
names(x)<-NULL

##3## 2.2.1 Factors ######

x<- factor(c("a","b","c","c","b","a"))
x
class(x)
levels(x)
x[2] <- "d" ## invalid to modify factor not in level
x
sex_char <- c("m", "m", "m")
sex_factor <- factor(sex_char, levels = c("m", "f"))

table(sex_factor)

# Reading in "text" instead of from a file here:
z <- read.csv(text = "value\n12\n1\n.\n9")
typeof(z$value)
as.double(z$value)
class(z$value) ### oops ...factor !!

z<-read.csv(text="value\n12\n1\n.\n9",na.strings = '.')
typeof(z$value)
class(z$value)
z$value

temp <-structure(1:5, comment = "my attribute")
attributes(temp)
attr(temp,"comment")

f1<-factor(letters)
levels(f1)<-rev(levels(f1))
levels(f1) <- c(letters)
f1

###### 2.3 Matrices and Array #########

a<-matrix(1:6,ncol=3,nrow=2)
a
b<-array(1:12,c(2,3,2))
b[,,1]
c <-1:6
dim(c)<-c(3,2)
dim(c) <-c(1,2,3)
c
length(a)
nrow(a)
ncol(a)
rownames(a)<-c("A","B")
colnames(a)<-c('a','b','c')
a

length(b)
dim(b)
dimnames(b)<-list(c("one","two"),c("a","b","c"),c("A","B"))
b[,,1]

is.array(b)
str(1:3)

str(matrix(1:3,ncol = 1))
str(array(1:3,3))


l <- list(1:3, "a", TRUE, 1.0)
dim(l) <- c(2, 2)

tempa<-c(1,2,3)
dim(tempa) <-c(3)
str(tempa)

x1 <- array(1:5, c(1, 1, 5))
x2 <- array(1:5, c(1, 5, 1))
x3 <- array(1:5, c(5, 1, 1))

