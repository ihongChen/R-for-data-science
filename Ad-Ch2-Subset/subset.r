
# $ Dollar sign -----------------------------------------------------------

mtcars[['cyl']]
mtcars$cyl

x<-list(abc=1)
x$a ## partial mapping

#### out of bound ####
x<-1:4
str(x[5])
str(x[NA_real_])
str(x[NULL])

mod <- lm(mpg~wt,data=mtcars)
summary(mod)

#### subseting and assignment ###
x<-1:5
x[c(1,2)] <- 2:3
x[-1] <- 4:1
x
x[c(1,1)]<-2:3
x
x[c(1,1)] <-2:3
x[c(T,F,NA)]
x[c(T,F,NA)]<- 1
x

df <- data.frame(a=c(1,10,NA))
df$a[df$a<5] <- 0

mtcars1 <- lapply(mtcars,as.integer)
mtcars[] <- lapply(mtcars,as.integer) ## keep original dataframe format
# sapply(mtcars,as.integer) 
mtcars

x<- list(a=1,b=2)
x
x[['b']] <- NULL
str(x)

y<-list(a=1)
y['b'] <- list(NULL)
y
str(y)
str(x)


# .4 application ----------------------------------------------------------

### lookup table ###
x <- c("m", "f", "u", "f", "f", "m", "m")
lookup <- c(m = "Male", f = "Female", u = NA)
lookup[x]
unname(lookup[x])
c(m="Known",f="Known",u="Unknown")[x]

grades <- c(1, 2, 2, 3, 1)
info <- data.frame(
  grade = 3:1,
  desc = c("Excellent", "Good", "Poor"),
  fail = c(F,F,T)
)
info
ids<-match(grades,info$grade)# compare w. match(info$grade,grades)
info[ids,]


# Using rowname 
rownames(info) <- info$grade
info[as.character(grades),]

##### random sample/bootstrap ####
df <- data.frame(x = rep(1:3, each = 2), y = 6:1, z = letters[1:6])
df[sample(nrow(df)),]
df[sample(nrow(df),3),] ## select 3 random row 
df[sample(nrow(df),6,rep=T),]# Select 6 bootstrap replicates

#### ordering ####
x<-c('b','c','a')
order(x)
x[order(x)]

df2<-df[sample(nrow(df)),3:1]
df2
df2[order(df2$x),]
df2[,order(names(df2))]


#### Expanding aggregated count ####
df <-data.frame(x=c(2,4,1),y=c(9,11,6),
           n=c(3,5,1))
df[rep(1:nrow(df),df$n),]

##### Removing columns from dataframe ###

df <- data.frame(x=1:3,y=3:1,z=letters[1:3])
df$z <- NULL
df[c("x","y")]
df[setdiff(names(df),"z")]

##### select rows based on logic ####
mtcars[mtcars$gear==5,]
mtcars[mtcar1$gear==5 & mtcars$cyl==4,]

####
subset(mtcars,gear==5,select = c('cyl','gear'))
subset(mtcars,gear==5 & cyl==4)

##### Boolean algebra vs sets ####

x<-sample(10) <4
which(x)

rep_len(F,5)
rep(F,5)

unwhich <- function(x,n){
  out<-rep_len(F,n)
  out[x]<-T
  out
}
unwhich(c(3,4),20)

x1<-1:10 %%2 ==0
x1
x2<-which(x1)
y1 <- 1:10 %%5 ==0
y2<-which(y1)

x1 & x2
intersect(x2,y2)

x1|y1 # union 
x1& !y1

setdiff(x2,y2) ## x2:2,4,6,8; y2:5,10
xor(x1,y1)

####
df[sample(nrow(df)),sample(ncol(df))]

df[1,1,drop=F]
