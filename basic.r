
# basic R- --http://blog.fens.me/rhadoop-r-basic/ -------------------------



seq(1:10)
assign('b',seq(1:10))
b
a <-1:10000
b <-10000:1
run1 <- function(){
  sum(as.numeric(a+b))
}

run2 <- function(){
  c2<-0
  for(i in 1:length(a)){
    c2<-a[i] + b[i] + c2
  }
  c2
}
system.time(run1())
system.time(run2())
ls()
ls(pattern="^sum",envir = baseenv())

x<-c(10.4,1.3,2.3,2,1,1,1)
x

c(1,2,3,4) -> x
x

labs <- paste0(c("x","y"),sep="")
paste(x,"y",sep="---")
paste0(x,"y",sep="000")

fruit <- c(5,10,15)
names(fruit) <- c("orange","apple","pineapple")
fruit['orange']

lunch <- fruit[c("apple","pineapple")]
prod(1:7) # factorial 
prod(10:1)
# system.time(prod(1:1e7)) # take 1.6 sec 

z<- c(1:3,NA)
is.na(z)
z1 <- 1+3i
mode(z) # numeric
mode(z1) #complex

class(z) #integer

attr(z,"name") <- "abc"
attributes(z)


state <- c("tas","sa","qlt","sa","nt","nsw")
statef <- factor(state)
levels(statef)
as.factor(state)

### 
incomes <- c(60,49,40,61,40,60)
incmeans <- tapply(incomes,statef,mean)

a<- 1:5;
inc <- tibble(state,incomes)
inc %>% 
  group_by(state) %>% 
  summarise(mean=mean(incomes))
  
####

z<-seq(1,1500)
dim(z)<-c(3,5,100)
z
attributes(z)

h <-seq(1,24)
h
Z<-array(h,dim=c(3,4,2)) # eq. dim(Z) <-c(3,4,2)
Z 

#### matrix 
A = matrix(seq(1:9),nrow=3,ncol=3, 
           dimnames = list(c("row1","row2","row3"),
                           c("C1","C2","C3")))
B = A
A * B
A %*% B
eigen(A)

#### Json #### 
install.packages("rjson")
library(rjson)

paste(readLines("jsonTest.json"),collapse = "")
json_data1<-fromJSON(paste(readLines("jsonTest.json"),collapse = ""))
json_data1

json_data1$table1$data$code
## to json
json_str <- toJSON(json_data1)
print(toJSON(json_data1))
cat(toJSON(json_data1))

# writeLines 
writeLines(json_str,"jsonTest2.json")
