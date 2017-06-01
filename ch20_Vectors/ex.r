set.seed(100);
dt = data.table(v1=rpois(5,3),
                v2= sample(c("g1","g2"),5,1),v3=rnorm(5))
dt

dt %>% arrange(desc(v1),v2,v3)

dt %>% filter(v1<=1,v3<0)

dt %>% mutate(v5=v1*v3,v6=substr(v2,2,2),
              v7=round(v3),v3=round(v3**2))

dt %>% select(v1,v2)

(dt %>% group_by(v2) %>% 
  summarise(size_g = n(),mean_v3 = mean(v3),sum_v1 = sum(v1))) %>% tbl_dt(FALSE)
dt
print(dt,n=2)

dt %>% select(starts_with("v"))
dt %>% select(ends_with("1"))
dt %>% select(contains("2"))
dt %>% select(matches("\\w\\d"))
dt %>% select(num_range("v",1:2))


dat = data.frame(A = 1:5, row.names = paste0("City_", LETTERS[1:5]))

as_data_frame()


`+` <- function(x, y) {
  if (runif(1) < 0.1) {
    sum(x, y)
  } else {
    sum(x, y) * 1.1
  }
}
table(replicate(1000,1+2))
letters
typeof(letters)
class(letters)
x<- list("1","b","d",1)
typeof(x)
class(x)
x<- list("a","b",1:10)
x
typeof(x)
length(x)

1:10 %%3 ==0
c(T,T,F,NA)
typeof(1) 
class(1)

typeof(1L)
x <-sqrt(2)^2
x
x-2
typeof(x)
dplyr::near(x,2)

c(-1,0,1) /0
is.finite(0)
is.infinite(Inf)
is.na(NA)
is.nan(NA)
is.na(NaN)

pryr::object_size(x)
y <- rep(x,1000)
pryr::object_size(y)
typeof(NA)
# class(NA)

runif(10)
tibble(x=1:4,y=1:2)
tibble(x=1:4,y=rep(1:2,2))
tibble(x=1:4,y=rep(1:2,each=2))
set_names(1:3,c("a","b","c"))

x <- c("one", "two", "three", "four", "five")
x[c(3, 2, 5)]
typeof(x)
x[c(1,5,5,2,2)]

x<-c(10,3,NA,5,8,1,NA)
x[!is.na(x)]
x <-c(abc=1,def=2,xyz=5)
x  
is.vector(x)
setNames()
set_names()

set_names(letters[1:5])
set_names(1:4,letters[1:4])
x<-rnorm(10)
x[which(x<0)]
x[x<=0]

x <-list(1:10,2,c(3,4,5),rep("a",100))
str(x)
x_named <- list(a=1,b=2,c=3)
x_named
str(x_named)
z <- list(list(1,2),list(3,4))
z
str(z)
z[[1]][[1]]

x1<-list(c(1,2),c(3,4))
x1

x<-1:10
attr(x,"greeting")
attr(x,"greeting") <- "Hi!!"
attr(x,"farewell") <-"ByeBye!!"
x
setNames(x,letters[1:10])
set_names(x,letters[1:10])

as.Date
methods("as.Date")

getS3method("as.Date","default")
getS3method("as.Date","numeric")
getS3method("as.Date","character")

x <- factor(c("ab", "cd", "ab"), levels = c("ab", "cd", "ef"))
typeof(x)
class(x)

attributes(x)
str(x)

x<- as.Date()

sim <- tribble(
  ~f,      ~params,
  "runif", list(min = -1, max = 1),
  "rnorm", list(sd = 5),
  "rpois", list(lambda = 10)
)
sim %>% 
  mutate(sim = invoke_map(f, params, n = 10))
