# tibbles 

# use tibbles instead of traditional data.frame in R

vignette("tibble")
library(tidyverse)

as_tibble(iris)

# 10.2 creating tibble ----------------------------------------------------
df1 = tibble(x=1:3,y=list(1:5,1:10,1:20))
df1$y

tibble(
  x = 1:5,
  y = 1,
  z = x^2 + y
)

tb <- tibble(
  `:)` = "smile",
  ` ` = "space",
  `2000` = "number"
)

# tribble :
tribble(
  ~x , ~y, ~z,
  # --|--| --
  "a",2,3.6,
  "b",1,8.5
)

# 10.3 tibbles vs data.frame ---------------------------------------------

tibble(
  a = lubridate::now() + runif(1e3) * 86400,
  b = lubridate::today() + runif(1e3) * 30,
  c = 1:1e3,
  d = runif(1e3),
  e = sample(letters,1e3,replace=T)
)
# print
nycflights13::flights %>% 
  print(n=10,width=Inf)

as_tibble(mtcars) %>% 
  print(n=3)

# 10.3.2 subsetting -------------------------------------------------------

df <- tibble(
  x= runif(5),
  y = rnorm(5)
)

# extract by name 
df$x
df[['x']]
# extract by position 
df[[1]]

# .  in pipeline
df %>% .$x
df %>% .[["x"]]

# 10.4. interacting with data.frame(older code) ----------------------------

class(as.data.frame(tb))


# 10.5 ex -----------------------------------------------------------------

# ex1:
class(mtcars)
class(as_tibble(mtcars))
# ex2 :
df <-data.frame(abc=1,xyz='a')
df[,"xyz"]
df[,c("abc","xyz")]

df_tbl <- as_tibble(df)
df_tbl
df_tbl[,"xyz"]
df_tbl[,c("abc","xyz")]

# vignette ----------------------------------------------------------------

vignette("tibble")

# no more stringAsFactors = FALSE
df <- data.frame(letters,stringsAsFactors = FALSE)
class(df$letters) # characters (not factor )
df_tbl <- tibble(x = letters)
class(df_tbl$x) # character

# list column
data.frame(x=1:3,y=list(1:5,1:10,1:20)) # not possible
tibble(x=1:3,y=list(1:5,1:10,1:20))

# never adjust the names of variables
names(data.frame(`crazy name` = 1)) # "crazy.name"
names(tibble(`crazy name`=1))#"carzy name"

# evaluate arg lazily and sequentially
tibble(x=1:30,y=x^2) 
options(tibble.print_min=15)
tibble(x=1:30,y=x^2) 

# dataframe vs tibble 
df1 <-data.frame(x=1:3,y=3:1)
class(df1[,1:2]) # data.frame
class(df1[,1]) # integer

df2 <- tibble(x=1:3,y=3:1)
class(df2[,1:2])
class(df2[,1])

df2[[1]]
class(df2$x)

# stricker
df <-data.frame(abc=1)
df$a
df2 <-tibble(abc=1)
df2$a # NULL, unknown column 'a'
