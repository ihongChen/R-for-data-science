
# iterator ----------------------------------------------------------------
## from wush blog: http://wush.ghost.io/itertools-intro/

library(itertools)

for (i in 1:3){
  print(i)
}

for (i in 1:3){
  for (j in 1:3){
    print(paste(i,j))
  }
}

# iterator ex1 ------------------------------------------------------------
## 雙層迴圈
it <- ihasNext(product(i=1:3,j=1:3))
while(hasNext(it)){
  x <- nextElem(it)
  print(paste(x$i,x$j))
}


result <- lapply(product(i=1:3,j=1:3),function(x){
  print(paste(x$i,x$j))
})


# ex2 .合併迴圈 ---------------------------------------------------------------

x <- 1:3  
y <- 4:6  
for(i in seq_along(x)) {  
  print(paste(x[i], y[i]))
}

it <- ihasNext(izip(x = 1:3, y = 4:6))  
while(hasNext(it)) {  
  x <- nextElem(it)
  print(paste(x$x, x$y))
}


# ex3. dataframe ---------------------------------------------------------

df <- iris[1:3,]  
for(i in seq_len(nrow(df))) {  
  x <- df[i,]
  print(paste(x$Sepal.Length, x$Sepal.Width, x$Petal.Length, x$Petal.Width, x$Species))
}

it <- ihasNext(iter(iris[1:3,], by = "row"))  
while(hasNext(it)) {  
  x <- nextElem(it)
  print(paste(x$Sepal.Length, x$Sepal.Width, x$Petal.Length, x$Petal.Width, x$Species))
}


# ex4. 截斷迴圈 ---------------------------------------------------------------

mkfinished <- function(time) {  
  starttime <- proc.time()[3]
  function() proc.time()[3] > starttime + time
}
f <- mkfinished(1) # 這是個函數，當時間比這個瞬間晚1秒時，f就會回傳FALSE, 迴圈會中止  
# 看看1秒內，迴圈可以跑多少
length(lapply(ibreak(iter(1:1000000), f), function(x) {  
  # do something
}))

it <- ihasNext(timeout(iter(1:1000000), 1))  
count <- 0  
while(hasNext(it)) {  
  x <- nextElem(it)
  count <- count + 1
}
count  


length(lapply(ilimit(iter(1:1000000), 100), function(x) {  
  # do something
}))



# ex5. 重複迴圈 --------------------------------------------------------------------

it <- ihasNext(recycle(iter(1:3), 2))  
while(hasNext(it)) {  
  x <- nextElem(it)
  print(x)
}

