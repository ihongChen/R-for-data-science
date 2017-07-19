
# data.table intro --------------------------------------------------------
library(data.table)
## 1. data.table stringAsFactor -> default False
t = data.table(a = LETTERS[1:3])
str(t)
t2 <- data.frame(a=LETTERS[1:3])
str(t2)

## 2. 

vars = data.frame(X = rnorm(3), Y = rnorm(3), Z = rnorm(3))
vars[,1:2]

vars_dt = data.table(vars)
vars_dt[,1:2]

## setDF
DT <- data.table(X = rnorm(3),Y = rnorm(3))
str(DT)
setDF(DT)
str(DT)

DT = data.table(X = rnorm(3), Y = rnorm(3))
tracemem(DT)
setDF(DT)
DF <- data.frame(DT)
retracemem(DF, retracemem(DT)) ## 記憶體位置改變(複製)



# From DataCamp -----------------------------------------------------------

### https://www.datacamp.com/courses/data-analysis-the-data-table-way
# 

DT <- data.table(A=1:6,B=c("a","b","c"),C=rnorm(6),D=T)
typeof(1L)

DT[3:5,] # DT[3:5]

DT
DT[i=c(1,3,5,5)]
DT[.N] # last row 
DT[.N-1] # second of last row

DT[,.(Total = sum(A),Mean = mean(C))]
DT[,.(B, C = sum(C))] # Recycleing in j
DT[, plot(A,C)]

class(DT[,C]) ## numeric vector
class(DT[,.(C)]) ## data.table

d <- 5

DT[,.(d)]

