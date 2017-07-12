
# 稀疏矩陣相似性 -----------------------------------------------------------------
library(pryr)
## sparse matrix 
library(Matrix)

nx <- 1e4
ny <- 1e3
nn <- 3000
i <- sample(1:nx,nn,replace = T)
j <- sample(1:ny,nn,replace = T)
x <- sample(1,nn,replace=T)
m <- sparseMatrix(i=i,j=j,x=x)

n <- sparseMatrix(i=j,j=i,x=x)

m@x
## jaccard 0 -- original matrix ##
jaccard_similarity0 <- function(x,y){
  
  a <- tcrossprod(x,y)
  
  nx <- nrow(x)
  ny <- nrow(y)
  
  c <- matrix(rowSums(x), nrow = nx, ncol = ny) - a
  b <- Matrix(rowSums(y), nrow = nx, ncol = ny, byrow = TRUE) - a
  
  sim <- a / (a + b + c)
  diag(sim) <- 0
  sim <- replace(sim,is.na(sim),0)
  sim
}

## jaccard 1 -- 中間有大矩陣產生

jaccard_similarity1 <- function(x,y){

  a <- tcrossprod(x,y)
  
  nx <- nrow(x)
  ny <- nrow(y)
  
  c <- Matrix(rowSums(x), nrow = nx, ncol = ny, sparse = TRUE) - a
  b <- Matrix(rowSums(y), nrow = nx, ncol = ny, byrow = TRUE, sparse = TRUE) - a
  
  sim <- a / (a + b + c)
  diag(sim) <- 0
  sim <- replace(sim,is.na(sim),0)
  sim <- drop0(Matrix(sim,sparse=T))
  sim
}
#### jaccard 2 -- 稀疏矩陣產生
jaccard_similarity2 <- function(x,y){
  
  a <- tcrossprod(x,y)
  im <- which(a>0,arr.ind=T)
  b <- rowSums(x)
  c <- rowSums(y)
  
  nx <- nrow(x)
  ny <- nrow(y)
  aim <- a[im]
  
  sim <- sparseMatrix(
    i = im[,1],
    j = im[,2],
    x = aim / (b[im[,1]] + c[im[,2]] - aim)
  )
  diag(sim) <- 0
  sim <- drop0(sim)
  
  sim
}


system.time(
  sim1 <- jaccard_similarity1(n,n)  
)

system.time(
  sim2 <- jaccard_similarity2(n,n)
)

identical(sim1[1:10,1:10],sim2[1:10,1:10])

# 檢查 ubcf_sparse 算法變慢原因? --------------------------------------------------
train_data <- rb_use[1:10000,]
newdata <- rb_use[1001:2000,]


system.time(
  sim1 <- jaccard_similarity1(as(newdata,"matrix"),
                             as(train_data,"matrix"))  
)

system.time(
  sim2 <- jaccard_similarity2(as(newdata,"dgCMatrix"),
                             as(train_data,"dgCMatrix"))  
)



system.time(
  neighbor1 <- .knn(sim1,20)
)
system.time(
  neighbor2 <- .knn(sim2,20)
)
object_size(neighbor1)
object_size(neighbor2)

system.time(
  s_uk1 <- sapply(1:nrow(sim1),function(x) sim1[x,neighbor1[,x]])
)
system.time(
  s_uk2 <- sapply(1:nrow(sim2),function(x) sim2[x,neighbor2[,x]])
)

system.time(sum_s_uk1 <- colSums(s_uk1,na.rm=T))
system.time(sum_s_uk2 <- colSums(s_uk2,na.rm=T))

neighbor2[,11]

drop(crossprod(neighbor2[,1],s_uk2[,1]))


## train , new data( predict data)
r_neighbors <- (as(train_data[neighbor1[,1]],"dgCMatrix"))

score1 <- drop(crossprod(r_neighbors,s_uk1[,1]))
score1[order(score1,decreasing = T)][1:10]

system.time(
  r_a_norms1 <- sapply(1:nrow(newdata),function(i){
    r_neighbors <- as(train_data[neighbor1[,i]],"dgCMatrix")
    drop(crossprod(r_neighbors,s_uk1[,i]))
  })
)


system.time(
  ## 2000 * new data users 
  r_a_norms2 <- sapply(1:nrow(newdata),function(i){
    r_neighbors <- as(train_data[neighbor2[,i]],"dgCMatrix")
    drop(crossprod(r_neighbors,s_uk2[,i]))
  })
)

r_neighbors <- (as(train_data[neighbor1[,1]],"dgCMatrix"))
r_neighbors[,6]

ratings1 <- t(r_a_norms1)/sum_s_uk1
ratings1 <- new("realRatingMatrix",data=dropNA(ratings1))

ratings2 <- t(r_a_norms2)/sum_s_uk2
ratings2 <- new("realRatingMatrix",data=dropNA(ratings2))




### time weighted similarity with neighbors

t1 <- proc.time()
neighbor1 <- .knn(sim1 ,20)

s_uk1 <- sapply(1:nrow(sim1), function(x) sim1[x,neighbor1[,x]])
sum_s_uk1 <- colSums(s_uk1,na.rm=T)
r_a_norms1 <- sapply(1:nrow(newdata),function(i){
  r_neighbors <- as(train_data[neighbor1[,i]],"dgCMatrix")
  drop(crossprod(r_neighbors,s_uk1[,i]))
})
ratings1 <- t(r_a_norms1)/sum_s_uk1
ratings1 <- new("realRatingMatrix",data=dropNA(ratings1))
t2 <- proc.time()
cat('proc time(s):',(t2-t1)[3])

##### method 2 ####

t1 <- proc.time()

neighbor2 <- .knn(sim2 ,20)
s_uk2 <- sapply(1:nrow(sim2), function(x) sim[x,neighbor2[,x]])
sum_s_uk2 <- colSums(s_uk2,na.rm=T)

r_a_norms2 <- sapply(1:nrow(newdata),function(i){
  r_neighbors <- as(train_data[neighbor2[,i]],"dgCMatrix")
  drop(crossprod(r_neighbors,s_uk2[,i]))
})

r_a_norms2 <- Matrix(r_a_norms2,sparse=T)

ratings2 <- t(r_a_norms2)/sum_s_uk2
ratings2 <- new("realRatingMatrix",data=ratings2)
t2 <- proc.time()
cat('proc time(s):',(t2-t1)[3])


# func --------------------------------------------------------------------

## similarity with of the neighbors
nn <- 20
build_nn_sim <- function(sim,new_data,train_data){
  t1 <- proc.time()
  
  neighbors <- .knn(sim, nn)
  t2 <- proc.time()
  cat('\nbuild neighbors(s):',(t2-t1)[3])
  ### Process SLOW !!!!! ###
  # 1. solution: convert to base matrix 
  tempBigMatrix <- as(sim,"matrix")
  s_uk <- vapply(1:nrow(sim), FUN=function(x)
    tempBigMatrix[x, neighbors[,x]],
    numeric(nn) ## numeric length of model$nn
  )
  rm(tempBigMatrix)
  # 2. solution: convert to dataframe and manuplate it ..
  sim_idx <- summary(sim) 
  
  
  
  t3 <- proc.time()
  cat('\nbuild s_uk(s):',(t3-t2)[3])
  
  
  sum_s_uk <- colSums(s_uk, na.rm=TRUE)
  
  ## calculate the weighted sum
  r_a_norms <- vapply(1:nrow(new_data), FUN=function(i) {
    ## neighbors ratings of active user i
    r_neighbors <- as(train_data[neighbors[,i]], "dgCMatrix")
    
    drop(crossprod(r_neighbors, s_uk[,i]))
  },
  numeric(ncol(new_data))
  )
  t4 <- proc.time()
  cat('\nbuild r_a_norms (s):',(t4-t3)[3])
  r_a_norms <- Matrix(r_a_norms, sparse = T)
  ratings <- t(r_a_norms)/sum_s_uk
  t5 <- proc.time()
  cat('\nbuild sparse matrix and rating',(t5-t4)[3])
  cat('\n')
  return(ratings)
}

new_data <- getData(eval_sets,"known")

sim0 <- similarity(new_data,train_data,method='jaccard')

sim1 <- jaccard_similarity1(as(new_data,"dgCMatrix"),
                           as(train_data,"dgCMatrix"))

sim2 <- jaccard_similarity2(as(new_data,"dgCMatrix"),
                            as(train_data,"dgCMatrix"))


# sim1_big <- jaccard_similarity1(as(rb_use,"dgCMatrix"),as(rb_use,"dgCMatrix"))
# sim2_big <- jaccard_similarity2(as(rb_use,"dgCMatrix"),as(rb_use,"dgCMatrix"))


system.time(
  ans0 <- build_nn_sim(sim0,new_data,train_data)
)

system.time(
  ans2 <- build_nn_sim(sim2,new_data,train_data)
)




# sparse matrix manuplate -------------------------------------------------

library(Matrix)
library(pryr)

nx <- 1e4
ny <- 1e4
nn <- 1e4
# i <- sample(1:nx,nn,replace = T)
# j <- sample(1:ny,nn,replace = T)
# x <- runif(nn)
# m <- sparseMatrix(i=i,j=j,x=x)
# i
# object_size(m)
# dim(m)

knn <- function(M){
  head(M[order(M,decreasing = T)],20)
}

knn2 <- function(A){
  head(order(A,decreasing = T),20)
}

m2 <- matrix(sample(c(0,runif(1)),prob = c(0.95,0.05),replace=T,nx*ny),ncol=ny,nrow=nx )
m2_sparse <- Matrix(m2,sparse = T)

object_size(m2_sparse)
object_size(m2)


m2_sparse[1:10,1:10]

system.time(res0 <- apply(m2,1,knn))
system.time(res1 <- apply(m2_sparse,1,knn))


# sparse matrix apply -----------------------------------------------------
# no good way (time consuming)

t1 <- proc.time()
neighbors <- apply(m2_sparse,1,knn2)
res2 <- sapply(1:nrow(m2_sparse),function(x) m2_sparse[x,neighbors[,x]])
dt <- proc.time() - t1
cat('process time:',dt[3])


# original matrix ---------------------------------------------------------
# big matrix (memory consuming)
t1 <- proc.time()
neighbors <- apply(m2,1,knn2)
res2 <- sapply(1:nrow(m2),function(x) m2[x,neighbors[,x]])
dt <- proc.time() - t1
cat('process time:',dt[3])



# sparse matrix with proper slicing ---------------------------------------
# good way

# system.time(res0 <- apply(m2,1,knn))
system.time(res1 <- apply(m2_sparse,1,knn))


