## collaborative filtering (UBCF)

## simple k-nearest neighbor
.knn <- function(sim, k) apply(sim, MARGIN=1, FUN=function(x) head(
  order(x, decreasing=TRUE, na.last=TRUE), k))

.knn2 <- function(x,k) {
  head(x[order(x,decreasing = T, na.last = T)],k)
}


## default parameters 
.BIN_UBCF_param <- list(
  method = "jaccard",
  nn = 25,
  weighted = TRUE,
  sample = FALSE
)


## jaccard similarity using sparse matrix 

jaccard_similarity <- function(x,y) {
  if(is.null(x) || !is(x, "binaryRatingMatrix") || !is(y,"binaryRatingMatrix"))
    stop("newdata and model data should be binaryRatingMatrix.")
  x <- as(x,"dgCMatrix")
  y <- as(y,"dgCMatrix")
  
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
  
  sim
}




BIN_UBCF2 <- function(data, parameter = NULL){
  
  p <- getParameters(.BIN_UBCF_param, parameter)
  
  if(p$sample) data <- sample(data, p$sample)
  
  model <- c(list(
    description = "UBCF-Binary Data: contains full or sample of data set",
    data = data
  ), p )
  
  predict <- function(model, newdata, n=10, data=NULL,
                      type=c("topNList", "ratings", "ratingMatrix"), ...) {
    
    type <- match.arg(type)
    
    
    ## newdata are userid
    if(is.numeric(newdata)) {
      if(is.null(data) || !is(data, "ratingMatrix"))
        stop("If newdata is a user id then data needes to be the training dataset.")
      newdata <- data[newdata,]
    }
    
    if(ncol(newdata) != ncol(model$data)) stop("number of items in newdata does not match model.")
    
    ## prediction
    ## Add sparse jaccard similarity reduce memory use
    if (model$method=='jaccard'){
      t1 <- proc.time()
      sim <- jaccard_similarity(newdata,model$data)
      t2 <- proc.time()
      cat('build similarity: ',(t2-t1)[3],'(s)\n')
    }else{
      sim <- similarity(newdata, model$data,
                        method = model$method)
    }
    
    t1 <- proc.time()
    
    neighbors <- .knn(sim, model$nn)
    t2 <- proc.time()
    cat('build neighbors:',(t2-t1)[3],'(s)\n')
    
    if(model$weighted) {
      ## similarity with of the neighbors
      
      # tempBigMatrix <- as(sim,"matrix")
      # s_uk <- vapply(1:nrow(sim), FUN=function(x)
      #   tempBigMatrix[x, neighbors[,x]],
      #   numeric(model$nn) ## numeric length of model$nn
      #   )
      # rm(tempBigMatrix)
      s_uk <- apply(sim, MARGIN = 1, .knn2, model$nn)
      
      t3 <- proc.time()
      cat('build s_uk:',(t3-t2)[3],'(s)\n')
      
      sum_s_uk <- colSums(s_uk, na.rm=TRUE)
      
      ## calculate the weighted sum
      r_a_norms <- vapply(1:nrow(newdata), FUN=function(i) {
        ## neighbors ratings of active user i
        r_neighbors <- as(model$data[neighbors[,i]], "dgCMatrix")
        
        drop(crossprod(r_neighbors, s_uk[,i]))
      },
      numeric(ncol(newdata))
      )
      
      r_a_norms <- Matrix(r_a_norms, sparse = T)
      t4 <- proc.time()
      cat('build r_a_norms:',(t4-t3)[3],'(s)\n')
      ratings <- t(r_a_norms)/sum_s_uk
      
    }else{
      ratings <- t(sapply(1:nrow(newdata), FUN=function(i) {
        colCounts(model$data[neighbors[,i]])
      }))
      ratings <- Matrix(ratings,sparse = T)
    }
    
    rownames(ratings) <- rownames(newdata)
    
    ratings <- new("realRatingMatrix", data=ratings)
    ## prediction done
    
    t2 <- proc.time()
    cat('total time elapse: ',(t2-t1)[3],'(s)\n')
    
    returnRatings(ratings, newdata, type, n)
  }
  
  ## construct recommender object
  new("Recommender", method = "UBCF2", dataType = class(data),
      ntrain = nrow(data), model = model, predict = predict)
}

.REAL_UBCF_param <- list(
  method = "cosine",
  nn = 25,
  sample = FALSE,
  ## FIXME: implement weighted = TRUE,
  normalize="center"
)


# REAL_UBCF2 <- function(data, parameter = NULL){
#   
#   p <- getParameters(.REAL_UBCF_param, parameter)
#   
#   if(p$sample) data <- sample(data, p$sample)
#   
#   ## normalize data
#   if(!is.null(p$normalize)) data <- normalize(data, method=p$normalize)
#   
#   model <- c(list(
#     description = "UBCF-Real data: contains full or sample of data set",
#     data = data
#   ), p)
#   
#   predict <- function(model, newdata, n=10,
#                       data=NULL, type=c("topNList", "ratings", "ratingMatrix"), ...) {
#     
#     type <- match.arg(type)
#     
#     ## newdata are userid
#     if(is.numeric(newdata)) {
#       if(is.null(data) || !is(data, "ratingMatrix"))
#         stop("If newdata is a user id then data needes to be the training dataset.")
#       newdata <- data[newdata,]
#     }
#     
#     if(!is.null(model$normalize))
#       newdata <- normalize(newdata, method=model$normalize)
#     
#     ## predict ratings
#     if (model$method=='jaccard'){
#       sim <- jaccard_similarity(newdata,model$data)
#     }else{
#       sim <- similarity(newdata, model$data,
#                         method = model$method)  
#     }
#     
#     
#     neighbors <- .knn(sim, model$nn)
#     
#     ## r_ui = r_u_bar + [sum_k s_uk * r_ai - r_a_bar] / sum_k s_uk
#     ## k is the neighborhood
#     ## r_ai - r_a_bar_ is normalize(r_ai) = newdata
#     
#     s_uk <- sapply(1:nrow(sim), FUN=function(x)
#       sim[x, neighbors[,x]])
#     sum_s_uk <- colSums(s_uk)
#     
#     ## calculate the weighted sum
#     r_a_norms <- sapply(1:nrow(newdata), FUN=function(i) {
#       ## neighbors ratings of active user i
#       r_neighbors <- as(model$data[neighbors[,i]], "dgCMatrix")
#       drop(as(crossprod(r_neighbors, s_uk[,i]), "matrix"))
#     })
#     
#     ratings <- t(r_a_norms)/sum_s_uk
#     
#     rownames(ratings) <- rownames(newdata)
#     ratings <- new("realRatingMatrix", data=dropNA(ratings),
#                    normalize = getNormalize(newdata))
#     ratings <- denormalize(ratings)
#     
#     returnRatings(ratings, newdata, type, n)
#   }
#   
#   ## construct recommender object
#   new("Recommender", method = "UBCF2", dataType = class(data),
#       ntrain = nrow(data), model = model, predict = predict)
# }


## register recommender
recommenderRegistry$set_entry(
  method="UBCF2", dataType = "binaryRatingMatrix", fun=BIN_UBCF2,
  description="Recommender based on user-based collaborative filtering (jaccard sparse matrix used).",
  parameters=.BIN_UBCF_param)


# recommenderRegistry$set_entry(
#   method="UBCF2", dataType = "realRatingMatrix", fun=REAL_UBCF2,
#   description="Recommender based on user-based collaborative filtering.",
#   parameters=.REAL_UBCF_param)
