## hybrid user based collaborative filtering and content-based similarity



## simple k-nearest neighbor
.knn <- function(sim, k) apply(sim, MARGIN=1, FUN=function(x) head(
  order(x, decreasing=TRUE, na.last=TRUE), k))

.BIN_UBCF_param <- list(
  method = "jaccard",
  nn = 25,
  weighted = TRUE,
  sample = FALSE,
  W = 0.2,
  simContent = NULL
)

BIN_UBCF <- function(data, parameter = NULL){
  
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
    ## FIXME: add Weiss dissimilarity
    
    sim_cf <- similarity(newdata, model$data,
                      method = model$method)
    # model users, new data users
    model_users <- rownames(model$data)  # names
    new_users <- newdata@data@data@p # index
    
    ## hybrid similarity ##
    if(!is.null(p$simContent)){
      sim_content <- p$simContent # matrix 
      
      W <- p$W
      sim <- W * sim_content[new_users,model_users] + (1-W) * sim_cf
    } else {
      sim <- sim_cf
    }
    
    
    neighbors <- .knn(sim, model$nn)
    
    if(model$weighted) {
      ## similarity with of the neighbors
      s_uk <- sapply(1:nrow(sim), FUN=function(x)
        sim[x, neighbors[,x]])
      
      sum_s_uk <- colSums(s_uk, na.rm=TRUE)
      
      ## calculate the weighted sum
      r_a_norms <- sapply(1:nrow(newdata), FUN=function(i) {
        ## neighbors ratings of active user i
        r_neighbors <- as(model$data[neighbors[,i]], "dgCMatrix")
        drop(as(crossprod(r_neighbors, s_uk[,i]), "matrix"))
      })
      
      ratings <- t(r_a_norms)/sum_s_uk
    }else{
      ratings <- t(sapply(1:nrow(newdata), FUN=function(i) {
        colCounts(model$data[neighbors[,i]])
      }))
    }
    
    rownames(ratings) <- rownames(newdata)
    
    ratings <- new("realRatingMatrix", data=dropNA(ratings))
    ## prediction done
    
    returnRatings(ratings, newdata, type, n)
  }
  
  ## construct recommender object
  new("Recommender", method = "UBCF_HYBRID", dataType = class(data),
      ntrain = nrow(data), model = model, predict = predict)
}

# .REAL_UBCF_param <- list(
#   method = "cosine",
#   nn = 25,
#   sample = FALSE,
#   ## FIXME: implement weighted = TRUE,
#   normalize="center"
# )

# 
# REAL_UBCF <- function(data, parameter = NULL){
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
#     sim <- similarity(newdata, model$data,
#                       method = model$method)
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
#   new("Recommender", method = "UBCF", dataType = class(data),
#       ntrain = nrow(data), model = model, predict = predict)
# }


## register recommender
recommenderRegistry$set_entry(
  method="UBCF_HYBRID", dataType = "binaryRatingMatrix", fun=BIN_UBCF,
  description="Recommender based on user-based collaborative filtering and content-based similarity.",
  parameters=.BIN_UBCF_param)


# recommenderRegistry$set_entry(
#   method="UBCF", dataType = "realRatingMatrix", fun=REAL_UBCF,
#   description="Recommender based on user-based collaborative filtering.",
#   parameters=.REAL_UBCF_param)
