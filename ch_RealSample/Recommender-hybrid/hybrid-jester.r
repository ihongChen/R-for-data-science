## create a hybrid recommender
library(recommenderlab)
library(tidyverse)
HybridRecommender <- function(..., weights = NULL) {
  recommender <- list(...)
  
  if(is.null(weights)) weights <- rep(1, length(recommender))
  else if(length(recommender) != length(weights)) stop("Number of recommender and length of weights does not agree!")
  weights <- weights/sum(weights)
  
  if(!all(sapply(recommender, is, "Recommender"))) stop("Not all supplied models are of class 'Recommender'.")
  
  model <- list(recommender = recommender, weights = weights)
  
  predict <- function(model=NULL, newdata, n=10,
                      data= NULL, type=c("topNList", "ratings", "ratingMatrix"), ...) {
    
    type <- match.arg(type)
    
    ## newdata are userid
    if(is.numeric(newdata)) {
      if(is.null(data) || !is(data, "ratingMatrix"))
        stop("If newdata is a user id then data needes to be the training dataset.")
      newdata <- data[newdata, , drop = FALSE]
    }
    
    #if(ncol(newdata) != length(model$labels)) stop("number of items in newdata does not match model.")
    
    pred <- lapply(model$recommender, FUN = function(object)
      object@predict(object@model, newdata, data=data, type="ratings", ...))
    
    ratings <- matrix(NA, nrow=nrow(newdata), ncol = ncol(newdata))
    for(i in 1:nrow(pred[[1]])) {
      ratings[i,] <- colSums(t(sapply(pred, FUN = function(p)
        as(p[i,], "matrix"))) * model$weights, na.rm = TRUE)
    }
    
    ratings <- as(ratings, "realRatingMatrix")
    colnames(ratings) <- colnames(newdata)
    
    
    if(type == "ratingMatrix")
      stop("Hybrid cannot predict a complete ratingMatrix!")
    
    returnRatings(ratings, newdata, type, n)
  }
  
  ## this recommender has no model
  new("Recommender", method = "HYBRID",
      dataType = "ratingMatrix",
      ntrain = NA_integer_,
      model = model,
      predict = predict)
}


ri <- Recommender(data,'IBCF')
ru <- Recommender(data[2:5,1:7],'UBCF')
HybridRecommender(ri,ru)

ri@model
ru@model$data


##### test: not the same dim U-I matrix with hybrid ###
data("Jester5k")
Jester_binary <- binarize(Jester5k,minRating=5)
Jester_binary <- Jester_binary[rowCounts(Jester_binary) > 20]

Jester_binary500 <- sample(Jester_binary,500)
Jester_binary500_2 <- sample(Jester_binary,500)

# n20
ui3_Matrix <- as(Jester_binary[1:3,],"matrix")
ui3_Matrix[1,1:20] 
indexT <- which(ui3_Matrix[1,1:20] == T)
ui3_Matrix[1,indexT] = FALSE



image(Jester_binary500,type="b")
Jb1 <- Jester_binary500[1:100,]
Jb2 <- Jester_binary500_2[2:200,]
Jb1@data
class(Jb1)
recc1 <- Recommender(Jb1,'UBCF')
recc2 <- Recommender(Jb2,'IBCF')

hy12 <- HybridRecommender(recc1,recc2)
predhy12 <- hy12@predict(hy12@model,Jester_binary500_2[1:10])

# colnames(Jester_binary500_2[5:10,5:10])

predhy12@itemLabels
predhy12@items
predhy12@ratings


predhy12@items
predhy12@ratings

M <- matrix(NA,ncol=100,nrow=20)
for(i in 1:length(predhy12@items)) M[i,predhy12@items[[i]]] <- TRUE

image(as(M,"dgCMatrix"))

hy12@model


###### evaluation #####

eval_sets <- evaluationScheme(data = Jester_binary500_2,
                              method = "cross-validation",
                              k = 3,
                              given = -1)

eval_prediction <- predict(object = hy12,
                           newdata = getData(eval_sets,"known"),
                           n = 10, # item to recommend
                           type = "topNList")
eval_accuracy <- calcPredictionAccuracy(
  x = eval_prediction,
  data = getData(eval_sets, "unknown"),
  byUser = FALSE,
  given = 10)

eval_accuracy

