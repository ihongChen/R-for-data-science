
# recommender lab pack test -----------------------------------------------


library(recommenderlab)
library(Matrix)


library(tidyverse)

data("Jester5k")
# load data ---------------------------------------------------------------



Jester5k

eval_sets <- evaluationScheme(data = Jester5k,
                              method = "cross",
                              k = 4,
                              given = -1)


train_data <- getData(eval_sets,"train") ## 切割資料,由train建立model

rec <- Recommender(Jester5k,method='ubcf')
system.time(pred <- predict(rec,newdata = Jester5k,n=10))

source('RECOM_UBCF_SPARSE.r')

rec2 <- Recommender(Jester5k,method="ubcf2")
system.time(pred2 <- predict(rec2,newdata = Jester5k, n=10))


cosine_similarity(Jester5k,Jester5k)
# similarity(Jester5k[1:10],method='jaccard')


source('RECOM_IBCF_SPARSE.r')

jb5k <- binarize(Jester5k,minRating = 4)
rec2b <- Recommender(jb5k,method = 'IBCF2')
rec2 <- Recommender(Jester5k,method = 'IBCF2')
rec2