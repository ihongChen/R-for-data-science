
#### 此腳本利用交易資料,計算各[特徵]的推薦分數

library(RODBC)
library(tidyverse)
# library(dplyr)
library(recommenderlab)
library(reshape2)

##################################################################
## Helper function 
##################################################################

## modify method ##
setMethod("getData", signature(x = "evaluationScheme"),
          function(x, type = c("train", "known", "unknown"), run=1) {
            if(run > x@k) stop("Scheme does not contain that many runs!")
            
            type <- match.arg(type)
            switch(type,
                   train = x@data[x@runsTrain[[run]]], 
                   known = x@knownData[-x@runsTrain[[run]]],
                   unknown = x@unknownData[-x@runsTrain[[run]]]
            )
          })



PredictFeatureScores <- function(features_tables,
                                 users_binary_data,
                                 modelList,
                                 item_reccomend = 20){
  # ================================================================
  # Predict topNList items for users  
  # ================================================================
  # inputs :
  # --------
  # features_tables : user features tags 
  #                 - class : named matrix 
  # users_binary_data : transaction data of predicted users 
  #                 - class : binaryRatingMatrix
  # modelList : list of features model contain recommender 
  # n : predict top n items
  # ================================================================
  # outputs : 
  # ---------
  # predict score (class: TopNList )
  
  ## check 
  if(any(rownames(features_tables) != rownames(users_binary_data)))
    stop("rowname of [features_tables] and [users_binary_data] must consistent")
  
  
  # Recommender List based on different model (features).
  predictItemList <- 
    lapply(modelList,function(rec)
      predict(rec,users_binary_data,n=item_reccomend)
    )
  
  # 
  
  getUI_ScoreM <- function(recc_model,user_features_matrix,rb){
    ## ================================================================
    ## For a given feacture based model, 
    ##  performing features weighting and score again. 
    ## ================================================================
    ## input :
    ## -------
    ## model: model (class: Recommender)
    ## user_features : features matrix
    ## rb : predicted user transaction data (binaryRatingMatrix) 
    ## ================================================================
    ## output : 
    ## -------
    ## Predicted Scoring Matrix w.r.t recc_model and user_features
    ## ================================================================
    UI_MScore <- matrix(NA,ncol=ncol(rb),nrow=nrow(rb))
    # dimnames(UI_MScore) <- dimnames(rb)
    for (i in 1:length(recc_model@items)){
      UI_MScore[i,recc_model@items[[i]]] <- recc_model@ratings[[i]]
    }
    return(UI_MScore)
  }
  
  ## 基於特徵和客戶屬性預測(matrix rating 存於List )
  UI_ScoreM_List <- 
    lapply(predictItemList,function(rec){
      getUI_ScoreM(rec,features_tables,users_binary_data)
    })
  ## remove NA to 0
  scoreList <- lapply(UI_ScoreM_List,function(scoreMatrix){
    score <- scoreMatrix
    score[which(is.na(scoreMatrix),arr.ind = T)] <-0
    return(score)
  })
  ## 最後scores分數 ##
  
  itemsets <- colnames(users_binary_data)
  scores <- Reduce('+',scoreList) # Matrix with the same dim as users_binary_data
  
  rownames(scores) <- rownames(users_binary_data) ## naming ##
  colnames(scores) <- itemsets
  
  ## browser() ## debuger
  
  ## 對有買的品項 scores 給 -1 
  users <- as(users_binary_data,"matrix")
  scores[which(users == T,arr.ind = T)] <- -1 ## purchased items assign -1
  
  ## 產生推薦清單 ## 
  
  topNListPredict <- list()
  predict_Feature_List <- lapply(1:nrow(users_binary_data),function(i){
    orderIndexs <- order(scores[i,],decreasing = T)[1:item_reccomend] # topN index
    topNListPredict[[i]] <- scores[i,orderIndexs]
  })
  
  names(predict_Feature_List) <- rownames(users_binary_data)
  
  ## 推薦清單轉成topNList ##
  
  pred_Feature_TopNList <-
    new("topNList",
        items = lapply(predict_Feature_List,function(x) {
          itemNames <- names(x)
          sapply(itemNames,function(item) which(itemsets==item))
        }),
        ratings = lapply(predict_Feature_List, function(x){
          unname(x)
        }),
        itemLabels = itemsets,
        n = as.integer(item_reccomend)
    )
  return(pred_Feature_TopNList)
}





# 1. 國內股票型基金  -------------------------------------------------------------

##################################################################
## 1-1 資料讀出與整理 
##################################################################
conn <- odbcDriverConnect("Driver=SQL Server;Server=dbm_public;Database=project2017;Uid=sa;Pwd=01060728;")

load('./模型評估/申購基金ui資料.RData')

## 引入HYBRID IBCF/UBCF 
source('src/RECOM_IBCF_HYBRID.R')
source('src/RECOM_UBCF_HYBRID.r')
load('./模型評估/fundsDistance.RData')
load('./模型評估/usersDistance.RData')
## 引入物品相似矩陣
fund_ids <- r_b_purchase@data@itemInfo$labels ## 2,235
base_ids <- funds$基金代碼 ## 2,777
both_ids <-  base_ids %in% fund_ids 
both_ids2 <- fund_ids %in% base_ids
gower_mat <- as.matrix(gower_distance)
gower_mat <- gower_mat[both_ids,both_ids]
gower_mat %>% dim() # 2,170

# load('./模型評估/fundsDistance.RData') ## gower distance ... funds sim
# r_b_purchase

funds_Ids <- r_b_purchase@data@itemInfo$labels ## 2,235 ## 交易紀錄購買基金
funds_base <- sqlQuery(conn,"SELECT 基金代碼 FROM v_基金推薦_基金屬性",stringsAsFactors = F) ## MMA 線上販售基金
funds_base <- funds_base$基金代碼

fundBoth <- funds_Ids %in% funds_base

funds_clus1 <- sqlQuery(conn,"SELECT * FROM v_基金推薦_基金屬性 WHERE CLUSTER=1",stringsAsFactors=F)
funds_clus1 <- funds_clus1 %>% as_tibble()
funds1 <- funds_clus1$基金代碼

# 移除無販售的基金 #
rb_use <- r_b_purchase[,fundBoth] # 45,350 * 2,170 # 原:2,235 ...使用的交易資料...
rb_use <- rb_use[!rowCounts(rb_use)==0,] ## delete no data user , 45,288
# 移除申購數=1的資料 #
rb_use <- rb_use[rowCounts(rb_use)>1] ##
rb_use ## 26,622 * 2170
# 移除關戶客戶 ##
rb_use <- rb_use[rownames(gower_mat_U),] ## 26,621 * 2,170
###
indexID_dom <- which(colnames(rb_use) %in% funds1)

rb_use[,indexID_dom] ## 462國內股票型 (交易紀錄)

UI_dgCmatrix_dom <- as(rb_use,"dgCMatrix")
UI_dgCmatrix_dom[,-indexID_dom] <- 0 ## 非國內股票型=0

# image(UI_dgCmatrix_dom) ##45,350 * 2,170
rb_dom <- as(UI_dgCmatrix_dom,"realRatingMatrix")
rb_dom <- binarize(rb_dom,minRating=0.1)
rb_dom ## binary rating matrix 

## delete no data users ...
rb_dom <- rb_dom[rowCounts(rb_dom)!=0,]
rb_dom # 12,839 * 2,170


##### reccomender feature 1    ###### 
# recc_dom <- Recommender(data = rb_dom,
#                           method = "IBCF",
#                           parameter = list(method = "Jaccard"))

# 2. 國外債券型 ----------------------------------------------------------------


##################################################################
## 2-1 資料讀出與整理 
##################################################################

funds_clus2 <- sqlQuery(conn,"SELECT * FROM v_基金推薦_基金屬性 WHERE CLUSTER=2",stringsAsFactors=F)
funds_clus2 <- funds_clus2 %>% as_tibble()
funds2 <- funds_clus2$基金代碼 # 1031

indexID_foreign_bonds <- which(colnames(rb_use) %in% funds2)

rb_use[,indexID_foreign_bonds] ## 785 國外債券型 (交易紀錄)

UI_dgCmatrix_foreign_bonds <- as(rb_use,"dgCMatrix")
UI_dgCmatrix_foreign_bonds[,-indexID_foreign_bonds] <- 0 ## 非國外債券型=0

# image(UI_dgCmatrix_dom) ##45,350 * 2,170
rb_bonds <- as(UI_dgCmatrix_foreign_bonds,"realRatingMatrix")
rb_bonds <- binarize(rb_bonds,minRating=0.1)
rb_bonds ## binary rating matrix 


rm(UI_dgCmatrix_foreign_bonds,UI_dgCmatrix_dom)
###### recommender feature 2 --- bonds #####
# recc_bonds <- Recommender(data = rb_bonds,
#                           method = "IBCF",
#                           parameter = list(method = "Jaccard"))

# 3. 國外股票型 ----------------------------------------------------------------

##################################################################
## 3-1 資料讀出與整理 
##################################################################

funds_clus3 <- sqlQuery(conn,"SELECT * FROM v_基金推薦_基金屬性 WHERE CLUSTER=3",stringsAsFactors=F)
funds_clus3 <- funds_clus3 %>% as_tibble()
funds3 <- funds_clus3$基金代碼 # 1200

indexID_foreign_stocks <- which(colnames(rb_use) %in% funds3)

rb_use[,indexID_foreign_stocks] ## 923 國外股票型 (交易紀錄)

UI_dgCmatrix_foreign_stocks <- as(rb_use,"dgCMatrix")
UI_dgCmatrix_foreign_stocks[,-indexID_foreign_stocks] <- 0 ## 非國外股票型=0

# image(UI_dgCmatrix_dom) ##45,350 * 2,170
rb_stocks <- as(UI_dgCmatrix_foreign_stocks,"realRatingMatrix")

rb_stocks <- binarize(rb_stocks,minRating=0.1)
rb_stocks ## binary rating matrix

###### recommender feature 3####
# recc_stocks <- Recommender(data = rb_stocks,
#                            method = "IBCF",
#                            parameter = list(method = "Jaccard"))
# 

# rm(UI_dgCmatrix_foreign_stocks,UI_dgCmatrix_foreign_bonds,UI_dgCmatrix_dom)
gc()



# 標記用戶特徵 ------------------------------------------------------------------

SQL_trans <- " select b.cluster,a.身分證字號,基金中文名稱 from v_基金推薦_申購明細 a
	  left join v_基金推薦_基金屬性 b on left(a.基金中文名稱,3) = b.基金代碼
  where [申購登錄年] >= 2015 "

user_purchase_details <- sqlQuery(conn,SQL_trans,stringsAsFactors = F)
user_purchase_details <- user_purchase_details %>% mutate_each(funs(factor),cluster) 
str(user_purchase_details)
user_purchase_details %>% head()

user_features <- 
  user_purchase_details %>% 
  mutate(fundId=substr(基金中文名稱,1,3)) %>%
  group_by(身分證字號,fundId,cluster) %>% 
  count() %>% 
  ungroup() %>% 
  arrange(desc(n)) 
  # dcast(身分證字號~fundId,value.var="n")
user_features1 <- 
  user_features %>% 
  dcast(身分證字號~cluster,fun.aggregate=sum,value.var='n') %>% 
  mutate(國內股票型 = ifelse(`1`!=0,1,0),
              國外債券型 = ifelse(`2`!=0,1,0),
              國外股票型 = ifelse(`3`!=0,1,0))

user_features1 %>% head(10)  ## 比較 user_features1$身分證字號 == rownames(r_b_purchase)
user_features1 %>% dim() # 45350

users_df <- data_frame(userid = rownames(rb_use))
user_features_matrix <- 
  user_features1 %>% 
  select(身分證字號,國內股票型,國外債券型,國外股票型) %>% 
  left_join(users_df,by=c("身分證字號" = "userid")) %>% 
  select(-身分證字號) %>% 
  as.matrix()
rownames(user_features_matrix) <- user_features1$身分證字號


# 模型評估 ----------------------------------------------------------------
# modelList <- list(f1 = recc_dom,f2=recc_bonds,f3=recc_stocks) 

rb_use <- rb_use[!rowCounts(rb_use)==0,] ## delete no data user , 26,622
set.seed(100)
eval_sets <- evaluationScheme(data = rb_use,
                              method = "split",
                              train = 0.9,
                              k = 1,
                              given = -1)


train_data <- getData(eval_sets,"train") ## 切割資料,由train建立model
## model1 ##
train_dom_users <- rownames(train_data)[rownames(train_data) %in% rownames(rb_dom[rowCounts(rb_dom)!=0])]
train_data[train_dom_users,] ## 11,557 train data 裡面為 f1:domestic stocks交易紀錄

## model2 ##
train_bonds_users <- rownames(train_data)[rownames(train_data) %in% rownames(rb_bonds[rowCounts(rb_bonds)!=0])]
train_data[train_bonds_users,] # 16,021
## model3 ##
train_stock_users <- rownames(train_data)[rownames(train_data) %in% rownames(rb_stocks[rowCounts(rb_stocks)!=0])]
train_data[train_stock_users,] # 17,180
## Build features based Model ####
number_neighbors = 30
model1 <- Recommender(train_data[train_dom_users,],
                      method = "IBCF",
                      parameter = list(method = "Jaccard",
                                       k = number_neighbors))
model2 <- Recommender(train_data[train_bonds_users,],
                      method = "IBCF",
                      parameter = list(method = "Jaccard",
                                       k = number_neighbors ))
model3 <- Recommender(train_data[train_stock_users,],
                      method = "IBCF",
                      parameter = list(method = "Jaccard",
                                       k = number_neighbors))

model1_u <- Recommender(train_data[train_dom_users,],
                      method = "UBCF",
                      parameter = list(method = "Jaccard",
                                       nn = number_neighbors))
model2_u <- Recommender(train_data[train_bonds_users,],
                      method = "UBCF",
                      parameter = list(method = "Jaccard",
                                       nn = number_neighbors ))
model3_u <- Recommender(train_data[train_stock_users,],
                      method = "UBCF",
                      parameter = list(method = "Jaccard",
                                       nn = number_neighbors))



modelList <- list(f1=model1,f2=model2,f3=model3)
modelListU <- list(f1=model1_u,f2=model2_u,f3=model3_u)
userids <- rownames(rb_use)
test_rownames <- userids[!userids %in% rownames(train_data)]   # 2663

### Evaluate ######
# features based IBCF
eval_pred_list <- lapply(c(3,5,10,15,20),function(x){
  PredictFeatureScores(features_tables = user_features_matrix[test_rownames,],
                       modelList = modelList,
                       users_binary_data = getData(eval_sets,"known"), ## 交易資料須為known part來預測unknown part##
                       item_reccomend = x)
})

# features based UBCF
eval_pred_listU <- lapply(c(3,5,10,15,20),function(x){
  PredictFeatureScores(features_tables = user_features_matrix[test_rownames,],
                       modelList = modelListU,
                       users_binary_data = getData(eval_sets,"known"), ## 交易資料須為known part來預測unknown part##
                       item_reccomend = x)
})



# accuracy 
# feature based IBCF
eval_acc_list <-  
  lapply(eval_pred_list,function(eval_pred){
    calcPredictionAccuracy(
      x = eval_pred,
      data = getData(eval_sets, "unknown"),
      byUser = F,
      given = 10
    )
  })
# feature based UBCF
eval_acc_listU <-  
  lapply(eval_pred_listU,function(eval_pred){
    calcPredictionAccuracy(
      x = eval_pred,
      data = getData(eval_sets, "unknown"),
      byUser = F,
      given = 10
    )
  })

acc_features <- sapply(eval_acc_list,'[',c('TPR','FPR'))
acc_featuresU <- sapply(eval_acc_listU,'[',c('TPR','FPR'))
n <- c(3,5,10,15,20)
acc_features <- rbind(acc_features,n)
acc_features <- 
  t(acc_features) %>% 
  as.tibble() %>% 
  mutate(algo = "FEATURES_IBCF")
  
acc_featuresU <- 
  t(acc_featuresU) %>% 
  as.tibble() %>% 
  mutate(algo = "FEATURES_UBCF") %>% 
  cbind(n)



#### 與 ibcf/ubcf/popular 比較 ####
# ibcf recommender
recc_ibcf <- Recommender(train_data,
                         method = "IBCF",
                         parameter = list(method = "Jaccard"))


# ubcf recommender
recc_ubcf <- Recommender(train_data,
                         method='UBCF',
                         parameter = list(method = "Jaccard",nn=50)
                         )
# popular recommender
recc_pop <- Recommender(train_data,method='Popular')
# hybrid content + IBCF recommender
rec_ibcf_hybrid <- 
  Recommender(train_data,method="IBCF_HYBRID",
              parameter = list(method = "Jaccard",W=0,simContent = 1- gower_mat))


# hybrid content UBCF recommender 
rec_ubcf_hybrid <- 
  Recommender(train_data,method = "UBCF_HYBRID",
              parameter = list(method = "Jaccard",W=0.1,simContent = 1-gower_mat_U))


# eval
# ibcf
eval_pred_ibcf <- lapply(n,function(i){
  predict(object = recc_ibcf,
          newdata = getData(eval_sets,"known"),
          n = i,
          type = "topNList")
  })
# ibcf + content based (hybrid)
eval_pred_ibcf_hybrid <- lapply(n,function(i){
  predict(object = rec_ibcf_hybrid,
          newdata = getData(eval_sets,"known"),
          n = i,
          type = "topNList")
})

# ubcf
eval_pred_ubcf <- lapply(n,function(i){
  predict(object = recc_ubcf,
          newdata = getData(eval_sets,"known"),
          n = i,
          type = "topNList")
})
##### 評估混合式UBCF Weighting (無優化ubcf) #####
# hybrid content ubcf

## 無法lapply一次計算(因記憶體超載)....
recc_ubcf <- Recommender(train_data,
                         method='UBCF')
rec_ubcf_hybrid <- 
  Recommender(train_data,method = "UBCF_HYBRID",
              parameter = list(method = "Jaccard",
                               # nn = 50,
                               W = 1,
                               simContent = 1-gower_mat_U))

# pred_ubcf <- predict(recc_ubcf,newdata = getData(eval_sets,"known"),n=20)  
pred_ubcf_hybrid <- predict(rec_ubcf_hybrid,
                            newdata = getData(eval_sets,"known"),
                            n = 20,
                            type = "topNList")

# acc_ubcf <- calcPredictionAccuracy(
#   x = pred_ubcf,
#   data = getData(eval_sets,"unknown"),
#   byUser = F,
#   given = 10
# )


acc_hybrid_ubcf1 <- calcPredictionAccuracy(
  x = pred_ubcf_hybrid,
  data = getData(eval_sets,"unknown"),
  byUser = F,
  given = 10)

acc_hybrid_ubcf_w02 <- calcPredictionAccuracy(
  x = pred_ubcf_hybrid,
  data = getData(eval_sets,"unknown"),
  byUser = F,
  given = 10)

acc_hybrid_ubcf_w03 <- calcPredictionAccuracy(
  x = pred_ubcf_hybrid,
  data = getData(eval_sets,"unknown"),
  byUser = F,
  given = 10)

acc_hybrid_ubcf_w05 <- calcPredictionAccuracy(
  x = pred_ubcf_hybrid,
  data = getData(eval_sets,"unknown"),
  byUser = F,
  given = 10)

################################################
# popular
eval_pred_pop <- lapply(n,function(i){
  predict(object = recc_pop,
          newdata = getData(eval_sets,"known"),
          n = i,
          type = "topNList")
})


### eval accuracy ###
# ibcf 
eval_acc_list_ibcf <- 
  lapply(eval_pred_ibcf,function(eval_pred){
    calcPredictionAccuracy(
      x = eval_pred,
      data = getData(eval_sets,"unknown"),
      byUser = F,
      given = 10
    )
})
# hybrid ibcf
eval_acc_list_ibcf_hybrid <- 
  lapply(eval_pred_ibcf_hybrid,function(eval_pred){
    calcPredictionAccuracy(
      x = eval_pred,
      data = getData(eval_sets,"unknown"),
      byUser = F,
      given = 10
    )
  })

# ubcf
eval_acc_list_ubcf <- 
  lapply(eval_pred_ubcf,function(eval_pred){
    calcPredictionAccuracy(
      x = eval_pred,
      data = getData(eval_sets,"unknown"),
      byUser = F,
      given = 10
    )
  })
# hybrid content ubcf 
acc_hybrid_ubcf <- calcPredictionAccuracy(
  x = pred_ubcf_hybrid20,
  data = getData(eval_sets,"unknown"),
  byUser = F,
  given = 10)
# pop
eval_acc_list_pop <- 
  lapply(eval_pred_pop,function(eval_pred){
    calcPredictionAccuracy(
      x = eval_pred,
      data = getData(eval_sets,"unknown"),
      byUser = F,
      given = 10
    )
  })




acc_pop <- sapply(eval_acc_list_pop,'[',c('TPR','FPR'))
acc_ibcf <- sapply(eval_acc_list_ibcf,'[',c('TPR','FPR'))
acc_ubcf <- sapply(eval_acc_list_ubcf,'[',c('TPR','FPR'))
acc_ibcf_hybrid <- sapply(eval_acc_list_ibcf_hybrid,'[',c('TPR','FPR'))
n <- c(3,5,10,15,20)
acc_ibcf <- rbind(acc_ibcf,n)
acc_ubcf <- rbind(acc_ubcf,n)
acc_pop <- rbind(acc_pop,n)

acc_df_ibcf <- t(acc_ibcf) %>% as.tibble()
acc_df_ubcf <- t(acc_ubcf) %>% as.tibble()
acc_df_pop <- t(acc_pop) %>% as.tibble()

acc_df_ibcf <- acc_df_ibcf %>% mutate(algo = "IBCF")
acc_df_ubcf <- acc_df_ubcf %>% mutate(algo = "UBCF")
acc_df_pop <- acc_df_pop %>% mutate(algo = "POPULAR")


acc_df_tot <- acc_features %>% 
  rbind(acc_df_ibcf,acc_df_ubcf,acc_df_pop,acc_featuresU) 
  
# load(file='./模型評估/ev_result_features.RData')

# 不畫feature IBCF, 
acc_df_tot %>% filter(algo == 'UBCF')
ggplot(acc_df_tot %>% filter(algo != 'FEATURES')
       ,aes(x=FPR,y=TPR,color=algo)) + 
  geom_point() + geom_line() +
  annotate("text", x = c(0.0015,0.0023,0.0046,0.0068,0.0092), 
           y = c(0.175,0.21,0.28,0.32,0.35) ,
           label = c('n=3','n=5','n=10','n=15','n=20')) +
  ggtitle('ROC')



save(eval_acc_list,
     eval_acc_listU,
     eval_acc_list_ibcf,
     eval_acc_list_ubcf,
     eval_acc_list_pop,
     acc_df_tot,file = './模型評估/ev_result_features.RData')







# eval_prediction2 <- predict(object = recc_ibcf,
#                            newdata = getData(eval_sets,"known"),
#                            n = 20, # item to recommend
#                            type = "topNList")
# 
# eval_accuracy2 <- calcPredictionAccuracy(
#   x = eval_prediction2,
#   data = getData(eval_sets, "unknown"),
#   byUser = F,
#   given = 10)
# 
# eval_prediction3 <- predict(recc_ubcf,
#                             newdata = getData(eval_sets,"known"),
#                             n=20,
#                             type="topNList")
# 
# eval_accuracy3 <- calcPredictionAccuracy(
#   x=eval_prediction3,
#   data = getData(eval_sets,"unknown"),
#   byUser = F,
#   given = 10
# )

# eval_accuracy
# eval_accuracy2
# eval_accuracy3
# 
# rbind('features'=eval_accuracy,'ibcf'=eval_accuracy2,'ubcf'=eval_accuracy3)



### 
algorithms <- list(
  "random items" = list(name="RANDOM"),
  "popular items" = list(name="POPULAR"),
  "user-based CF" = list(name="UBCF",param=list(nn=50)),
  "item-based CF" = list(name="IBCF",param=list(k=10))
)
ev_result <-evaluate(eval_sets,algorithms,type="topNList",n=c(3,5,10,20))
plot(ev_result,annotate=c(2,3))
# 
# 
# 
# ev_result$`user-based CF`@results
# ev_result$`random items`@results
# ev_result$`popular items`@results
# ev_result$`item-based CF`@results



################################################################################
# TEST --------------------------------------------------------------------


# reccIBCF_dom <- Recommender(rb_dom,'IBCF',parameter = 
#                               list(normalize_sim_matrix = F))
# 
# image(reccIBCF_dom@model$sim[indexID_dom,indexID_dom]) ## 國內基金相似度
# image(reccIBCF_dom@model$sim) ## 全部基金相似度
# # reccIBCF_dom@model
# pred_dom_IBCF <- reccIBCF_dom@predict(model = reccIBCF_dom@model,
#                                       newdata = rb_use,n=20,type="topNList")
# 
# # pred_dom_IBCF@items[1:10]
# # pred_dom_IBCF@ratings[1:10]
# 
# ## 建立u-i特徵分數矩陣 ###
# 
# UI_MatrixScore_dom <- matrix(NA,ncol=ncol(rb_use),nrow=nrow(rb_use))
# dimnames(UI_MatrixScore_dom) <- dimnames(rb_use)
# # pred_dom_IBCF@items[[1]]
# 
# 
# for(i in 1:length(pred_dom_IBCF@items)){
#   UI_MatrixScore_dom[i,pred_dom_IBCF@items[[i]]] <- pred_dom_IBCF@ratings[[i]]
# } 
# 
# 
# UI_score_dom <- as(UI_MatrixScore_dom,"realRatingMatrix")
# UI_score_dom
# # UI_score_dom@data@Dimnames
# # 
# # image(UI_score_dom[rownames(rb_dom),][1:100,1:50])
# 
# rm(UI_MatrixScore_dom,pred_dom_IBCF)
# 
# ## =============================================================
# ##  total score 
# ## =============================================================
# 
# user_features_1to10 <- user_features_matrix[1:10,]
# rb_use[1:10,]
# UI_MScore <- matrix(NA,ncol=ncol(rb_use),nrow=10)  
# 
# 
# getUI_ScoreM <- function(pred_IBCF,user_features_matrix,rb){
#   ## ================================================================
#   ## input :
#   ## -------
#   ## pred_IBCF: IBCF model (class: Recommender)
#   ## user_features : features matrix ,
#   ## rb : predicted user transaction data (binaryRatingMatrix) 
#   ## ================================================================
#   ## output : 
#   ## -------
#   ## Predicted Scoring Matrix w.r.t pred_IBCF model and user_features
#   ## ================================================================
#   UI_MScore <- matrix(NA,ncol=ncol(rb),nrow=nrow(rb))
#   dimnames(UI_MScore) <- dimnames(rb)
#   for (i in 1:length(pred_IBCF@items)){
#     UI_MScore[i,pred_IBCF@items[[i]]] <- pred_IBCF@ratings[[i]]
#   }
#   return(UI_MScore)
# }
# 
# 
# 
# 
# m <- matrix(1:10,ncol=2)
# m
# 
# 
# ## 
# recommenderList <- list('recommender_dom'=recc_dom,
#                  'recommender_bonds'=recc_bonds,
#                  'recommender_stocks'=recc_stocks)
# 
# # 基於不同特徵的推薦 List
# predictList <- 
# lapply(recommenderList,function(rec)
#   predict(rec,rb_use[1:10,],n=20)
#   )
# # (rownames(user_features_1to10) == rownames(rb_use[1:9,]))
# 
# ## 基於國內股票型(feature1)和客戶屬性預測結果(return Matrix rating)
# getUI_ScoreM(predictList$recommender_dom,user_features_1to10,rb_use[1:10,]) #
# 
# ## 基於特徵和客戶屬性預測(matrix rating 存於List )
# UI_ScoreM_List <- 
# lapply(predictList,function(rec){
#   getUI_ScoreM(rec,user_features_1to10,rb_use[1:10])
# })
# 
# 
# ## 
# scoreList <- lapply(UI_ScoreM,function(scoreMatrix){
#   score <- scoreMatrix
#   score[which(is.na(scoreMatrix),arr.ind = T)] <-0
#   return(score)
# })
# 
# 
# score_ans <- Reduce('+',scoreList)
# rownames(score_ans) <- rownames(scoreList$recommender_dom) ## naming ##
# 
# ## remove purchesed items for a user
# rowCounts(rb_use[1,])
# user1 <- as(rb_use[1,],"matrix")
# which(user1 == T) ## 有買的 -- index:140
# colnames(user1)[140]
# rb_use[1,'310'] ## names : 基金代碼 --310 
# ## 對有買的品項 score_ans 給 -1 
# rowCounts(rb_use[1:10,])
# users <- as(rb_use[1:10,],"matrix")
# score_ans[which(users == T,arr.ind = T)] <- -1 ## purchased items assign -1
# 
# colnames(user1)[c(5,1942)]
# # for user 7 056270720 #
# # ('120','T34' -- purchased--
# # ,'J99' ,'T35' ,'78F','L91' ,'PD0','Z07','BL6') --- recommend ---
# which(users == T,arr.ind = T)
# ## topN items and rating 
# topNIndexs1 <- order(score_ans[1,],decreasing = T)[1:10]
# score_ans[1,topNIndexs1]
# rownames(score_ans)
# topNListPredict <- list()
# # topNListPredict[[1]] <- 1:10
# # topNListPredict[[2]] <- 2:10
# namesVec <- rownames(score_ans)
# predict_List <- lapply(1:10,function(i){
#   orderIndexs <- order(score_ans[i,],decreasing = T)[1:10] # topN index
#   topNListPredict[[i]] <- score_ans[i,orderIndexs]
# })
# names(predict_List) <- namesVec
# predict_List ## 

######## 如何存成 TopList Class ?? #########
# lapply(predict_List,function(x) unames(x))
# 
# itemsets <- colnames(rb_use)
# names(predict_List$`033591090`) %in% itemsets 
# predict1_items <- names(predict_List$`033591090`)
# itemsets[which(itemsets %in% predict1_items)]
# predict1_items
# ## names 對應成 index 
# sapply(predict1_items,function(x) which(itemsets==x)) # name->fundid, val->fundindex
# 
# # itemsets == names(predict_List$`033591090`) 
# predFeaturesTest <-
#   new("topNList",
#       items = lapply(predict_List,function(x) {
#         itemNames <- names(x)
#         sapply(itemNames,function(item) which(itemsets==item))
#         }),
#       ratings = lapply(predict_List, function(x){
#        unname(x)
#       }),
#       itemLabels = itemsets,
#       n = as.integer(10)
#       )

##===================================================
# note : train data 才能 get rownames
# getData(eval_sets,"")


