
#### 此腳本利用交易資料,計算各[特徵]的推薦分數

library(RODBC)
library(tidyverse)
library(recommenderlab)
library(reshape2)

##################################################################
## Helper function 
##################################################################

PredictFeatureScores <- function(features_tables,
                                 users_binary_data,
                                 modelList,
                                 n = 20){
  # inputs :
  # --------
  # features_tables : user features tags (class:named matrix -- userid, items)
  # users_binary_data : transaction data of predicted users (class: binaryRatingMatrix)
  # modelList : list of features model contain recommender 
  # n : predict top n items
  # ===================================================================
  # outputs : scores
  # ------------
  # 
  
  
  getUI_ScoreM <- function(pred_IBCF,user_features_matrix,rb){
    ## ================================================================
    ## input :
    ## -------
    ## pred_IBCF: IBCF model (class: Recommender)
    ## user_features : features matrix ,
    ## rb : predicted user transaction data (binaryRatingMatrix) 
    ## ================================================================
    ## output : 
    ## -------
    ## Predicted Scoring Matrix w.r.t pred_IBCF model and user_features
    ## ================================================================
    UI_MScore <- matrix(NA,ncol=ncol(rb),nrow=nrow(rb))
    dimnames(UI_MScore) <- dimnames(rb)
    for (i in 1:length(pred_IBCF@items)){
      UI_MScore[i,pred_IBCF@items[[i]]] <- pred_IBCF@ratings[[i]]
    }
    return(UI_MScore)
  }
  
}



# UI_IBCF_score_rating <- function(rb,rb_use){
#   #==========================================# 有問題喔!!!!! ######
#   ## input  
#   # rb : rating binary wrt features 
#   # rb_use : total rating binary used for transaction data
#   # output: u-i特徵分數矩陣 
#   #==========================================
#   reccIBCF <- Recommender(rb,'IBCF',parameter=list(normalize_sim_matrix=F))
#   
#   pred_IBCF <- reccIBCF@predict(model = reccIBCF@model,
#                                 newdata = rb_use,
#                                 n = 20,
#                                 type = "topNList")
#   UI_MatrixScore <- matrix(NA,ncol=ncol(rb_use),nrow=nrow(rb_use))
#   dimnames(UI_MatrixScore) <- dimnames(rb_use)
#   
#   for (i in 1:length(pred_IBCF@items)){
#     UI_MatrixScore[i,pred_IBCF@items[[i]] ] <- pred_IBCF@ratings[[i]]
#   }
#   UI_Features_Scores <- as(UI_MatrixScore,"realRatingMatrix")
#   return(UI_Features_Scores)
# }




# 1. 國內股票型基金  -------------------------------------------------------------

##################################################################
## 1-1 資料讀出與整理 
##################################################################
conn <- odbcDriverConnect("Driver=SQL Server;Server=dbm_public;Database=project2017;Uid=sa;Pwd=01060728;")

load('./模型評估/申購基金ui資料.RData')
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
rb_dom # 17,370 * 2,170

##### split data/train data???  ##############

##### reccomender feature 1   ###### 
recc_dom <- Recommender(data = rb_dom,
                          method = "IBCF",
                          parameter = list(method = "Jaccard"))

dataPredict1 <- predict(recc_dom,rb_use[1:10,])
##################################################################
## 1-2 IBCF 推薦分數
##################################################################
# UI_IBCF_score_dom <- UI_IBCF_score_rating(rb_dom,rb_use) ## 國內基金型 rating 

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
recc_bonds <- Recommender(data = rb_bonds,
                          method = "IBCF",
                          parameter = list(method = "Jaccard"))

dataPredict2 <- predict(recc_bonds,rb_use[1:10,])

##################################################################
## 2-2 IBCF 推薦分數
##################################################################

# UI_IBCF_score_bonds <- UI_IBCF_score_rating(rb_bonds,rb_use) ## 國外債券型 rating 

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
recc_stocks <- Recommender(data = rb_stocks,
                           method = "IBCF",
                           parameter = list(method = "Jaccard"))

dataPredict3 <- predict(recc_stocks,rb_use[1:10,])

rm(UI_dgCmatrix_foreign_stocks)
##################################################################
## 3-2 IBCF 推薦分數
##################################################################
# UI_IBCF_score_stocks <- UI_IBCF_score_rating(rb_stocks,rb_use)


rm(UI_dgCmatrix_foreign_stocks,UI_dgCmatrix_foreign_bonds,UI_dgCmatrix_dom)
gc()

# image(UI_IBCF_score_bonds[1:10,1:100])


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


# 評分 SCORE ----------------------------------------------------------------

user_features_matrix %>% head()



## test 
m <- matrix(rnorm(100),ncol=5)
f <- matrix(sample(c(0,1),20,replace=T))
m
f

ans <- t(sapply(1:length(f1),function(i) m[i,]*f[i]))
ans2 <- t(sapply(1:length(f2),function(i) m[i,]*f2[i]))

sapply()


col1<-c(NA,1,2,3)
col2<-c(1,2,3,NA)
col3<-c(NA,NA,2,3)

rowSums(cbind(col1,col2,col3), na.rm=TRUE)
cbind(col1,col2,col3)





order(MScore_total[1,],na.last=NA,decreasing = T)



order(MScore2[1,],na.last=NA,decreasing = T)


MScore1[1,1264]

colnames(UI_MScore) <- colnames(rb_use)
rownames(UI_MScore) <- rownames(rb_use[1:10,])

dataPredict1@ratings


UI_MatrixScore <- matrix(NA,ncol=ncol(rb_use),nrow=nrow(rb_use))
dimnames(UI_MatrixScore) <- dimnames(rb_use)

for (i in 1:length(pred_IBCF@items)){
  UI_MatrixScore[i,pred_IBCF@items[[i]] ] <- pred_IBCF@ratings[[i]]
}
UI_Features_Scores <- as(UI_MatrixScore,"realRatingMatrix")






U1 <- as(UI_IBCF_score_dom,"dgCMatrix") # 45350*2170
U2 <- as(UI_IBCF_score_bonds,"dgCMatrix")
U3 <- as(UI_IBCF_score_stocks,"dgCMatrix")
image(U3)

u <- as(newdata,"dgCMatrix")

score_m <- U1 + U2 + U3

t(score_m) %*% x 
x %>% dim()

U1

x %>% dim()
image(score_m)

U <- as(UI_IBCF_score_stocks,"dgCMatrix")
x <- as(user_features_matrix,"dgCMatrix")
# rownames(U[1:10,]) == names(x[1:10])


U %>% dim()
u %>% dim()
x %>% dim()

apply(U,1,sum,na.rm=T)

score_m <- t(U) %*% x

score_m

matrix(x,ncol=1) 

image(as(U,'realRatingMatrix'))




# TEST --------------------------------------------------------------------


reccIBCF_dom <- Recommender(rb_dom,'IBCF',parameter = 
                              list(normalize_sim_matrix = F))

image(reccIBCF_dom@model$sim[indexID_dom,indexID_dom]) ## 國內基金相似度
image(reccIBCF_dom@model$sim) ## 全部基金相似度
# reccIBCF_dom@model
pred_dom_IBCF <- reccIBCF_dom@predict(model = reccIBCF_dom@model,
                                      newdata = rb_use,n=20,type="topNList")

# pred_dom_IBCF@items[1:10]
# pred_dom_IBCF@ratings[1:10]

## 建立u-i特徵分數矩陣 ###

UI_MatrixScore_dom <- matrix(NA,ncol=ncol(rb_use),nrow=nrow(rb_use))
dimnames(UI_MatrixScore_dom) <- dimnames(rb_use)
# pred_dom_IBCF@items[[1]]


for(i in 1:length(pred_dom_IBCF@items)){
  UI_MatrixScore_dom[i,pred_dom_IBCF@items[[i]]] <- pred_dom_IBCF@ratings[[i]]
} 


UI_score_dom <- as(UI_MatrixScore_dom,"realRatingMatrix")
UI_score_dom
# UI_score_dom@data@Dimnames
# 
# image(UI_score_dom[rownames(rb_dom),][1:100,1:50])

rm(UI_MatrixScore_dom,pred_dom_IBCF)

## =============================================================
##  total score 
## =============================================================

user_features_1to10 <- user_features_matrix[1:10,]
rb_use[1:10,]
UI_MScore <- matrix(NA,ncol=ncol(rb_use),nrow=10)  


getUI_ScoreM <- function(pred_IBCF,user_features_matrix,rb){
  ## ================================================================
  ## input :
  ## -------
  ## pred_IBCF: IBCF model (class: Recommender)
  ## user_features : features matrix ,
  ## rb : predicted user transaction data (binaryRatingMatrix) 
  ## ================================================================
  ## output : 
  ## -------
  ## Predicted Scoring Matrix w.r.t pred_IBCF model and user_features
  ## ================================================================
  UI_MScore <- matrix(NA,ncol=ncol(rb),nrow=nrow(rb))
  dimnames(UI_MScore) <- dimnames(rb)
  for (i in 1:length(pred_IBCF@items)){
    UI_MScore[i,pred_IBCF@items[[i]]] <- pred_IBCF@ratings[[i]]
  }
  return(UI_MScore)
}




m <- matrix(1:10,ncol=2)
m


## 
recommenderList <- list('recommender_dom'=recc_dom,
                 'recommender_bonds'=recc_bonds,
                 'recommender_stocks'=recc_stocks)

# 基於不同特徵的推薦 List
predictList <- 
lapply(recommenderList,function(rec)
  predict(rec,rb_use[1:10,],n=20)
  )
# (rownames(user_features_1to10) == rownames(rb_use[1:9,]))

## 基於國內股票型(feature1)和客戶屬性預測結果(return Matrix rating)
getUI_ScoreM(predictList$recommender_dom,user_features_1to10,rb_use[1:10,]) #

## 基於特徵和客戶屬性預測(matrix rating 存於List )
UI_ScoreM_List <- 
lapply(predictList,function(rec){
  getUI_ScoreM(rec,user_features_1to10,rb_use[1:10])
})


## 
scoreList <- lapply(UI_ScoreM,function(scoreMatrix){
  score <- scoreMatrix
  score[which(is.na(scoreMatrix),arr.ind = T)] <-0
  return(score)
})


score_ans <- Reduce('+',scoreList)
rownames(score_ans) <- rownames(scoreList$recommender_dom) ## naming ##

## remove purchesed items for a user
rowCounts(rb_use[1,])
user1 <- as(rb_use[1,],"matrix")
which(user1 == T) ## 有買的 -- index:140
colnames(user1)[140]
rb_use[1,'310'] ## names : 基金代碼 --310 
## 對有買的品項 score_ans 給 -1 
rowCounts(rb_use[1:10,])
users <- as(rb_use[1:10,],"matrix")
score_ans[which(users == T,arr.ind = T)] <- -1 ## purchased items assign -1

colnames(user1)[c(5,1942)]
# for user 7 056270720 #
# ('120','T34' -- purchased--
# ,'J99' ,'T35' ,'78F','L91' ,'PD0','Z07','BL6') --- recommend ---
which(users == T,arr.ind = T)
## topN items and rating 
topNIndexs1 <- order(score_ans[1,],decreasing = T)[1:10]
score_ans[1,topNIndexs1]
rownames(score_ans)
topNListPredict <- list()
# topNListPredict[[1]] <- 1:10
# topNListPredict[[2]] <- 2:10
namesVec <- rownames(score_ans)
predict_List <- lapply(1:10,function(i){
  orderIndexs <- order(score_ans[i,],decreasing = T)[1:10] # topN index
  topNListPredict[[i]] <- score_ans[i,orderIndexs]
})
names(predict_List) <- namesVec
predict_List ## 

######## 如何存成 TopList Class ?? #########
lapply(predict_List,function(x) unames(x))

itemsets <- colnames(rb_use)
names(predict_List$`033591090`) %in% itemsets 
predict1_items <- names(predict_List$`033591090`)
itemsets[which(itemsets %in% predict1_items)]
predict1_items
## names 對應成 index 
sapply(predict1_items,function(x) which(itemsets==x)) # name->fundid, val->fundindex

# itemsets == names(predict_List$`033591090`) 
predFeaturesTest<-
new("topNList",
    items = lapply(predict_List,function(x) {
      itemNames <- names(x)
      sapply(itemNames,function(item) which(itemsets==item))
      }),
    ratings = lapply(predict_List, function(x){
     unname(x)
    }),
    n = 10
    )

##===================================================
predFeaturesTest@n

unname(predict_List$`033591090`)

items= lapply(predict_List,function(x) {
  itemNames <-names(x)
  sapply(itemNames,function(item) which(itemsets==item))
})
items