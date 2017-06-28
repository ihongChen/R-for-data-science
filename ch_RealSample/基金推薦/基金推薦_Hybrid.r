
# 基金推薦--協同過濾法-- -----------------------------------------------------------

## 套件引用
library(RODBC)
library(tidyverse)
library(recommenderlab)
library(reshape2)
library(data.table)

## 連結資料庫
conn <- odbcDriverConnect("Driver=SQL Server;Server=dbm_public;Database=project2017;Uid=sa;Pwd=01060728;")
load('./模型評估/fundsDistance.RData')
load('./模型評估/申購基金ui資料.RData')

fund_ids <- r_b_purchase@data@itemInfo$labels ## 2,235
base_ids <- funds$基金代碼 ## 2,777
both_ids <-  base_ids %in% fund_ids 
both_ids2 <- fund_ids %in% base_ids
gower_mat <- as.matrix(gower_distance)
gower_mat <- gower_mat[both_ids,both_ids]
gower_mat %>% dim() # 2,170

rb_use <- r_b_purchase[,both_ids2] ## 45,350 * 2,170
rb_use <- rb_use[!rowCounts(rb_use)==0,] ## delete none data user
rb_use <- rb_use[rowCounts(rb_use)>1,]
################################################
#### build model ######
################################################
set.seed(100)
which_train <- sample(x = c(TRUE, FALSE),
                      size = nrow(rb_use),
                      replace = TRUE,
                      prob = c(0.8, 0.2))
recc_data_train <-rb_use[which_train,]
recc_data_test <-rb_use[!which_train,]

recc_model <- Recommender(data = recc_data_train,
                          method = "IBCF",
                          parameter = list(method = "Jaccard"))

dim(recc_model@model$sim)
image(recc_model@model$sim)
range(recc_model@model$sim)

###### Hybrid item sim ######

simIBCF <- as(recc_model@model$sim,"matrix")
simIContents <- 1- gower_mat

weight.Content <- 0.5
sim_tot <-  simIContents* weight.Content +  simIBCF* (1 - weight.Content)
# image(sim_tot)
recc_model@model$sim <- as(sim_tot, "dgCMatrix")

################ evaluation ##############

eval_sets <- evaluationScheme(data = rb_use,
                              method = "cross-validation",
                              k = 3,
                              given = -1)


eval_prediction <- predict(object = recc_model,
                           newdata = getData(eval_sets,"known"),
                           n = 10, # item to recommend
                           type = "topNList")
eval_accuracy <- calcPredictionAccuracy(
  x = eval_prediction,
  data = getData(eval_sets, "unknown"),
  byUser = FALSE,
  given = 10)

eval_accuracy

## 評估Hybrid模型 ###

evaluateModel <- function(recc_data,item_dist,
                          number_neighbors=20,
                          k=5,
                          weight.Content=0.5,
                          items_to_recommend=20){
  #####################################################################
  ## recc_data : binaryRatingMatrix with transaction data(U-I matrix) 
  ## item_dist : items matrix measure pairwise distance, 
  ## number_neighbors :  
  ## items_to_recommend: (20)
  ##  k: default=5 , k-fold validation, 
  ##  weight.Content : default=0.5, weight for content 
  ####################################################################
  
  eval_sets <- evaluationScheme(data=recc_data,
                                method="cross-validation",
                                k=k, ## k:kfold validate
                                given=-1) ##
  
  recc_model <- Recommender(data = getData(eval_sets, "train"),
                            method = "IBCF",
                            parameter = list(method = "Jaccard",
                                             k = number_neighbors))
  #### Hybrid similarity ####
  sim_cf <- as(recc_model@model$sim, "matrix") ## similarity from CF model
  sim_contents <- 1- item_dist ## similarity from CONTENT 
  sim_tot <-  sim_contents*weight.Content +  sim_cf*(1 - weight.Content)
  recc_model@model$sim <- as(sim_tot, "dgCMatrix")
  
  #### evaluate ####
  eval_prediction <- predict(object = recc_model,
                             newdata=getData(eval_sets,"known"), 
                             n=items_to_recommend,
                             type="topNList")
  eval_accuracy <- calcPredictionAccuracy(x = eval_prediction,
                                          data = getData(eval_sets,"unknown"),
                                          byUser = F,
                                          given = items_to_recommend)
  print(eval_accuracy[c('precision','recall','TPR','FPR')])
  return(eval_accuracy)
}

w <- seq(0,1,0.1)

w_index <- sapply(seq(1:10),function(x) paste0('weight_',x))

eva_resultLists<- lapply(w,function(x){
  result <- evaluateModel(rb_use,gower_mat,weight.Content=x)
  eva_resultLists <- tibble(eva = result)
  })

eva_df <- sapply(eva_resultLists,'[[',"eva")[5:8,] %>% as_tibble()
colnames(eva_df) <- w
rownames(eva_df) <- c('prec','recall','TPR','FPR')
eva_df <- t(eva_df) # matrix
eva_df <- eva_df %>% as_tibble() %>% mutate(w=w)
ggplot(eva_df,aes(x=w,y=TPR)) + geom_smooth()
qplot(x=w,y=TPR,data=eva_df) + geom_smooth() + 
  scale_x_continuous(name="weight", limits=c(0, 1)) +
  ggtitle('Weight of item-content') +
  annotate("text", x=0.9, y=0.2, label= "item#: 20,\ncv: 5,\nnn: 20") 

# qplot(x=FPR,y=TPR,data=eva_df)

#################################################################################
#### Predict results
#################################################################################

hybrid_reccList <- function(r_b,item_dist,weight.Content){
  ## 給出混合式IBCF推薦清單
  recc_model <- Recommender(r_b,method = "IBCF")
  
  sim_cf <- as(recc_model@model$sim, "matrix") ## similarity from CF model
  sim_contents <- 1- item_dist ## similarity from CONTENT 
  sim_tot <-  sim_contents*weight.Content +  sim_cf*(1 - weight.Content)
  recc_model@model$sim <- as(sim_tot, "dgCMatrix")
  
  pred_result <- predict(recc_model,r_b,type="topNList",n=20)
  return(pred_result)
}


predictList <- hybrid_reccList(rb_use,gower_mat,weight.Content=0.1)

df_t <- t(data.table::as.data.table(as(predictList,"list")))
itemNames <- sapply('基金',paste0,c(1:20))[,1]
df_HybridRecc <- df_t
colnames(df_HybridRecc) <- itemNames
df_upload <- as_data_frame(df_HybridRecc) %>% mutate(id = ids)
df_upload %>% head()

sqlSave(conn,
        df_upload,
        tablename="基金推薦_物品混合式_全清單",
        rownames = F)
#################################################################################
# 資料: 申購基金 ----------------------------------------------------------------
#################################################################################

# sql_fund_purchase <- "select * from v_基金推薦_申購明細 where [申購登錄年] >= 2015"
# fund_purchase <- sqlQuery(conn,sql_fund_purchase)
# r_b_purchase <- getUIMatrix(fund_purchase) ## 稀疏矩陣,
# save(r_b_purchase,file="模型評估/申購基金ui資料.RData")
# 排除購買基金檔數 > 1, 2, 3, 4, 5 #
# r_b_purchase_gt2 <- r_b_purchase[rowCounts(r_b_purchase)>1]
# r_b_purchase_gt3 <- r_b_purchase[rowCounts(r_b_purchase)>2]
# r_b_purchase_gt4 <- r_b_purchase[rowCounts(r_b_purchase)>3]
# r_b_purchase_gt5 <- r_b_purchase[rowCounts(r_b_purchase)>4]
# 
# # 評估算法 #
# ev5 <- evaluateAlgo(r_b_purchase_gt5)
# ev4 <- evaluateAlgo(r_b_purchase_gt4)
# ev3 <- evaluateAlgo(r_b_purchase_gt3)
# ev2 <- evaluateAlgo(r_b_purchase_gt2)
# ev <- evaluateAlgo(r_b_purchase)
# 
# save(ev,ev2,ev3,file="模型評估/申購模型評估.RData")
# 
# recommenderList_all <- recommenderList(r_b_purchase,'IBCF')
# recommenderList_gt2 <- recommenderList(r_b_purchase_gt2,findBestAlgo(ev2))
# 
# 
# ##########################################################################
# ### IBCF ,UBCF, POPULAR,
# ##########################################################################
# hot_model <- Recommender(r_b,method="POPULAR")
# pred_result <- predict(recommender_model,r_bex,type="topNList",n=20)
# hot_result <- predict(hot_model,r_b,type="topNList",n=20)
# 
# pred_result_list <- as(pred_result,"list")
# hot_result_list <- as(hot_result,"list")
# 
# ### best model --> UBCF --> 推薦
# df_t <- t(data.table::as.data.table(pred_result_list))
# itemNames <- sapply('item',paste0,c(1:20))[,1]
# df_exclude <- as.data.frame(df_t,stringsAsFactors = F);
# colnames(df_exclude) <- itemNames;
# 
# ## 少於5次的 利用最熱銷產品推薦 
# hot_dt_t <- t(as.data.table(hot_result_list))
# 
# uid_include <- rownames(df_exclude)
# df_hot <- as.data.frame(hot_dt_t[!rownames(hot_dt_t) %in% uid_include,],
#                         stringsAsFactors = F)
# colnames(df_hot) <- itemNames

#################################################################################
# helper function  ----------------------------------------------------------------
#################################################################################
# getUIMatrix <- function(fund) {
#   ##### 從基金(申購/庫存)明細資料
#   #### 整理成recommenderLab使用的rating_binaryMatrix (用戶-物品稀疏矩陣)
#   fund1 <-
#     fund %>%
#     mutate(fundId=substr(基金中文名稱,1,3)) %>%
#     group_by(身分證字號,fundId) %>%
#     count() %>%
#     ungroup() %>%
#     arrange(desc(n)) %>%
#     dcast(身分證字號~fundId,value.var="n")
# 
#   rownames(fund1) <-fund1$身分證字號
#   fund1$身分證字號 <-NULL
# 
#   ### user - item matrix ###
# 
#   ui_trans <- as(data.matrix(fund1),"realRatingMatrix")
# 
#   r_b <- binarize(ui_trans,minRating=0.1) # 55798 * 2318
#   r_b <- as(r_b,"binaryRatingMatrix")
#   return(r_b)
# }
# 
# evaluateAlgo <- function(r_b) {
#   ## 評估算法結果
#   ## ============
#   ## params -- input :binary rating U-I sparse matrix
#   ##        --
# 
#   algorithms <- list(
#     "random items" = list(name="RANDOM"),
#     "popular items" = list(name="POPULAR"),
#     "user-based CF" = list(name="UBCF",param=list(nn=50)),
#     "item-based CF" = list(name="IBCF",param=list(k=50))
#     # "SVD approx" = list(name="SVD",param=list(k=50)) ## can't work for binary case ....
#   )
# 
#   scheme_rb_split <-evaluationScheme(r_b,method="split",train=0.9,k=1,given=-1)
#   ev_result_split <-evaluate(scheme_rb_split,algorithms,type="topNList",n=c(1,3,5,10,20))
# 
# 
#   # scheme_rb_cv <- evaluationScheme(r_bex,method="cross",k=4,given=-1) # cross
#   # ev_resultEx_cross <- evaluate(scheme_rbex_cv,algorithms,type="topNList",
#   #                               n=c(1,3,5,10,20))
# 
#   plot(ev_result_split,annotate=c(2,3))
#   plot(ev_result_split,annotate=c(2,3),"prec/rec",legend="topleft")
# 
#   return(ev_result_split)
# }
# 
# findBestAlgo <- function(ev){
#   ### 透過check最大recall值,找出最佳模型 UBCF/IBCF/POPULAR/RANDOM ##
#   lengthOfData <- dim(avg(ev$`user-based CF`))[1]
#   ev_dataList <- avg(ev)
#   recall_compare <- sapply(ev_dataList,`[[`,lengthOfData,'recall')
#   best_model <- names(which.max(recall_compare))
#   if (best_model=='popular items') {
#     best_model <- 'popular'
#   } else if (best_model=='user-based CF'){
#     best_model <- 'UBCF'
#   } else if (best_model=='item-based CF'){
#     best_model <- 'IBCF'
#   }
#   return(best_model)
# }
# 
# recommenderList <- function(r_b,best_model){
#   ## 給出推薦清單
#   recommender_model <- Recommender(r_b,method = best_model)
#   print(paste0('best model :',best_model))
#   pred_result <- predict(recommender_model,r_b,type="topNList",n=20)
#   return(pred_result)
# }
# 
# 
# 
# 
