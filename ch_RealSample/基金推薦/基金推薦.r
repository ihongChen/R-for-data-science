
# 基金推薦--協同過濾法-- -----------------------------------------------------------

## 套件引用
library(RODBC)
library(tidyverse)
library(recommenderlab)
library(reshape2)
library(data.table)
## 連結資料庫
conn <- odbcDriverConnect("Driver=SQL Server;Server=dbm_public;Database=project2017;Uid=sa;Pwd=01060728;")


# 資料源: 基金庫存明細 -------------------------------------------------------------
# 

# 取資料(交易年 >= 2015 )
# sql_fund <- "SELECT * FROM [v_基金推薦_庫存明細] where [交易年(開始)] >= 2015"
# fund=sqlQuery(conn,sql_fund)
# 
# dim(fund) # 131,845 *7
# save(fund,file="fund.RData")
# load('fund.Rdata')
# 
# ## 
# 
# fund1 <- 
#   fund %>%
#   group_by(身分證字號,基金中文名稱) %>%
#   count() %>% 
#   ungroup()
# 
# ###### 用戶持有一檔基金數量 ######
# fund1 <- 
#   fund1 %>%
#   arrange(desc(n))
# 
# # fund1 %>% distinct(基金中文名稱) # 2301 Item
# 
# # fund1 %>% 
# #   distinct(身分證字號) # 55666 user 
# 
# 
# ## id歸戶-每個用戶擁有的基金庫存
# fund2 <-fund1 %>%
#   dcast(身分證字號~基金中文名稱)
# 
# rownames(fund2) <-fund2$身分證字號
# fund2$身分證字號 <-NULL
# 
# # sum(fund2[1,],na.rm=T)
# # dim(fund2) # 55,666*2,301 (user-item)
# 
# rownames(fund2[1,])
# ######### use recommenderLab #########################
# ## 參考 vignette("recommenderlab") 
# 
# 
# 
# # user-item  -------------------------------------------------------------------
# ui_trans_m <- data.matrix(fund2)
# dim(ui_trans_m) # 55798*2318 - UI
# 
# ui_trans <- as(ui_trans_m,"realRatingMatrix")
# # image(ui_trans,main = "U-I table")
# # colCounts(ui_trans)[1:5]
# 
# r_b <- binarize(ui_trans,minRating=0.1) # 55798 * 2318
# r_b <- as(r_b,"binaryRatingMatrix")
# r_bex <-r_b[rowCounts(r_b)>4] # 排除庫存基金數<5
# 
# image(r_b,main="User-Item binary table")
# image(r_bex[rowCounts(r_bex)>10],main="U-I (持有數>10)")
# 
# # 資料探索 --------------------------------------------------------------------
# dim(r_b) # 43,979 users * 2,318 items
# # 檢查資料
# rowCounts(r_b[1,])
# rowCounts(r_b[2,])
# rowCounts(r_b[10,])
# hist(rowCounts(r_b), breaks=100,main = '每人基金持有數',
#      xlim=c(1,20))
# 
# # 用戶持有
# # sort(rowCounts(r_b),decreasing = T)[10000:15000] # 前10000名用戶,持有數至少4檔基金
# own_table <- table(rowCounts(r_b)) #大部分用戶持有僅持有一檔(種)基金
# table(rowCounts(r_bex)) 
# own_table_df <- own_table %>% data.frame() 
# colnames(own_table_df) <- c("基金數","人數")
# own_table_df %>% View()
# 
# total <- 
#   own_table_df %>% summarise(總人數=sum(人數))
# 
# own_table_df %>% 
#   mutate(perc = 人數/total$總人數)
# 
# write.csv(own_table_df,"fundsOwn_table.csv",row.names = FALSE)
# # 物品相似度 -------------------------------------------------------------------
# 
# # 
# # # simItem_table <- similarity(r_b,method="cosine",which="items") ## 基於cosine相似度
# # simItem_table <- similarity(r_b,method="jaccard",which="items") ## 基於jaccard相似度
# # simItem_table_M <-as(simItem_table,"matrix")
# # ## 僅涵蓋購買基金數>4, 共計有7125人
# # simItem_table_ex <- similarity(r_bex,method="jaccard",which="items")
# # simItem_table_exM <- as(simItem_table_ex,"matrix")
# # simItem_table_ex_exclude <- ifelse(simItem_table_exM<0.01,NA,simItem_table_exM)
# # 
# # simItem_sparse_ex <-as(simItem_table_ex_exclude,"realRatingMatrix")
# # image(simItem_sparse_ex,xlab="Item1",ylab="Item2") # 
# # 
# # 
# # #### 考慮全部購買55,666人
# # simItem_table_exclude <- ifelse(simItem_table_M<0.01,NA,simItem_table_M) #排除<0.01相似度
# # # simItem_table_exclude %>% head() %>% View()
# # simItem_sparse <- as(simItem_table_exclude,"realRatingMatrix")
# # 
# # image(simItem_sparse,xlab="Item-1",ylab="Item-2") ## 物品相似度
# # 
# # rowCounts(simItem_sparse[1,]) ## 23 筆資料
# # rowCounts(simItem_sparse[2,]) ## 6
# # 
# # simItem_df <- as(simItem_sparse,"data.frame")
# # simItem_df %>% head()
# # simItem_df <- simItem_df %>% 
# #   `colnames<-` (c('基金1','基金2','相似度'))
# # 
# # dim(simItem_df)
# # simItem_df %>% head() %>% rownames()
# # 
# # vartypes = c(`基金1` = "varchar(99)",`基金2` = "varchar(99)",`相似度` = "numeric(4,3)")
# # # vartypes
# # ## test 
# # # sqlSave(conn2,simItem_df[sample(nrow(simItem_df),5),],
# # #         tablename = "test2",rownames = FALSE,varTypes=vartypes)
# #   
# # sqlSave(conn,simItem_df,tablename = "基金推薦_基金相似度_J",rownames = FALSE,varTypes=vartypes)
# 
# 
# # library(data.table)
# 
# # simItem_df %>% 
# #   filter(`基金1` %like% '103' ) ## %like% : from data.table
# 
# 
# 
# # 熱門基金 --------------------------------------------------------------------
# 
# hot100Fund <- 
# colCounts(r_b) %>% sort(decreasing= TRUE) %>% head(100)
# 
# hot100Fund_df <- tibble(`基金`=names(hot100Fund),`持有人數`=hot100Fund)
# 
# names(hot100Fund)
# ## write data to csv
# write.table(hot100Fund_df, file = "hot100Fund.csv", sep = ",")
# hot100Fund
# 
# image(simItem_sparse[names(hot100Fund),names(hot100Fund)],
#       xlab='Item1',ylab='Item2', main='熱門基金相似度')
# 
# temp <- simItem_sparse[names(hot100Fund),names(hot100Fund)]
# # as(simItem_sparse[names(hot100Fund),names(hot100Fund)],"matrix") %>% View()
# 
# image(simItem_sparse[1:100,1:100])
# 
# 
# # 推薦模型 --------------------------------------------------------------
# # recommenderRegistry$get_entries(dataType="binaryRatingMatrix")
# # ####### popular ####### 
# # r_popular <- Recommender(r_b[1:45000],method="POPULAR")
# # names(getModel(r_popular))
# # p_popular <- predict(r_popular, r_b[50000:50010], type="topNList",n=5)
# # as(p_popular,"list")
# # 
# # ##### User based ######
# # r_user <- Recommender(r_b[1:45000,],method="UBCF")
# # p_user <- predict(r_user, r_b[50000:50010], type="topNList",n=5)
# # l <- as(p_user,"list")
# # 
# # as(bestN(p_user,n=5),"list")
# # 
# # names(getModel(r_user))
# # 
# # getModel(r_user)
# # 
# # ###### Item based ######
# # r_item <- Recommender(r_b[1:45000,],method="IBCF")
# # p_item <- predict(r_item,r_b[50000:50010],type = "topNList",n=5)
# # as(bestN(p_item,n=5),"list")
# # names(getModel(r_item))
# # 
# # image(getModel(r_item)$sim)
# 
# # 模型測試區 ---------------------------------------------------------------------
# 
# 
# ####################################################################
# ##   算法評估
# ####################################################################
# 
# algorithms <- list(
#   "random items" = list(name="RANDOM"),
#   "popular items" = list(name="POPULAR"),
#   "user-based CF" = list(name="UBCF",param=list(nn=50)),
#   "item-based CF" = list(name="IBCF",param=list(k=50))
#   # "SVD approx" = list(name="SVD",param=list(k=50))
# )
# 
# scheme_rb_split <-evaluationScheme(r_b,method="split",train=0.9,k=1,given=-1)
# ev_result_split <-evaluate(scheme_rb_split,algorithms,type="topNList",n=c(1,3,5,10,20))
# ## 排除持有數<4 === 共7,125 users 2,301 items #
# 
# 
# # r_bex
# 
# scheme_rbex_split <- evaluationScheme(r_bex,method="split",train=0.9,k=1,given=-1) # split
# 
# scheme_rbex_cv <- evaluationScheme(r_bex,method="cross",k=4,given=-1) # cross
# ev_resultEx_split <- evaluate(scheme_rbex_split,algorithms,type="topNList",
#                           n=c(1,3,5,10,20))
# ev_resultEx_cross <- evaluate(scheme_rbex_cv,algorithms,type="topNList",
#                           n=c(1,3,5,10,20))
# 
# plot(ev_result_split,annotate=c(1,3))
# plot(ev_resultEx_split,annotate=c(1,3))
# plot(ev_resultEx_cross,annotate=c(1,3))
# avg(ev_resultEx_cross)
# 
# plot(ev_resultEx_cross,annotate = c(2,3),"prec/rec",legend="topleft",ylim=c(0,0.1))
# save(ev_resultEx_split,ev_resultEx_cross,file="ev_result.RData")
# load('ev_result.RData')
# 
# ## 找出最佳模型  -- check recall 
# ev_dataList <- avg(ev_resultEx_cross)
# ev_dataList$`popular items`[5,'recall']
# ev_dataList$`user-based CF`[5,'recall']
# 
# recall_compare <- sapply(ev_dataList,`[[`,5,'recall') 
# best_model <- names(which.max(recall_compare))
# if (best_model=='popular items') {
#   best_model <- 'popular'
# } else if (best_model=='user-based CF'){
#   best_model <- 'UBCF'
# } else if (best_model=='item-based CF'){
#   best_model <- 'IBCF'
# }
# 
# 
# 
# # 預測結果 --------------------------------------------------------------------
# 
# ## predict
# 
# # rec_popular <- Recommender(r_bex,method="popular")
# # rec_ubcf <- Recommender(r_bex,method = 'UBCF')
# # rec_ibcf <- Recommender(r_bex,method = "IBCF")
# 
# # pred_popular <- predict(rec_popular, r_bex[1:10], type="topNList",n=5)
# # pred_ubcf <- predict(rec_ubcf,r_bex[1:10],type="topNList",n=5)
# # pred_ibcf <- predict(rec_ibcf,r_bex[1:10],type="topNList",n=5)
# # 
# # as(pred_popular,"list")
# # as(pred_ubcf,"list")
# # as(pred_ibcf,"list")
# # 
# # rowCounts(r_bex[1,])
# # rowCounts(r_bex[2,])
# # best_model = 'UBCF'
# 
# recommender_model <- Recommender(r_bex,method = best_model)
# hot_model <- Recommender(r_b,method="POPULAR")
# pred_result <- predict(recommender_model,r_bex,type="topNList",n=20)
# hot_result <- predict(hot_model,r_b,type="topNList",n=20)
# 
# pred_result_list <- as(pred_result,"list")
# hot_result_list <- as(hot_result,"list")
# 
# # temp <- pred_result_list %>% head(20)
# 
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
# 
#   
# 
# ##
# sqlSave(conn,hot100Fund_df,
#         tablename = "基金推薦_熱門100基金",
#         rownames = F)
# 
# sqlSave(conn,df_exclude,
#         tablename = "基金推薦_個人基金Top20",
#         rownames = "uid")
# 
# sqlSave(conn,df_hot,
#         tablename = "基金推薦_熱門基金Top20",
#         rownames = "uid")
# 
# 
# # test --------------------------------------------------------------------
# 
# ### 大部分UBCF推薦清單是熱門商品!!!
# names <- rownames(df_exclude)
# rownames(df_exclude) <- NULL
# df <- cbind(names,df_exclude)
# 
# test <-
# df %>% 
#   group_by(item1) %>% 
#   summarise(n=n()) %>% 
#   arrange(desc(n))
#   
# 
# write.table(test, "clipboard", sep="\t", row.names=FALSE)
# 



# 資料: 申購基金 ----------------------------------------------------------------


sql_fund_purchase <- "select * from v_基金推薦_申購明細 where [申購登錄年] >= 2015"
fund_purchase <- sqlQuery(conn,sql_fund_purchase)
r_b_purchase <- getUIMatrix(fund_purchase) ## 稀疏矩陣,
save(r_b_purchase,file="模型評估/申購基金ui資料.RData")
# 排除購買基金檔數 > 1, 2, 3, 4, 5 #
r_b_purchase_gt2 <- r_b_purchase[rowCounts(r_b_purchase)>1]
r_b_purchase_gt3 <- r_b_purchase[rowCounts(r_b_purchase)>2]
r_b_purchase_gt4 <- r_b_purchase[rowCounts(r_b_purchase)>3]
r_b_purchase_gt5 <- r_b_purchase[rowCounts(r_b_purchase)>4]

# 評估算法 #
ev5 <- evaluateAlgo(r_b_purchase_gt5)
ev4 <- evaluateAlgo(r_b_purchase_gt4)
ev3 <- evaluateAlgo(r_b_purchase_gt3)
ev2 <- evaluateAlgo(r_b_purchase_gt2)
ev <- evaluateAlgo(r_b_purchase)

save(ev,ev2,ev3,file="模型評估/申購模型評估.RData")

recommenderList_all <- recommenderList(r_b_purchase,'IBCF')
recommenderList_gt2 <- recommenderList(r_b_purchase_gt2,findBestAlgo(ev2))


# helper function  ----------------------------------------------------------------

getUIMatrix <- function(fund) {
  ##### 從基金(申購/庫存)明細資料 
  #### 整理成recommenderLab使用的rating_binaryMatrix (用戶-物品稀疏矩陣)
  fund1 <- 
    fund %>% 
    mutate(fundId=substr(基金中文名稱,1,3)) %>% 
    group_by(身分證字號,fundId) %>% 
    count() %>% 
    ungroup() %>% 
    arrange(desc(n)) %>% 
    dcast(身分證字號~fundId,value.var="n")

  rownames(fund1) <-fund1$身分證字號
  fund1$身分證字號 <-NULL
  
  ### user - item matrix ###
  
  ui_trans <- as(data.matrix(fund1),"realRatingMatrix")
  
  r_b <- binarize(ui_trans,minRating=0.1) # 
  r_b <- as(r_b,"binaryRatingMatrix")
  return(r_b)
}

evaluateAlgo <- function(r_b) {
  ## 評估算法結果
  ## ============
  ## params -- input :binary rating U-I sparse matrix  
  ##        -- 
  
  algorithms <- list(
    "random items" = list(name="RANDOM"),
    "popular items" = list(name="POPULAR"),
    "user-based CF" = list(name="UBCF",param=list(nn=50)),
    "item-based CF" = list(name="IBCF",param=list(k=50))
    # "SVD approx" = list(name="SVD",param=list(k=50)) ## can't work for binary case ....
  )
  
  scheme_rb_split <-evaluationScheme(r_b,method="split",train=0.9,k=1,given=-1)
  ev_result_split <-evaluate(scheme_rb_split,algorithms,type="topNList",n=c(1,3,5,10,20))
  
   
  # scheme_rb_cv <- evaluationScheme(r_bex,method="cross",k=4,given=-1) # cross
  # ev_resultEx_cross <- evaluate(scheme_rbex_cv,algorithms,type="topNList",
  #                               n=c(1,3,5,10,20))
  
  plot(ev_result_split,annotate=c(2,3))
  plot(ev_result_split,annotate=c(2,3),"prec/rec",legend="topleft")
  
  return(ev_result_split)
}

findBestAlgo <- function(ev){
  ### 透過check最大recall值,找出最佳模型 UBCF/IBCF/POPULAR/RANDOM ##
  lengthOfData <- dim(avg(ev$`user-based CF`))[1]
  ev_dataList <- avg(ev)
  recall_compare <- sapply(ev_dataList,`[[`,lengthOfData,'recall') 
  best_model <- names(which.max(recall_compare))
  if (best_model=='popular items') {
    best_model <- 'popular'
  } else if (best_model=='user-based CF'){
    best_model <- 'UBCF'
  } else if (best_model=='item-based CF'){
    best_model <- 'IBCF'
  }
  return(best_model)
}

recommenderList <- function(r_b,best_model){
  ## 給出推薦清單
  recommender_model <- Recommender(r_b,method = best_model)
  print(paste0('best model :',best_model))
  pred_result <- predict(recommender_model,r_b,type="topNList",n=20)
  return(pred_result)
}



hot_model <- Recommender(r_b,method="POPULAR")
pred_result <- predict(recommender_model,r_bex,type="topNList",n=20)
hot_result <- predict(hot_model,r_b,type="topNList",n=20)

pred_result_list <- as(pred_result,"list")
hot_result_list <- as(hot_result,"list")

# temp <- pred_result_list %>% head(20)


### best model --> UBCF --> 推薦
df_t <- t(data.table::as.data.table(pred_result_list))
itemNames <- sapply('item',paste0,c(1:20))[,1]
df_exclude <- as.data.frame(df_t,stringsAsFactors = F);
colnames(df_exclude) <- itemNames;

## 少於5次的 利用最熱銷產品推薦 
hot_dt_t <- t(as.data.table(hot_result_list))

uid_include <- rownames(df_exclude)
df_hot <- as.data.frame(hot_dt_t[!rownames(hot_dt_t) %in% uid_include,],
                        stringsAsFactors = F)
colnames(df_hot) <- itemNames


