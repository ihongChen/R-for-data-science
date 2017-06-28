# rm(list = ls())
# .libPaths(c( "C:/Users/Adrian/Documents/R/win-library/3.2", "C:/Program Files/R/R-3.3.2/library"))
# install.packages("cluster")
# install.packages("magrittr")
# install.packages("Rtsne")
# install.packages("tibble")

# library(magrittr)
library(tidyverse)
library(RODBC)
library(cluster) ## 
library(Rtsne) ## dim reduction for visualization
library(stringr)
###
conn <- odbcDriverConnect("Driver=SQL Server;Server=dbm_public;Database=Test;Uid=sa;Pwd=01060728;")

load('../模型評估/申購基金ui資料.RData')

funds_Ids <- r_b_purchase@data@itemInfo$labels ## 2,235 ## 交易紀錄購買基金
funds_base <- sqlQuery(conn,"SELECT 基金代碼 FROM project2017.dbo.v_基金推薦_基金屬性",
                       stringsAsFactors = F) ## MMA 線上販售基金
funds_base <- funds_base$基金代碼

fundBoth <- funds_Ids %in% funds_base

rb_use <- r_b_purchase[,fundBoth] # 45,350 * 2,170 # 原:2,235 ...使用的交易資料...
rb_use <- rb_use[!rowCounts(rb_use)==0,] ## delete no data user , 45,288
# 移除申購數=1的資料 #
rb_use <- rb_use[rowCounts(rb_use)>1] ##
rb_use ## 26,622 * 2170

## 客戶特徵 ##
fund_datasets <- sqlQuery(conn,
                          "select * from Test.dbo.v_Shane_基金客戶分群_基礎",
                          stringsAsFactors=F)

ids <- rownames(rb_use) # 26622
ids_data <- fund_datasets$身分證字號
ids_data <- str_trim(ids_data)
ids[!(ids %in% ids_data)]

fund_datasets <- 
  fund_datasets %>% 
    filter(str_trim(`身分證字號`) %in% ids)
    
fund_datasets$身分證字號 <- ids[ids %in% ids_data]
fund_datasets %>% dim ## 26621 *55 ,1 closed
####################################################################################################################################################################

#### distance ####

mfunds <- fund_datasets[,-c(1:2)] %>% as_tibble()

# summary(mfunds)

mfunds<-  mfunds %>% 
  mutate_each(funs(factor),性別) %>% 
  mutate_each(funs(ordered),投資屬性) %>% 
  mutate_each(funs(factor),存款記號) %>% 
  mutate_each(funs(factor),放款記號) %>% 
  mutate_each(funs(factor),呆帳記號) %>% 
  mutate_each(funs(ordered),投資屬性) %>% 
  mutate_each(funs(factor),基金記號) %>% 
  mutate_each(funs(factor),單筆申購記號) %>% 
  mutate_each(funs(factor),定時定額記號) %>% 
  mutate_each(funs(factor),信用卡記號) %>% 
  mutate_each(funs(factor),人身保險記號) %>% 
  mutate_each(funs(factor),產險記號) %>% 
  mutate_each(funs(factor),證券記號) %>% 
  mutate_each(funs(factor),黃金存摺戶) %>% 
  mutate_each(funs(factor),房貸記號) %>% 
  mutate_each(funs(factor),信貸記號) %>% 
  mutate_each(funs(factor),`KPI往來產品數(持有)`) %>% 
  mutate_each(funs(factor),`KPI往來產品數(使用)`) %>% 
  mutate_each(funs(factor),網路會員記號) %>% 
  mutate_each(funs(factor),`MMA+註記`) %>% 
  mutate_each(funs(ordered),年齡級距) %>% 
  mutate_each(funs(ordered),AUM級距)

# fund_datasets$身分證字號

rownames(mfunds)<-fund_datasets$身分證字號


system.time(
  gower_distance <- 
    daisy(mfunds[,c(1,2,3,19,20,23,25)],
          metric = "gower")
)

# summary(gower_distance)

gower_mat_U <-  gower_distance %>% as.matrix() 
rownames(gower_mat_U) <- rownames(mfunds)
colnames(gower_mat_U) <- rownames(mfunds)

### make sparse ## 
# neighbors <- 100
# for(i in 1:nrow(gower_mat_U))
#   gower_mat_U[i,head(order(gower_mat_U[i,], decreasing=FALSE, na.last=FALSE),
#              ncol(gower_mat_U) - neighbors)] <- 0

gower_mat_U[1:5,1:5] # check 


### 儲存 gower_distance, 
## fund_datasets==> 原始資料,funds ==> 填補空值,mfund ==> 計算gower_distance用
save(gower_mat_U,file="../模型評估/usersDistance.RData")

# Output most similar pair
fund_datasets[
  which(gower_mat_U == min(gower_mat_U[gower_mat_U != min(gower_mat_U,na.rm=T)],na.rm=T),arr.ind = TRUE)[1, ], ] %>% glimpse()

# most dis-similar pair
funds_datasets[
  which(gower_mat_U == max(gower_mat_U[gower_mat_U != max(gower_mat_U,na.rm=T)],na.rm=T),arr.ind = TRUE)[1, ], ] %>% glimpse()

#################################################
##### Choose a cluster algo ######
#################################################

## PAM 
## choosing the number of k 
set.seed(1)
sil_width <- c(NA)
for (i in 2:15){
  pam_fit <- pam(gower_distance,diss=T,k=i)
  sil_width[i] <- pam_fit$silinfo$avg.width
}
## plot sihoutte width (higher is better)

sil_width
plot(1:15,sil_width,xlab="Number of clusters",ylab="Silhoutte Width")
lines(1:15,sil_width)


## k=3 is best ## 

pam_fit <-pam(gower_distance,diss=T,k=7)
pam_fit
# mfunds %>% select(-1)
 pam_result <- mfunds[c(1:5000),c(1,2,3,19,20,23,25)] %>% 
  mutate_each(funs(factor),性別) %>% 
  mutate_each(funs(ordered),投資屬性) %>% 
  # mutate_each(funs(factor),存款記號) %>% 
  # mutate_each(funs(factor),放款記號) %>%
  # mutate_each(funs(factor),呆帳記號) %>%
  # mutate_each(funs(factor),基金記號) %>%
  # mutate_each(funs(factor),單筆申購記號) %>%
  # mutate_each(funs(factor),定時定額記號) %>%
  # mutate_each(funs(factor),信用卡記號) %>%
  # mutate_each(funs(factor),人身保險記號) %>%
  # mutate_each(funs(factor),產險記號) %>%
  # mutate_each(funs(factor),證券記號) %>%
  # mutate_each(funs(factor),黃金存摺戶) %>% 
  # mutate_each(funs(factor),房貸記號) %>% 
  # mutate_each(funs(factor),信貸記號) %>% 
  # mutate_each(funs(factor),`KPI往來產品數(持有)`) %>% 
  mutate_each(funs(factor),`KPI往來產品數(使用)`) %>% 
  # mutate_each(funs(factor),網路會員記號) %>% 
  # mutate_each(funs(factor),`MMA+註記`)  %>% 
  # mutate_each(funs(ordered),年齡級距) %>% 
  # mutate_each(funs(ordered),AUM級距2) %>%
  mutate(cluster = pam_fit$clustering) %>% 
  group_by(cluster) %>% 
  do(the_summary = summary(.))

print(pam_result$the_summary[[1]])

# 寫入到clipboard 
# summary1cluster_df <- data.frame(unclass(pam_result$the_summary[[1]]),check.names = F)
# summary2cluster_df <- data.frame(unclass(pam_result$the_summary[[2]]),check.names = F)
# summary3cluster_df <- data.frame(unclass(pam_result$the_summary[[3]]),check.names = F)
# summary4cluster_df <- data.frame(unclass(pam_result$the_summary[[4]]),check.names = F)
# summary5cluster_df <- data.frame(unclass(pam_result$the_summary[[5]]),check.names = F)
# summary6cluster_df <- data.frame(unclass(pam_result$the_summary[[6]]),check.names = F)
# summary7cluster_df <- data.frame(unclass(pam_result$the_summary[[7]]),check.names = F)
# write.table(summary1cluster_df,"clipboard",sep="\t",row.names = F)


### 分群存回sql
fund_cluster <- fund_datasets[c(1:5000),] %>% mutate(cluster = pam_fit$clustering) %>% 
  select(matches('身份證字號|cluster'))

sqlSave(conn,fund_cluster,tablename="基金推薦_基金客戶",rownames=F)
### visulize with t-sne
tsne_obj <- Rtsne(gower_distance,is_distance = T)
tsne_data <- tsne_obj$Y %>% as.tibble() %>%
  set_names(c("X","Y")) %>%
  mutate(cluster = factor(pam_fit$clustering))
         #name = fund_datasets$`身份證字號`)

ggplot(tsne_data,aes(x=X,y=Y)) +
  geom_point(aes(color=cluster))


write.csv(summary1cluster_df,file="G1.csv")





