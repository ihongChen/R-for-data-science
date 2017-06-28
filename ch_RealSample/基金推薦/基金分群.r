###
# install.packages("cluster")
# install.packages("Rtsne")
# install.packages("mice")
library(tidyverse)
library(RODBC)
library(cluster) ## 
library(Rtsne) ## dim reduction for visualization
library(mice) ## missing value 
###
conn <- odbcDriverConnect("Driver=SQL Server;Server=dbm_public;Database=project2017;Uid=sa;Pwd=01060728;")

fund_datasets <- sqlQuery(conn,"select * from project2017.dbo.v_基金推薦_基金屬性",stringsAsFactors=F)

# purchase_funds<-purchase_funds %>% as.tibble()

fund_datasets <- fund_datasets %>% as.tibble()
str(fund_datasets)

fund_datasets$`國內外基金註記` <- as.character(fund_datasets$`國內外基金註記`)
fund_datasets$`熱賣基金註記` <- as.character(fund_datasets$`熱賣基金註記`)

fund_datasets

funds <-
fund_datasets %>% 
  select(matches("基金代碼|規模區間|成立幾年|報酬率|基金評等|投資型態別|熱賣基金註記|商品投資屬性|淨值|Sharpe|Beta|國內外"))

str(funds)

summary(funds)

# fund_exclude_sharpe_outlier <- funds %>% filter(Sharpe>-5) %>% select(Sharpe,基金代碼)
# ggplot(fund_exclude_sharpe_outlier,aes(x=Sharpe)) +
#   geom_density() 
# summary(fund_exclude_sharpe_outlier) # sharpe -->中位數0.39 ## summary(funds$Sharpe)

#################################################
####### 手動填補遺失資料 ##
#################################################
# 國內外基金 NA -> 5
table(funds$`國內外基金註記`,exclude=NULL)
funds[is.na(funds$`國內外基金註記` ),c('國內外基金註記')] <- '1'

# 基金目前規模區間, NA -> 13 
table(funds$`基金目前規模區間`,exclude=NULL)
fundForeign <- funds %>% filter(國內外基金註記 == 1) 
fundDomestic <- funds %>% filter(國內外基金註記 == 0)
table(fundDomestic$基金目前規模區間) ## 國內基金規模以 d.10~100億 最多,選擇插入此值
table(fundForeign$基金目前規模區間) ## 國外基金規模以 h.1~5兆 最多,選擇插入此值
# funds[is.na(funds$`基金目前規模區間`),] %>% 
#   left_join(fund_datasets,suffix=c('.x','.y')) %>% View()
  # select(`基金代碼`,`基金規模(台幣/億)`,`熱賣基金註記`)

funds[funds$`國內外基金註記`==1& is.na(funds$`基金目前規模區間`),
      '基金目前規模區間'] <- 'h.1~5兆元台幣'
funds[funds$`國內外基金註記`==0& is.na(funds$`基金目前規模區間`),
      '基金目前規模區間'] <-'d.10~100億元台幣'
table(funds$基金目前規模區間)

# 投資型態別 --> NA: 0
table(funds$投資型態別,exclude=NULL)

# 商品投資屬性 NA: 14
table(funds$商品投資屬性,exclude= NULL) ## 以RR4 種類最多
table(fundForeign$商品投資屬性,exclude= NULL) ## 國外 以RR4 種類最多
funds[is.na(funds$`商品投資屬性`),] ## 國外基金居多,塞入RR4尚屬合理

funds[is.na(funds$`商品投資屬性`),'商品投資屬性'] <- 'RR4'
table(funds$商品投資屬性,exclude=NULL)

#################################################### 
## 中位數填補空缺值 
####################################################
summary(funds)
# Beta NA:64
funds[which(is.na(funds$Beta)),]$Beta <-median(funds$Beta,na.rm=T)
# 淨值 NA: 5
summary(funds$淨值,na.rm=T) ## left skew 
funds[which(is.na(funds$`淨值`)),]$淨值 <- median(funds$淨值,na.rm=T)
# sharpe beta --> 64 NA
funds[which(is.na(funds$Sharpe)),]$Sharpe <- median(funds$Sharpe,na.rm=T) 
# 一個月累積報酬率(%) --> NA:5
funds[which(is.na(funds$`一個月累積報酬率(%)`)),]$`一個月累積報酬率(%)` <-median(funds$`一個月累積報酬率(%)`,na.rm=T)

# 三個月累積報酬率(%) --> NA:19
funds[which(is.na(funds$`三個月累積報酬率(%)`)),]$`三個月累積報酬率(%)` <-
  median(funds$`三個月累積報酬率(%)`,na.rm=T)
# 六個月累積報酬率(%) --> NA:44
funds[which(is.na(funds$`六個月累積報酬率(%)`)),]$`六個月累積報酬率(%)` <-
  median(funds$`六個月累積報酬率(%)`,na.rm=T)

# 一年累積報酬率(%) --> NA:64
funds[which(is.na(funds$`一年累積報酬率(%)`)),]$`一年累積報酬率(%)` <-
  median(funds$`一年累積報酬率(%)`,na.rm=T)

# 三年累積報酬率(%) --> NA:393
funds[which(is.na(funds$`三年累積報酬率(%)`)),]$`三年累積報酬率(%)` <-
  median(funds$`三年累積報酬率(%)`,na.rm=T)

# 五年累積報酬率(%) --> NA:797
funds[which(is.na(funds$`五年累積報酬率(%)`)),]$`五年累積報酬率(%)` <-
  median(funds$`五年累積報酬率(%)`,na.rm=T)

# 自今年以來報酬率(%) --> NA:6
funds[which(is.na(funds$`自今年以來報酬率(%)`)),]$`自今年以來報酬率(%)` <-
  median(funds$`自今年以來報酬率(%)`,na.rm=T)

# 自成立日起報酬率(%) --> NA:588
funds[which(is.na(funds$`自成立日起報酬率(%)`)),]$`自成立日起報酬率(%)` <-
  median(funds$`自成立日起報酬率(%)`,na.rm=T)

# 基金評等 --> NA:423
funds[which(is.na(funds$`基金評等`)),]$`基金評等` <-
  median(funds$`基金評等`,na.rm=T)

## 基金成立幾年 NA:6
funds[is.na(funds$`基金成立幾年`),'基金成立幾年'] <- floor(mean(funds$基金成立幾年,na.rm=T))

## 檢查 ##
# md.pattern(funds) %>% View()

######### 
# 2777 * 16 Variables #

#### distance ####
summary(funds)
funds[,-1]
mfunds <- funds[,-1] 

summary(mfunds)


mfunds<-
mfunds %>% mutate_each(funs(factor),國內外基金註記) %>% 
  mutate_each(funs(ordered),基金目前規模區間) %>% 
  mutate_each(funs(factor),熱賣基金註記) %>% 
  mutate_each(funs(factor),投資型態別) %>% 
  mutate_each(funs(factor),商品投資屬性)

rownames(mfunds)<-funds[1]$基金代碼
gower_distance <- daisy(mfunds,metric = "gower")

summary(gower_distance)

gower_mat <-
  gower_distance %>% as.matrix() 

gower_mat[1:5,1:5] # check 
### 儲存 gower_distance, 
## fund_datasets==> 原始資料,funds ==> 填補空值,mfund ==> 計算gower_distance用
save(fund_datasets,funds,mfunds,gower_distance,file="./模型評估/fundsDistance.RData")
# Output most similar pair
fund_datasets[
  which(gower_mat == min(gower_mat[gower_mat != min(gower_mat,na.rm=T)],na.rm=T),
        arr.ind = TRUE)[1, ], ] %>% glimpse()

# most dis-similar pair
funds[
  which(gower_mat == max(gower_mat[gower_mat != max(gower_mat,na.rm=T)],na.rm=T),
        arr.ind = TRUE)[1, ], ] %>% glimpse()

#################################################
##### Choose a cluster algo ######
#################################################

## PAM 
## choosing the number of k 
set.seed(1)
sil_width <- c(NA)
for (i in 2:10){
  pam_fit <- pam(gower_distance,diss=T,k=i)
  sil_width[i] <- pam_fit$silinfo$avg.width
}
## plot sihoutte width (higher is better)

sil_width
plot(1:10,sil_width,xlab="Number of clusters",ylab="Silhoutte Width")
lines(1:10,sil_width)


## k=3 is best ## 

pam_fit <-pam(gower_distance,diss=T,k=7)
mfunds
# mfunds %>% select(-1)
pam_result <- fund_datasets %>% 
  mutate_each(funs(factor),國內外基金註記) %>% 
  mutate_each(funs(factor),基金目前規模區間) %>% 
  mutate_each(funs(factor),基金公司代碼) %>% 
  mutate_each(funs(factor),計價幣別) %>% 
  mutate_each(funs(factor),基金經理人) %>% 
  mutate_each(funs(factor),區域別) %>% 
  mutate_each(funs(factor),基金投資產業分類1) %>%
  mutate_each(funs(factor),基金投資產業分類2) %>% 
  mutate_each(funs(factor),基金投資產業分類3) %>% 
  mutate_each(funs(factor),保本型基金註記) %>% 
  mutate_each(funs(factor),高收益債註記) %>% 
  mutate_each(funs(factor),熱賣基金註記) %>% 
  mutate_each(funs(factor),投資型態別) %>% 
  mutate_each(funs(factor),商品投資屬性) %>% 
  mutate(cluster = pam_fit$clustering) %>% 
  group_by(cluster) %>% 
  do(the_summary = summary(.))

print(pam_result$the_summary[[4]])

# 寫入到clipboard 
summary1cluster_df <- data.frame(unclass(pam_result$the_summary[[1]]),check.names = F)
summary2cluster_df <- data.frame(unclass(pam_result$the_summary[[2]]),check.names = F)
summary3cluster_df <- data.frame(unclass(pam_result$the_summary[[3]]),check.names = F)

write.table(summary3cluster_df,"clipboard",sep="\t",row.names = F)
# glimpse(fund_datasets[c(1693,1081,1718),])

### 分群存回sql
fund_cluster <- fund_datasets %>% mutate(cluster = pam_fit$clustering) %>% 
  select(matches('基金代碼|cluster'))

sqlSave(conn,fund_cluster,tablename="基金推薦_基金分群",rownames=F)
### visulize with t-sne
tsne_obj <- Rtsne(gower_distance,is_distance = T)
tsne_data <- tsne_obj$Y %>% as.tibble() %>%
  set_names(c("X","Y")) %>%
  mutate(cluster = factor(pam_fit$clustering),
         name = funds$基金代碼)

ggplot(tsne_data,aes(x=X,y=Y)) +
  geom_point(aes(color=cluster))

