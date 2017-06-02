# real world examples from titanic datasets,
## ref from :https://zhuanlan.zhihu.com/p/25185856

library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('mice')  ## missing data
library('dplyr') 
library('randomForest')
library(tidyverse)
library(data.table)
##

train = read.csv('./datasets/train.csv',stringsAsFactors = F) %>% as_data_frame()
test = read.csv('./datasets/test.csv',stringsAsFactors = F) %>% as_data_frame()

str(train)
str(test)

head(train)

full = bind_rows(train,test)
str(full)
summary(full)

# sum(is.na(full$Survived))


# 2. 特徵工程1 --------------------------------------------------

## cleaning data 
## grab title from passenger Name

full$Name
full$Title = gsub('(.*, )|(\\..*)','',full$Name)
table(full$Title)

table(full$Sex,full$Title)
full$Title
# rare title / reassign#
rare_title = c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')
full$Title[full$Title=='Mlle'] = 'Miss'
full$Title[full$Title=='Ms'] = 'Miss'
full$Title[full$Title=='Mme'] = 'Mrs'
full$Title[full$Title %in% rare_title] = 'Rare Title'

## show title counts by sex ...##
table(full$Sex,full$Title)
## grab surname ##
full$Surname <- sapply(full$Name,function(x) {
  strsplit(x,split = "[,.]")[[1]][1]
  })
## 提取姓氏
full$Surname <- NULL
colnames(full)

full$urname = sapply(full$Name,function(x) strsplit(x,split='[,.]')[[1]][1])
as.factor(full$urname) #875 unique first Name
cat(paste('We have <b>', nlevels(factor(full$urname)), '</b> unique surnames. I would be interested to infer ethnicity based on surname --- another time.'))

## b. 家庭狀況影響生存?
# create a family size variable including passenger themselves.
full$Fsize = full$SibSp + full$Parch + 1
full$Family = paste(full$urname,full$Fsize,sep='_')

ggplot(full[1:891,],aes(x=Fsize,fill=factor(Survived))) + 
  geom_bar(stat='count',position='dodge') +
  scale_x_continuous(breaks=c(1:11))+
  labs(x='Family Size')
  # theme_few()

ggplot(full[1:891,],aes(x=Fsize,fill=factor(Survived))) +
  geom_bar(stat='count',position = 'dodge') + 
  labs(x='Family Size')  ## find 1 : 單身存活狀況差...... 


## 離散化 family size 1, [2:4]小家庭, >= 5 大家庭

full$FsizeD[full$Fsize==1] <- 'singleton'
full$FsizeD[full$Fsize <5 & full$Fsize >1] <- 'small'
full$FsizeD[full$Fsize >4] = 'large'

table(full$FsizeD,full$Survived)
mosaicplot(table(full$FsizeD,full$Survived),
           main='Family Size by Survival',shade=T) #馬賽克圖
mosaicplot(table(full$FsizeD,full$Survived),shade=T,main="家族存活圖")  ## find1 :大家庭與單身漢存活率差

## 客艙層數 A,B,C,D,E,F,G,T

full$Cabin[1:28]
sum(full$Cabin =='') #missing value : 1014
length(full$Cabin)
## 
strsplit(full$Cabin[2],NULL)[[1]][1]

full$Deck = factor(sapply(full$Cabin,function(x) strsplit(x,NULL)[[1]][1]))
summary(full$Deck)


## 處理缺失值(中位數)
which(full$Embarked =='')
# full[which(full$Embarked==''),'Embarked']
full[c(62,830),'Embarked'] # missing data ''

## 去除缺失值乘客的id
embark_fare <-
  full %>%
  filter(PassengerId!=62 & PassengerId!=830)


embark_fare %>% 
  group_by(Embarked,Pclass) %>% 
  summarise(mean = mean(Fare,na.rm=T))

ggplot(embark_fare,aes(x=Embarked,y=Fare,fill=factor(Pclass)))+
  geom_boxplot() +
  geom_hline(aes(yintercept=80) ,
             color='red',linetype='dashed',lwd=0.5) +
  scale_y_continuous(labels=dollar_format()) + 
  theme_few() ## 畫圖明顯的c 中位數為80

embark_fare


full$Pclass[c(63,830)]
full$Fare[c(62,830)]

full$Embarked[c(62,830)] = 'C'

# 票價缺失
glimpse(full[1044,]) ## 從S出發 Pclass=3

## 觀察從S出發,三等艙乘客
full[full$Pclass=='3'&full$Embarked=='S',]$Fare %>% median(na.rm=T)
ggplot(full[full$Pclass=='3' & full$Embarked =='S',],
       aes(x=Fare)) +
  geom_density(alpha=0.2,fill='blue') + 
  geom_vline(aes(xintercept=median(Fare,na.rm=T)),color='red',
             linetype='dashed') +
  scale_x_continuous(labels=dollar_format()) ## 利用中位數取代缺失值是合理的!
  # theme_bw() 

# Replace missing fare value with median fare for class/embarkment
full$Fare[1044] <- median(full[full$Pclass == '3' & full$Embarked == 'S', ]$Fare, na.rm = TRUE)
full[full$Pclass=='3',]

## 年齡缺失 
sum(is.na(full$Age)) ## LARGE !!!
# 利用rpart (mice package)來作缺失值預測

# Make variables factors into factors
factor_vars = 
  c('PassengerId','Pclass','Sex','Embarked','Title',
  'urname','Family','FsizeD')
full[factor_vars] =
  lapply(full[factor_vars],function(x) as.factor(x))

usecol <- !names(full) %in% c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived')
full[,usecol ]
# seed 
set.seed(129)
## 多重補插法,剔除沒有用的變量
mice_mode = mice(full[,!names(full) %in% 
                        c('PassengerId','Name','Ticket','Cabin','Family','urname','Survived')],
                 method='rf')
## 保存輸出
mice_output= complete(mice_mode)
head(mice_output)
# 繪製年齡分佈,確保沒有發生偏移失真
ggplot(mice_output,aes(x=Age)) +
  geom_histogram()
par(mfrow=c(1,2))
hist(full$Age,freq=F,main='Age: Original Data',
     col='darkgreen',ylim=c(0,0.04))
hist(mice_output$Age, freq=F, main='Age: MICE output',
     col='green',ylim=c(0,0.04))
## replace age,

full$Age = mice_output$Age
sum(is.na(full$Age))

###特徵工程2 ####

## 按照年齡區分, 
# 兒童<18, 
# 母親:
# 1女性, 2年齡>18, 3.擁有超過一個子女 4.頭銜不是Miss

ggplot(full[1:891,],aes(Age,fill=factor(Survived))) +
  geom_histogram()+
  facet_grid(.~Sex) +
  theme_few()

## 生成兒童變量,並且區分child,adult
full$Child[full$Age<18] = 'Child'
full$Child[full$Age>=18] = 'Adult'
table(full$Child,full$Survived)
## 生成母親變量
full$Mother = 'NotMother'
full$Mother[full$Sex=='female' & full$Parch>0 &
              full$Age>18 & full$Title !='Miss']  = 'Mother'
table(full$Mother,full$Survived)
# Finish by factorizing our two new factor variables
full$Child  <- factor(full$Child)
full$Mother <- factor(full$Mother)


md.pattern(full)

### 4. 建立模型   ####
train = full[1:891,]
test = full[892:1309,]

set.seed(754)
rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + Age + 
                           SibSp + Sex + Parch + Fare + Embarked + Title + FsizeD + 
                           Child + Mother, data = train)
# 
# rf_model = randomForest(factor(Survived)~ Pclass+Sex+Age+SibSp+ Sex*Parch +
#                           Fare + Embarked + Title + FsizeD +
#                           Child + Mother, data=train)
par(mfrow=c(1,1))
plot(rf_model,ylim=c(0,0.36))

legend('topright',colnames(rf_model$err.rate),col=1:3,fill=1:3)

## 變量選擇 (超重要喔!!)#####

importance = importance(rf_model)
varImportance = data.frame(Variables=row.names(importance),
                           Importance=round(importance[,"MeanDecreaseGini"],2))
rankImportance = varImportance %>%
  mutate(Rank=paste0('#',dense_rank(desc(Importance))))


ggplot(rankImportance,
       aes(x=reorder(Variables,Importance), 
           y= Importance, fill=Importance)) +
  geom_bar(stat='identity') +
  # geom_histogram(stat='identity')
  geom_text(aes(x=Variables,y=0.5,label=Rank),
            hjust=0,vjust=0.55,size=4,color='red') +
  coord_flip()

## predict ##
prediction = predict(rf_model,test)

solution = data.frame(PassengerID=test$PassengerId,Survived=prediction,row.names = NULL)

head(solution)
