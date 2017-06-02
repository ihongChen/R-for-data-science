## 處理數據丟失 ####
## code :http://www.xueqing.tv/cms/article/98

data <- airquality
data[4:10,3]<-rep(NA,7)
data[4:10,]
data[1:5,4] <-NA
summary(data)

pMiss <- function(x) {sum(is.na(x))/length(x)*100}
apply(data,2,pMiss)
sapply(data,pMiss)


library(mice)
md.pattern(data)

install.packages("VIM")
library(VIM)