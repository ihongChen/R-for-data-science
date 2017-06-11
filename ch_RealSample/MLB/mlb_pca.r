### source : https://rpubs.com/skydome20/R-Note7-PCA
### handle MLB


library(tidyverse)

data <- read_csv("2012MLB.csv")
data %>% head()

pca <- prcomp(formula = ~ H1B+H2B+H3B+HR+RBI+SB+BB,
              data = data ,
              scale = T)
plot(pca,type="line",main="2012 MLB with PCA var")
# 用藍線標示出特徵值=1的地方
abline(h=1, col="blue") # Kaiser eigenvalue-greater-than-one rule

vars <- (pca$sdev) **2
props <- vars/sum(vars)

cumulative.props <- cumsum(props)  # 累加前n個元素的值
cumulative.props

#當我們取前三個主成份，可以解釋 70.64% 的變異
cumulative.props[3]

plot(cumulative.props)

top3_pca.data <- pca$x[, 1:3]
top3_pca.data 

### 特徵向量 

pca$rotation

top3_pca.data.eigenvector <-pca$rotation[,1:3]
top3_pca.data.eigenvector
first.pca <- top3_pca.data.eigenvector[, 1]   #  第一主成份
second.pca <- top3_pca.data.eigenvector[, 2]  #  第二主成份
third.pca <- top3_pca.data.eigenvector[, 3]   #  第三主成份

first.pca[order(first.pca, decreasing=FALSE)]  
second.pca[order(second.pca,decreasing = F)]

# 使用dotchart，繪製主成份負荷圖
dotchart(first.pca[order(first.pca, decreasing=FALSE)] ,   # 排序後的係數
         main="Loading Plot for PC1",                      # 主標題
         xlab="Variable Loadings",                         # x軸的標題
         col="red")                                        # 顏色

# pca$rotation 
top3_pca.data <- pca$x[, 1:3]
top3_pca.data 


# 選取 PC1 和 PC2 繪製主成份負荷圖
biplot(pca, choices=1:2)  
summary(pca)


