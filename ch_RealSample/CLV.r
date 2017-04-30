
# Customer segmentation – LifeCycle Grids with R --------------------------

# from :http://analyzecore.com/2015/02/16/customer-segmentation-lifecycle-grids-with-r/

# loading libraries
library(dplyr)
library(reshape2)
library(ggplot2)

library(tidyverse)

# sample data
# creating data sample
set.seed(10)
data <- data.frame(orderId=sample(c(1:1000), 5000, replace=TRUE),
                   product=sample(c('NULL','a','b','c'), 5000, replace=TRUE,
                                  prob=c(0.15, 0.65, 0.3, 0.15)))

data <- tibble(orderId=sample(c(1:1000),5000,replace=T),
       product=sample(c('NULL','a','b','c'),5000,replace=T,
       prob =c(0.15,0.65,0.3,0.15))
       )


order <- data.frame(orderId=c(1:1000),
                    clientId=sample(c(1:300), 1000, replace=TRUE))

order <-tibble(orderId=c(1:1000),
               clientId=sample(c(1:300),1000,replace=T))
gender <- tibble(clientId=c(1:300),
                 gender=sample(c('male', 'female'), 300, replace=TRUE, prob=c(0.40, 0.60)))
date <- tibble(orderId=c(1:1000),
               orderdate=sample((1:100), 1000, replace=TRUE))
orders <- order %>% 
  inner_join(data,by="orderId") %>% 
  inner_join(gender,by="clientId") %>% 
  inner_join(date,by="orderId") %>% 
  filter(product!="NULL")

orders$orderdate<-as.Date(orders$orderdate,origin="2012-01-01")

rm(data,order,gender,date)
dim(orders) # 4374,5

# reporting date
today <- as.Date('2012-04-11', format='%Y-%m-%d')

# processing data
dcast(orders, orderId + clientId + gender + orderdate ~ product, 
                value.var='product', fun.aggregate=length)

orders %>% 
  dcast(orderId+clientId+gender ~product,
        value.var='product',fun.aggregate=length)

orders <- orders %>% 
  group_by(clientId) %>% 
  mutate(frequency=n(),recency=as.numeric(today-orderdate)) %>% 
  filter(orderdate==max(orderdate)) %>% 
  filter(orderId==max(orderId)) %>% 
  ungroup()

# explore data
ggplot(orders,aes(x=frequency)) +
  theme_bw() +
  geom_histogram(alpha=0.6, binwidth=1) +
  ggtitle("Dustribution by frequency")

ggplot(orders,aes(x=recency)) +
  theme_bw() + 
  geom_histogram(alpha=0.6,binwidth = 1) + 
  ggtitle("Distribution by recency")

