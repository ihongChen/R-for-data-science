# ch5 explore data analysis -----------------------------------------------

library(tidyverse)
# 7.3 ---------------------------------------------------------------------
View(diamonds)
ggplot(data=diamonds) + 
  geom_bar(mapping = aes(x=cut))

diamonds %>% 
  count(cut)

# diamonds %>% 
#   count(color,cut)

# continuous variables (use histogram)
ggplot(data=diamonds) + 
  geom_histogram(mapping = aes(x=carat),binwidth = 0.5)

diamonds %>% 
  count(cut_width(carat,0.5)) ### 

# filter with small diamonds
small<-diamonds %>% 
  filter(carat<3) 
ggplot(data=small, mapping = aes(x=carat)) + 
  geom_histogram(binwidth = 0.1)
  
ggplot(data=small,mapping=aes(x=carat,fill=cut)) +
  geom_histogram(binwidth = 0.1)

ggplot(data=small,mapping=aes(x=carat,color=cut)) +
  geom_freqpoly(binwidth=0.1)

ggplot(data=small,mapping=aes(x=carat)) +
  geom_histogram(binwidth=0.01)


# outliers ----------------------------------------------------------------

ggplot(diamonds) + 
  geom_histogram(mapping = aes(x = y), binwidth = 0.5) +
  coord_cartesian(ylim=c(0,10))
  # geom_point(mapping = aes(x,y))

unusual <- diamonds %>% 
  filter(y<3 | y>20) %>% 
  arrange(y)
unusual


# ex ----------------------------------------------------------------------
View(small)
## price avg for a different size of diamonds
## 鑽石價格 --> 切級距
xx <- cut_width(small$price,1000)
small %>% 
  filter(y>3 & y<20) %>% 
  mutate(克拉級距=cut_width(carat,0.5), 
        價格級距 = cut_width(price,10000)) %>% 
  select(carat,price,克拉級距,價格級距) %>% 
  ggplot() + 
    geom_bar(mapping=aes(x = 價格級距,fill = 克拉級距))
## 鑽石價格 --> 切級距 2  
small %>% 
  filter(y>3 & y<20) %>% 
  mutate(克拉級距=cut_width(carat,0.5), 
             價格級距 = cut_width(price,10000)) %>% 
  group_by(克拉級距) %>% 
  summarise(
    平均價格 = mean(price)
  ) %>% 
  ggplot() +
    geom_bar(mapping = aes(x=克拉級距,y=平均價格),stat="identity")
