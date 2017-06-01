# install.packages("ISLR")
## code from: https://dpmartin42.github.io/blogposts/r/cluster-mixed-types
### categorial & continuous data

set.seed(1680) # for reproducibility

library(dplyr) # for data cleaning
library(ISLR) # for college dataset
library(cluster) # for gower similarity and pam
library(Rtsne) # for t-SNE plot
library(ggplot2) # for visualization

##### data cleaning #####

College %>% class()
college_tib <- College %>% as_tibble()

str(college_tib)
head(college_tib) %>% View()

college_clean <- college_tib %>% 
  mutate(name=row.names(.),
    accept_rate = Accept/Apps,
    isElite = cut(Top10perc,
                  breaks = c(0,50,100),
                  labels = c("Not Elite", "Elite"),
                  include.lowest=TRUE)) %>% 
  select(name,accept_rate,Outstate,Enroll,Grad.Rate,Private,isElite)

glimpse(college_clean)

#### distance #####
summary(college_clean)
# note : Enroll is postive skew ,log ratio is require
gower_distance <- daisy(college_clean[,-1],
                        metric = "gower",
                        type= list(logratio=3))
summary(gower_distance)

gower_mat <-
gower_distance %>% as.matrix() 

# Output most similar pair
college_clean[
  which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),
        arr.ind = TRUE)[1, ], ]

which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),
      arr.ind=T) ## exclude 0 value

# most dis-similar pair
college_clean[
  which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]),
        arr.ind = TRUE)[1, ], ]


##### Choose a cluster algo ######

## PAM 
## choosing the number of k 
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

pam_fit <-pam(gower_distance,diss=T,k=3)

pam_result<-college_clean %>% 
  select(-name) %>% 
  mutate(cluster = pam_fit$clustering) %>% 
  group_by(cluster) %>% 
  do(the_summary = summary(.))

print(pam_result$the_summary)


college_clean[pam_fit$medoids,]


### visulize with t-sne
tsne_obj <- Rtsne(gower_distance,is_distance = T)
tsne_data <- tsne_obj$Y %>% as.tibble() %>% 
  set_names(c("X","Y")) %>% 
  mutate(cluster = factor(pam_fit$clustering),
         name = college_clean$name)

ggplot(tsne_data,aes(x=X,y=Y)) +
  geom_point(aes(color=cluster))
 