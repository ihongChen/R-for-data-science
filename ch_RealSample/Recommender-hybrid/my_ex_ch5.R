##### 
library(tidyverse)
library(recommenderlab)
library(data.table)
library(countrycode)
col_types = 
  cols(
    mpg = col_double(),
    cyl = col_integer(),
    disp = col_double(),
    hp = col_integer(),
    drat = col_double(),
    vs = col_integer(),
    wt = col_double(),
    qsec = col_double(),
    am = col_integer(),
    gear = col_integer(),
    carb = col_integer()
  )

table_in <- read_csv("./anonymous-msweb.txt",
                     col_names = F
                       )
table_in[,1:3]
### define users table ###
setnames(table_in,1:2,c("category","value"))
table_users <- table_in[,1:2]  

table_users <-
table_users %>% 
  filter(category %in% c("C","V") )
###################################################
#### define rating matrix ###
##################################################
# table_users[, chunk_user := cumsum(category == "C")] ## only work for Data.table
table_users <- 
table_users %>% 
  # filter(category=="C") %>%
  mutate(chunk_user = cumsum(category=="C"))

table_users2 <- table_users %>% filter(category=='C')

temp2 <- temp %>% 
  filter(category=="C")

table_long<-
table_users2 %>% right_join(table_users,by = c("chunk_user")) %>% 
  mutate(user = value.x,item = value.y) %>% 
  filter(category.y=='V') %>% 
  select(chunk_user,user,item)

head(table_long) ### transaction data (long).... ###

### transform to user-item format ###

temp <- table_long[1:100,]
temp2<-
temp %>% dcast(chunk_user+user ~ paste0("item_",item),drop=T) %>% 
  select(starts_with("item_")) 

temp2[!is.na(temp2)] <- 1
temp2

table_wide <- 
table_long %>% dcast(chunk_user+user ~ paste0("item_",item),drop=T)
  
rownames(table_wide) <- table_wide[,"user"] 
table_wide[,"user"] <- NULL
table_wide[,"chunk_user"] <- NULL
# colnames(table_wide)[1:5]
# rownames(table_wide)[1:5]
table_wide[!is.na(table_wide)] <- 1
table_wide[1:5,1:5] ### u-i data.frame 

matrix_wide <- as.matrix(table_wide)
colnames(matrix_wide)
# head(matrix_wide[,1:6])
matrix_wide %>% dim()

matrix_wide[is.na(matrix_wide)] <- 0
ratings_matrix <- as(matrix_wide,"binaryRatingMatrix")
ratings_matrix ### convert to binaryRatingMatrix ... sparse

image(ratings_matrix[1:50, 1:50], main = "Binary rating matrix")

#####

n_users <- colCounts(ratings_matrix)
qplot(n_users) 

qplot(n_users[n_users < 100]) + stat_bin(binwidth = 10) +
  ggtitle("Distribution of the number of users")

ratings_matrix <- 
ratings_matrix[,colCounts(ratings_matrix)>=5]
colCounts(ratings_matrix)

sum(rowCounts(ratings_matrix)==0)
ratings_matrix <- ratings_matrix[rowCounts(ratings_matrix) >= 5, ]

ratings_matrix

#############################################
##### Extracting item attributes #####
#############################################
table_in <- read.csv('./anonymous-msweb.txt', header = FALSE) ## read_csv lost some column

table_items <- table_in %>% 
  filter(V1 == "A")
table_in %>% head(20)
table_items <- 
table_items %>%  mutate(id= V2,description=V4,url=V5) %>% 
  select(-starts_with("V"))

name_countries <- c(countrycode_data$country.name,
                    "Taiwan", "UK", "Russia", "Venezuela",
                    "Slovenija", "Caribbean", "Netherlands (Holland)",
                    "Europe", "Central America", "MS North Africa")
table_items[description %in% name_countries,]
table_items$description %in% name_countries

table_items <- 
table_items %>% mutate(
  category = ifelse(description %in% name_countries,'region','product')
)

# table_items[, list(n_items = .N), by = category]  
table(table_items$category)

##################################################
########## BUILD MODEL ##########
##################################################

which_train <- sample(x = c(TRUE, FALSE),
                      size = nrow(ratings_matrix),
                      replace = TRUE,
                      prob = c(0.8, 0.2))

recc_data_train <- ratings_matrix[which_train, ]
recc_data_test <- ratings_matrix[!which_train, ]


recc_model <- Recommender(data = recc_data_train,
                          method = "IBCF",
                          parameter = list(method = "Jaccard"))
# recc_model@model$sim
dim(recc_model@model$sim)

image(recc_model@model$sim)

range(recc_model@model$sim)

dist_ratings <- as(recc_model@model$sim, "matrix") ## distance from purchase history
# table_items[, 1 - dist(category == "product")]

dist_category <- 1-dist(table_items$category == "product")
# class(dist_category)

dist_category <- as.matrix(dist_category)

rownames(dist_category) <- table_items[,'id']
colnames(dist_category) <-table_items[,'id']
vector_items <- rownames(dist_ratings)

vector_items <- substring(vector_items,6,9)
# vector_items <- rownames(dist_ratings)

dist_category <- dist_category[vector_items, vector_items]
image(dist_category)
### hybrid 2 kinds of dist ####
weight_category <- 0.2
dist_tot <- dist_category * weight_category + dist_ratings * (1 - weight_category)
image(dist_tot)

recc_model@model$sim <- as(dist_tot, "dgCMatrix")


### predict ##
n_recommended <- 10
recc_predicted <- predict(object = recc_model,
                          newdata = recc_data_test,
                          n = n_recommended)
head(recc_predicted@itemLabels)
table_labels <- data_frame(id=recc_predicted@itemLabels)
table_labels <- table_labels %>% mutate(id2=as.integer(substring(id,6,9)))
table_labels<-table_labels %>% left_join(table_items,by=c("id2"="id")) 
table_labels$description <- as.character(table_labels$description)
table_labels$id <- NULL
# merge(table_labels, table_items,
#       by = "id2", all.x = TRUE, all.y = FALSE,
#       sort = FALSE)
recc_user_1 <- recc_predicted@items[[1]]
descriptions <- table_labels$description
items_user_1 <- table_labels$description[recc_user_1]
items_user_1
recc_user_1
recc_matrix <- sapply(recc_predicted@items, function(x){
  recommended <- descriptions[x]
  c(recommended, rep("", n_recommended - length(recommended)))
})
dim(recc_matrix)
recc_matrix[,1:3]

table_recomm_per_item <- table(recc_matrix)
recomm_per_item <- as(table_recomm_per_item, "numeric")
recomm_per_item
bin_recomm_per_item <- cut(recomm_per_item,
                           breaks = c(0, 100, 200, 500,
                                      max(recomm_per_item)))
qplot(bin_recomm_per_item) + ggtitle("Recommendations per item")


######################################################
#### Evaluate the models 
######################################################

evaluateModel <- function (
  # data inputs
  ratings_matrix, # rating matrix
  table_items, # item description table
  # K-fold parameters
  n_fold = 10, # number of folds
  items_to_keep = 4, # number of items to keep in the test set
  # model parameters
  number_neighbors = 30, # number of nearest neighbors
  weight_description = 0.2, # weight to the item description-based distance
  items_to_recommend = 10 # number of items to recommend
){
  # build and evaluate the model
  set.seed(1)
  eval_sets <- evaluationScheme(data = ratings_matrix,
                                method = "cross-validation",
                                k = n_fold,
                                given = items_to_keep)
  recc_model <- Recommender(data = getData(eval_sets, "train"),
                            method = "IBCF",
                            parameter = list(method = "Jaccard",
                                             k = number_neighbors))
  dist_ratings<- as(recc_model@model$sim, "matrix")
  vector_items <- rownames(dist_ratings) %>% substring(6,9) %>% as.integer()
  
  dist_category <- 1-dist(table_items$category == "product")
  # class(dist_category)
  
  # dist_category <- as.matrix(dist_category)
  
  
  # dist_category <- table_items[, 1 - as.matrix(dist(category ==
  #                                                     "product"))]
  dist_category <- as.matrix(dist_category)
  rownames(dist_category) <- table_items$id
  colnames(dist_category) <- table_items$id
  dist_category <- dist_category[vector_items]
  dist_tot <- dist_category * weight_description +
    dist_ratings * (1 - weight_description)
  recc_model@model$sim <- as(dist_tot, "dgCMatrix")
  eval_prediction <- predict(object = recc_model,
                             newdata = getData(eval_sets,
                                               "known"),
                             n = items_to_recommend,
                             type = "topNList")
  eval_accuracy <- calcPredictionAccuracy(
    x = eval_prediction,
    data = getData(eval_sets, "unknown"),
    byUser = FALSE,
    given = items_to_recommend)
  return(eval_accuracy)
}


ids <- ratings_matrix %>% colnames() %>% substring(6,9) %>% as.integer()
table_items <- table_items %>% as_tibble()
table_items2<- table_items %>% filter(id %in% ids)

model_evaluation <- evaluateModel(ratings_matrix = ratings_matrix,
                                  table_items = table_items2)



##################################################################
#### Optimize parameters 
###################################################################

###params: number_neighbors  & weight_description ###

nn_to_test <- seq(4, 80, by = 2)

list_performance <- lapply(
  X = nn_to_test,
  FUN = function(nn){
    evaluateModel(ratings_matrix = ratings_matrix,
                  table_items = table_items,
                  number_neighbors = nn,
                  weight_description = 0)
  })
list_performance[[1]]
sapply(list_performance,'[[',"precision")

df_performance <- tibble(nn=nn_to_test,
                         precision=sapply(list_performance,'[[',"precision"),
                         recall = sapply(list_performance,'[[',"recall"))
weight_precision <- 0.5
table_performance <-
df_performance %>% mutate(
  performance = precision * weight_precision + recall * (1 -weight_precision)
  )
table_performance
convertIntoPercent <- function(x){ paste0(round(x*100),"%")}
qplot(table_performance$nn, table_performance$precision) +
  geom_smooth() + scale_y_continuous(labels = convertIntoPercent)

qplot(table_performance$nn, table_performance$recall) +
  geom_smooth() + scale_y_continuous(labels = convertIntoPercent)
