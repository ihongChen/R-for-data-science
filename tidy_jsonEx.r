
# vignette from tidyjson : ------------------------------------------------

#  https://cran.r-project.org/web/packages/tidyjson/vignettes/introduction-to-tidyjson.html #
# tidy json ---------------------------------------------------------------

# install.packages("tidyjson")
library(tidyjson)
library(tidyverse)

# simple example ----------------------------------------------------------


# Define a simple JSON array of people
people <- '[
  {
    "name": "bob",
    "age": 32
  }, 
  {
    "name": "susan", 
    "age": 54
  }
]'

# cat(people)

# Structure the data
people %>%                  # %>% is the magrittr pipeline operator 
  gather_array %>%          # gather (stack) the array by index
  spread_values(            # spread (widen) values to widen the data.frame
    name = jstring("name"), # value of "name" becomes a character column
    age = jnumber("age")    # value of "age" becomes a numeric column
  )

library(jsonlite)
fromJSON(people) # easier !!!

# complex example ---------------------------------------------------------

purch_json <- '
[
  {
    "name": "bob", 
    "purchases": [
      {
        "date": "2014/09/13",
        "items": [
          {"name": "shoes", "price": 187},
          {"name": "belt", "price": 35}
        ]
      }
    ]
  },
  {
    "name": "susan", 
    "purchases": [
                  {
                    "date": "2014/10/01",
                    "items": [
                      {"name": "dress", "price": 58},
                      {"name": "bag", "price": 118}
                    ]
                  },
                  {
                    "date": "2015/01/03",
                    "items": [
                        {"name": "shoes", "price": 115}
                    ]
                  }
    ]
  }
]
'
purch_json %>% prettify()
cat(purch_json)

library(jsonlite)
purch_df <-jsonlite::fromJSON(purch_json,simplifyDataFrame = T) 
str(purch_df) # difficulty to work with 

purch_df$purchases[[1]]$items

items <- lapply(purch_df$purchases,'[[',"items")
prices <-lapply(items,lapply,'[[',"price")

vapply(lapply(prices,unlist),sum,integer(1))


# using tidyjson ----------------------------------------------------------
purch_items <-
  purch_json %>% 
    gather_array %>% 
    spread_values(person=jstring("name")) %>% 
    enter_object("purchases") %>% gather_array %>% 
    spread_values(purchases.date = jstring("date")) %>% 
    enter_object("items") %>% gather_array %>% 
    spread_values(
      item.name= jstring("name"),
      item.price = jnumber("price")
    ) %>% 
    select(person,purchases.date,item.name,item.price) # select only what is needed
    
  
purch_items

purch_items %>% 
  group_by(person) %>% 
  summarise(spend = sum(item.price))


# data --------------------------------------------------------------------

# create tbl_json

# Using a single character string
x <- '{"key": "value"}' %>% as.tbl_json
attr(x,"JSON")
# Using a vector of JSON strings
y <- c('{"key1": "value1"}', '{"key2": "value2"}') %>% as.tbl_json
y
attr(y,"JSON")

'{"key": "value", "array": [1, 2, 3]}' %>% prettify

df <- data.frame(
  x = 1:2,
  JSON = c('{"key1": "value1"}', '{"key2": "value2"}'),
  stringsAsFactors = FALSE
) 

z <- df %>% as.tbl_json(json.column="JSON")
attr(z,"JSON")

c('{"a": 1}', '[1, 2]', '"a"', '1', 'true', 'null') %>% json_types

'[1, "a", {"k": "v"}]' %>% gather_array %>% json_types

'{"name": "bob", "age": 32}' %>% gather_keys %>% json_types


'{"name": {"first": "bob", "last": "jones"}, "age": 32}' %>% 
  spread_values(
    first.name = jstring("name","first"),
    age = jnumber("age")
  ) %>% select(first.name,age)


'{"first": "bob", "last": "jones"}' %>% 
  gather_keys() %>% append_values_string()



c('{"name": "bob", "children": ["sally", "george"]}', '{"name": "anne"}') %>% 
  spread_values(parent.name = jstring("name")) %>%
  enter_object("children") %>% 
  gather_array %>% 
  append_values_string("children")



# Example : WorldBank -----------------------------------------------------

worldbank[1] %>% prettify()
amts <-
worldbank %>% 
  spread_values(total=jnumber("totalamt")) %>% 
  enter_object("majorsector_percent") %>% gather_array %>% 
  spread_values(
    sector = jstring("Name"),
    pct = jnumber("Percent")
  )  %>% mutate(total.m=total/10^6) %>% 
  select(document.id,sector,total.m,pct) %>% 
  tbl_df

amts %>% 
  group_by(document.id) %>% 
  summarise(pct.total = sum(pct)) %>% 
  group_by(pct.total) %>% 
  tally()

summary(amts$total.m)

amts %>%
  group_by(sector) %>%
  summarize(
    spend.portion = sum(total.m * pct / 100)
  ) %>%
  ungroup %>%
  mutate(spend.dist = spend.portion / sum(spend.portion)) %>%
  arrange(desc(spend.dist))


# Rmongo ------------------------------------------------------------------
##### 不好使用 ... 哭哭 ...
# install.packages("RMongo")
# library(RMongo)
# mongo <- mongoDbConnect("yw-com-backend-prod","localhost",27017)
# 
# query <- "
# {
#   progressLog.orderDate :{
#     $gte : 'ISODate(\'2017-05-03T15:19:41.706Z\')', '$lt':'new Date()'
#   }
# },
#   {_id:1,
#         'progressLog.orderDate':1,
#         'details' : 1,
#         'buyer.fullname':1,
#         'paid.amountPaid':1,
#         'store':1
# }
# "
# output <- dbGetQuery(mongo,"ToBuyOrder",query)
# 
# output <- dbGetQuery(mongo,"ToBuyOrder"," 
#     {},
#     {_id:1,
#         'progressLog.orderDate':1,
#         'details' : 1,
#         'buyer.fullname':1,
#         'paid.amountPaid':1,
#         'store':1}")
# # dbDisconnect(mongo)
# 
# dbGetQuery(mongo, "user","{'age':{'$gt': 21}}}")
# 

# mongolite ---------------------------------------------------------------
## ref: https://jeroen.github.io/mongolite/index.html#requirements-linux-mac
install.packages("mongolite")

library(mongolite)
m <- mongo(collection = "ToBuyOrder", 
           db = "yw-com-backend-prod",
           url = "mongodb://localhost")

###### api for mongolite ##########
# count
m$count()

m$count('{}')

# find 
alldata <- m$find('{}')

m$find(query = '{}',
       fields = '{"details":1,
       "store":1,
       "buyer.fullname":1,
       "paid.amountpaid":1}',
       limit = 10
       ) 

#### select by date #####

# get some example #
library(jsonlite)
mydata <- jsonlite::fromJSON("https://api.github.com/repos/jeroenooms/mongolite/issues")
mydata$created_at
mydata$created_at <- strptime(mydata$created_at,"%Y-%m-%dT%H:%M:%SZ",'UTC')
mydata$closed_at <- strptime(mydata$closed_at,"%Y-%m-%dT%H:%M:%SZ",'UTC')
# insert to mongo
issues <- mongo("issues") # test db
issues$insert(mydata)

# select by date
issues$find(
  query = '{"created_at":{"$gte":{"$date":"2017-01-01T00:00:00Z"}}} ',
  fields = '{"created_at" : 1, "user.login" : 1, "title":1, "_id":0}'
) 

#######################
## 有無訂單 
library(tidyverse)
toBuyOrder <-
  m$find(
  query = '{"progressLog.orderDate":{"$gte":{"$date":"2017-05-01T00:00:00Z"}}}',
  fields = '{
  "originRef.buyer":1,
  "progressLog.orderDate":1,
  "details":1,
  "buyer.fullname":1,
  "paid.amountPaid":1,
  "store":1}'
)

toBuyOrder %>% head()
Order5 <- toBuyOrder[1:5,]

# json-type character  
json_string<-
  Order5 %>% 
  toJSON() %>% 
  prettify() %>% 
  as.character()

cat(json_string)


json_string %>% 
  gather_array %>% 
  spread_values(
    buyer = jstring('buyer','fullname'),
    buyerId = jstring('originRef','buyer'),
    purchase.date = jstring('progressLog','orderDate'),
    paid.amount = jnumber('paid','amountPaid'),
    store = jstring('store','name')
  ) %>% 
  enter_object('details') %>% gather_array %>% 
  spread_values(
    purchase.item = jstring('name'),
    purchase.count = jnumber('count'),
    item.price = jnumber('sellingPrice')
    ) %>% View()
  

## ETL ##
companies[[1]] %>% prettify()

# library(tidyjson)
# stream_out(toBuyOrder,file("dump_tobuy.json"),verbose = FALSE )

# toBuyOrder %>% head() %>% View()
# 
# str(toBuyOrder)
# 
# details_list <- toBuyOrder$details
# details_list[[1]] 
# 
# details_list[[1]]
# lapply(details_list,`[[`,'name')
# details_list[[1]]


#   gather_array %>% 
#   spread_values(name = jstring('buyer.fullname'))

# 
# temp %>% 
#   select(-details)
# 
# temp$details %>% 
#   rbind.pages() %>% 
#   select(-customizations)
