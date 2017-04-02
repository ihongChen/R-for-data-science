
# intro -------------------------------------------------------------------
# install.packages("nycflights13")
library(nycflights13)
library(tidyverse)
flights
head(flights)

# dplyr -------------------------------------------------------------------
# filter / arrange /select / mutate / summarise


# filter ------------------------------------------------------------------

jan1 <- filter(flights,month==1,day==1)
(dec25 <- filter(flights,month==12,day==25))
nov_dec <- filter(flights,month==11 | month==12)
nov_dec <- filter(flights,month %in% c(11,12))

df <- tibble(x = c(1,NA,3))
filter(df,x>1)
filter(df,is.na(x) | x>1)

# 5.2.4 ex ----------------------------------------------------------------
colnames(flights)
filter(flights,arr_delay>=2)
filter(flights,dest %in%c('IAH','HOU')) %>%
  select(dest) %>%
  View
filter(flights,arr_delay>2 & dep_delay==0) %>%
  select(arr_delay,dep_delay)

count(filter(flights,dep_time %in% NA)) # number of dep_time is NA

# 5.3 arrange -------------------------------------------------------------

arrange(flights,year,month,day)

arrange(flights,desc(arr_delay))

df<-tibble(x=c(5,2,NA))
df
arrange(df,desc(x))

# 5.3.1 exercise ----------------------------------------------------------

arrange(df,desc(is.na(x)))
arrange(flights,desc(is.na(arr_time)))

colnames(flights)
arrange(flights,desc(distance/hour)) %>%
  select(tailnum)

