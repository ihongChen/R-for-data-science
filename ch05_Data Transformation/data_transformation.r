
# intro -------------------------------------------------------------------
# install.packages("nycflights13")
library(nycflights13)
library(tidyverse)
flights
head(flights)

# dplyr -------------------------------------------------------------------
# filter / arrange /select / mutate / summarise


# 5.2 filter ------------------------------------------------------------------

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


# 5.4 select --------------------------------------------------------------

select(flights,year,month,day)
select(flights,year:day)
select(flights,-(year:day))

select(flights,ends_with("r"))
## rename 
colnames(flights)
colnames(rename(flights,tail_num=tailnum))

## everthing with select
select(flights,time_hour,air_time,everything())

# 5.4.1 ex ----------------------------------------------------------------

select(flights,
       dep_time,dep_delay,arr_time,arr_delay)
select(flights,starts_with("de"),starts_with("arr"))

# one_of
vars <- c("year","month","day","dep_delay","arr_delay")
select(flights,one_of(vars))

# contain 
select(flights,contains("time"))


# 5.5 mutate --------------------------------------------------------------

flights_sml <- 
  select(flights,
         year:day,
         ends_with("delay"),
         distance,
         air_time)
mutate(flights_sml,
       gain = arr_delay-dep_delay,
       speed = distance/air_time *60 )

mutate(flights_sml,
       淨時間 = arr_delay - dep_delay,
       `滯空時間(小時)` = air_time/60 ,
       亂算 = `淨時間` / `滯空時間(小時)` )

## transmutate (只保留新變數)
transmute(flights,
          gain = arr_delay - dep_delay)

## 四則運算
transmute(flights,
          dep_time,
          hour = dep_time %/% 100,
          minute = dep_time %% 100)
6.4 %% 2 ## mod ,
6.4 %/% 2 ## floor 

## offset: lead /lag 
(x<-1:10)
lag(x)
lead(x)
x - lead(x)

## cumsum / cumprod / cummin /cummin 
## cummean (dplyr)
x
cumsum(x)
cummean(x)

y <- c(1,2,2,NA,3,4)
min_rank(y)
min_rank(desc(y))
row_number(y)
dense_rank(y)
percent_rank(y)
cume_dist(y)


# 5.5.2 ex ----------------------------------------------------------------

select(flights,dep_time,sched_dep_time) %>%
  mutate(dep_hour = dep_time %/% 100,
         dep_min  = dep_time %% 100,
         sched_dep_hour = sched_dep_time %/% 100,
         sched_dep_min  = sched_dep_time %% 100) %>%
  View()

select(flights, air_time,
       arr_time - dep_time)
transmute(flights,
       air_time,
       arr_time,
       dep_time,
       elapse_time = (arr_time%/% 100*60 +arr_time %% 100)
        - (dep_time %/% 100 *60 + dep_time %% 100))

select(flights,
       dep_time,
       sched_dep_time,
       dep_delay)



# 5.6 summarise -----------------------------------------------------------

summarise(flights,delay= mean(dep_delay,na.rm=T))
## group by + summarise
by_day <- group_by(flights,year,month,day) # group by
summarise(by_day,delay = mean(dep_delay,na.rm=T))

## pipe 
by_dest <- group_by(flights,dest)
delay <- summarise(by_dest,
                   count = n(),
                   dist = mean(distance,na.rm=TRUE),
                   delay = mean(arr_delay,na.rm=TRUE))
delay <- filter(delay,count > 2, dest != "HNL")
View(delay)
# plot delay vers dist
ggplot(delay ,mapping = aes(x=dist ,y= delay)) + 
  geom_point(mapping = aes(size = count),alpha = 1/3) + 
  geom_smooth(se=FALSE)


## use pipline   %>% ###
delays <- flights %>%
  group_by(dest) %>%
  summarise(
    count = n(),
    dist = mean(distance,na.rm=T),
    delay = mean(arr_delay ,na.rm = T)
  ) %>%
  filter(count>20, dest!="HNL")

# 5.6.2 missing data ------------------------------------------------------

flights %>%
  group_by(year,month,day) %>%
  summarise(mean = mean(dep_delay,na.rm=T))

not_cancelled <-flights %>%
  filter(!is.na(dep_delay),!is.na(arr_delay))

not_cancelled %>%
  group_by(year,month,day) %>%
  summarise(mean = mean(dep_delay))

# 5.6.3 counts ------------------------------------------------------------

delays <- not_cancelled %>%
  group_by(tailnum) %>%
  summarise(delay = mean(arr_delay))

View(delays)
ggplot(delays,mapping=aes(x=delay)) +
  geom_freqpoly(binwidth=10)


delays <- not_cancelled %>%
  group_by(tailnum) %>%
  summarise(
    count = n(),
    delay = mean(arr_delay)
  )
ggplot(delays,mapping=aes(x=count,y=delay)) + 
  geom_point(alpha=1/10)

## filter out extreme value
delays %>%
  filter(count>25 & count <300 ) %>%
  ggplot(mapping = aes(x=count,y=delay)) +
    geom_point(alpha=1/10)

# Lahman:: baseball batting -----------------------------------------------
library(Lahman)
batting <- as_tibble(Batting)

batters <- batting %>%
  group_by(playerID) %>%
  summarise(
    ba = sum(H,na.rm=T)/sum(AB,na.rm=T),
    ab = sum(AB,na.rm=T)
  )
batters %>%
  filter(ab>100) %>%
  ggplot(mapping = aes(x=ab,y=ba)) + 
    geom_point(alpha=1/10) + 
    geom_smooth(se = FALSE)

batters %>%
  arrange(desc(ba))

# 5.6.4 Useful summary functions ------------------------------------------
## subset
not_cancelled %>%
  group_by(year,month,day) %>%
  summarise(
    avg_delay1 = mean(arr_delay),
    avg_delay2 = mean(arr_delay[arr_delay>0]) # subset
  )

## sd/IQR/mad

not_cancelled %>% 
  group_by(dest) %>%
  summarise(distance_sd = sd(distance)) %>%
  arrange(desc(distance_sd)) %>%
  View()
## min , quantile(x,0.25)
not_cancelled %>%
  group_by(year,month,day) %>%
  summarise(
    first = min(dep_time),
    last = max(dep_time)
  )

## 
not_cancelled %>%
  group_by(year,month,day) %>%
  summarise(
    first = first(dep_time),
    last = last(dep_time)
  )
##### min_rank ?????????? ####
not_cancelled %>% 
  group_by(year, month, day) %>% 
  mutate(r = min_rank(desc(dep_time))) %>% 
  filter(r %in% range(r))

not_cancelled %>%
  group_by(year,month,day) %>%
  mutate(r = min_rank(desc(dep_time))) %>%
  filter(r %in% range(r)) %>%
  select(r)
  
(x <- c(NA,1:3,-1:1/0))
range(x,na.rm=T,finite=T)


# Counts: You’ve seen n(), which takes no arguments, 
# and returns the size of the current group. 
# To count the number of non-missing values, 
# use sum(!is.na(x)). 
# To count the number of distinct (unique) values, use n_distinct(x).

not_cancelled %>%
  group_by(dest) %>%
  summarise(carrier=n_distinct(carrier)) %>%
  arrange(desc(carrier))

not_cancelled %>%
  count(dest)
## count with weight (wt)
not_cancelled %>%
  count(tailnum,wt=distance) 
  # filter(tailnum =='D942DN')

not_cancelled %>%
  group_by(tailnum) %>%
  summarise(count = n(),
            sum_distance = sum(distance))

# How many flights left before 5am? (these usually indicate delayed
# flights from the previous day)
not_cancelled %>% 
  group_by(year, month,day) %>% 
  summarise(n_early = sum(dep_time < 500))

# What proportion of flights are delayed by more than an hour?
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(hour_perc = mean(arr_delay > 60))


# 5.6.5 grouping by multiple variable -------------------------------------

daily <- flights %>%
  group_by(year,month,day)
(per_day <- summarise(daily,flights = n()))

(per_month <- summarise(per_day,flights = sum(flights)))
(per_year <-summarise(per_month,flights = sum(flights)))

daily %>%
  ungroup() %>%
  summarise(flights=n())

# 5.6.7 ex ----------------------------------------------------------------

# flights always 10 min late 
flights %>%
  group_by(tailnum) %>%
  summarise(
    min_arr_delay = min(arr_delay,na.rm=TRUE)
  ) %>%
  filter(min_arr_delay>10)

# flight 30 min early 50% of time ,
flights %>%
  group_by(tailnum) %>%
  summarise(
    count = n(),
    med_arr_delay = median(arr_delay,na.rm=T)
  ) %>%
  filter(med_arr_delay<(-30))

# worst airport? bad carrier? 最糟糕的機場/航空公司
delay_carrier_dest <-flights %>%
  group_by(carrier,dest) %>%
  summarise(flights_no = n(),
            mean_arr_delay = mean(arr_delay,na.rm=T),
            mean_dep_delay = mean(dep_delay,na.rm=T))

delay_carrier_dest %>%
  filter(carrier=='OO' & flights_no >1)
  # filter(dest == 'DEN')


summarise(delay_carrier_dest,
          flights = sum(flights_no),
          avg_arr_delay = mean(mean_arr_delay,na.rm=TRUE),
          avg_dep_delay = mean(mean_dep_delay,na.rm=TRUE)) %>%
  arrange(desc(avg_arr_delay))


temp <-flights %>%
  group_by(dest) %>%
  summarise(
    flights_no = n(),
    avg_dep_delay = mean(dep_delay,na.rm=T))
# 平均機場起飛延遲時間 .. CAE > TUL > OKC
temp %>% 
  select(dest,
         flights_no,
         avg_dep_delay) %>%
  filter(avg_dep_delay>10) %>%
  arrange(desc(avg_dep_delay))

# 5.7 Grouped mutates (and filters) ---------------------------------------

flights_sml %>%
  group_by(year,month,day) %>%
  filter(rank(desc(arr_delay))<10) %>%
  select(arr_delay) %>%
  View()

(r1 <- rank(x1 <- c(3, 1, 4, 15, 92)))

flights %>% 
  group_by(dest) %>% 
  filter(n()>365) %>%
  summarise(count = n())


## Standardise to compute per group metrics:
popular_dests <- flights %>% 
  group_by(dest) %>% 
  filter(n() > 365)

popular_dests

popular_dests %>%
  filter(arr_delay > 0) %>%
  mutate(prop_delay = arr_delay / sum(arr_delay)) %>%
  select(year:day,dest,arr_delay,prop_delay)


popular_dests %>% 
  filter(arr_delay >0 ) %>% 
  mutate(sum_arr_delay= sum(arr_delay)) %>% 
  arrange(dest) %>% 
  select(sum_arr_delay)

# 5.7.1 ex ----------------------------------------------------------------------

flights %>% 
  group_by(tailnum) %>% 
  summarise(
    total_arr_delay = mean(arr_delay,na.rm=T),
    total_dep_delay = mean(dep_delay,na.rm=T),
    flights = n()
  ) %>%
  filter(flights>10) %>% 
  arrange(desc(total_dep_delay),desc(total_arr_delay))
## delay 集中在哪個月份？ ##
flights %>% 
  group_by(year,month) %>% 
  summarise(
    mean_dep_delay = mean(dep_delay,na.rm=T),
    mean_arr_delay = mean(dep_delay,na.rm=T)
  )

## delay 集中在一天中的哪個時間? ###
flights %>% 
  group_by() %>% 
  summarise(
    mean_daily_dep_delay = mean(dep_delay,na.rm=T),
    mean_daily_arr_delay = mean(arr_delay,na.rm=T)
  )
library(lubridate)
flights %>%
  mutate(hours = hour(strptime(time_hour,'%Y-%m-%d %H:%M:%S'))) %>% 
  group_by(hours) %>% 
  summarise(
    mean_hour_dep_delay = mean(dep_delay,na.rm=T)
  )
