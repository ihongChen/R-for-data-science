
# dplyr vignette Ex:  ---------------------------------------------------
# https://cran.rstudio.com/web/packages/dplyr/vignettes/introduction.html

### intro. #####
library(tidyverse)
install.packages("nycflights13")
library(nycflights13)
dim(flights)
head(flights)
class(flights)

flights %>% 
  filter(month==1,day==1)
flights %>% 
  filter(month==1 | month==2)

flights %>% 
  slice(1:3)

flights %>% 
  arrange(year,month,day)

flights %>% 
  arrange(desc(arr_delay))

flights %>% 
  select(year,month,day)
flights %>% 
  select(year:day)
?select
flights %>% 
  select(tail_num = tailnum)
flights %>% 
  rename(tail_num = tailnum)

flights %>% distinct(origin,dest) %>% arrange(origin)

flights %>% 
  mutate(gain=arr_delay-dep_delay,
         gain_per_hour = gain/(air_time/60)) %>% select(gain,gain_per_hour)

flights %>% 
  summarise(delay_mean=mean(dep_delay,na.rm=TRUE))

flights %>% 
  sample_n(20)
flights %>% 
  sample_frac(0.001)


vignette("window-functions")

by_tailnum <- group_by(flights,tailnum)
by_tailnum %>% filter(dep_delay==max(dep_delay)) %>% select(dep_delay,tailnum)

delay <- by_tailnum %>% 
  summarise(count=n(),
            dist=mean(distance,na.rm=T),
            delay=mean(arr_delay, na.rm = TRUE)) %>% 
  filter(delay,count>20,dist<2000)
ggplot(delay,aes(dist,delay)) +
  geom_point(aes(size=count),alpha=1/10) +
  geom_smooth() +
  scale_size_area()


destinations <- group_by(flights, dest)
summarise(destinations,
          planes = n_distinct(tailnum),
          flights = n()
)


flights %>% group_by(dest) %>% 
  summarise(planes=n_distinct(tailnum),flights=n())

daily <- flights %>% group_by(year,month,day)
per_day <- daily %>% summarise(flights=n())
per_day
per_month <- summarise(per_day,flights=sum(flights))
per_month
per_year <- summarise(per_month,flights =sum(flights))
per_year


#  memory issue-----------------------------------------------------

location(iris)
iris
iris2<-iris
location(iris2)
changes(iris2,iris)

iris2$Sepal.Length <- iris2$Sepal.Length *2
changes(iris2,iris)

iris3 <-mutate(iris,Sepal.Length = Sepal.Length*2)
changes(iris3,iris)


# sql ---------------------------------------------------------------------
## ref :https://cran.rstudio.com/web/packages/dplyr/vignettes/databases.html

# install.packages("RSQLite")
src_sqlite()
db <- src_sqlite("my_db.sqlite3", create = TRUE)

library(nycflights13)
flights
flights_sqlite <-copy_to(db,flights,temporary=FALSE,
                         index=list(c("year", "month", "day"),"carrier", "tailnum"))
flights_sqlite

flights_sqlite <- tbl(nycflights13_sqlite(), "flights")
db <- src_sqlite("my_db.sqlite3")
flights_sqlite<-tbl(flights_sqlite,"flights")

tbl(db,sql("select * from flights limit 3"))


select(flights_sqlite,year:day,dep_delay)
filter(flights_sqlite,dep_delay>20)
