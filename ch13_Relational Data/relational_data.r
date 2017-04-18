library(tidyverse)
library(nycflights13)
airlines
airports
planes
weather

planes %>% 
  count(tailnum) %>% 
  filter(n>1)

weather %>% 
  count(year,month,day,hour,origin) %>% 
  filter(n>1)

flights %>% 
  count(year,month,day,flight) %>% 
  filter(n>1)

flights %>% 
  count(year,month,day,tailnum) %>% 
  filter(n>1)

flights %>% 
  count(year,month,day,tailnum,sched_arr_time) %>% 
  filter(!is.na(tailnum),n>1)


# 13.4 mutating join ------------------------------------------------------

flights2 <- flights %>% 
  select(year:day,hour,origin,dest,tailnum,carrier)
flights2 %>% 
  select(-origin,-dest) %>% 
  left_join(airlines,by="carrier") %>% 
  head()

flights2 %>% 
  select(-origin,-dest) %>% 
  mutate(name=airlines$name[match(carrier,airlines$carrier)])

# 13.4.1 understand mutate join -------------------------------------------

x<-tribble(
  ~key,~val_x,
  1,"x1",
  2,"x2",
  3,"x3"
)

y<-tribble(
  ~key,~val_y,
  1,"y1",
  2,"y2",
  4,"y3"
)

x %>% 
  inner_join(y,by="key")

x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  2, "x3",
  1, "x4"
)
y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2"
)

left_join(x,y,by="key")

x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  2, "x3",
  3, "x4"
)

y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2",
  2, "y3",
  3, "y4"
)

left_join(x,y,by="key")


# natural join 
flights2 %>% 
  left_join(weather)

flights2 %>% 
  left_join(airports,c("dest"="faa"))

airports %>%
  semi_join(flights, c("faa" = "dest")) %>%
  ggplot(aes(lon, lat)) +
  borders("state") +
  geom_point() +
  coord_quickmap()


# semi-join, anti-join ----------------------------------------------------

top_dest <- flights %>% 
  count(dest,sort=T) %>% 
  head(n=10)
top_dest

flights %>% 
  filter(dest %in% top_dest$dest)

flights %>% 
  semi_join(top_dest)

flights %>%
  anti_join(planes, by = "tailnum") %>%
  count(tailnum, sort = TRUE)
