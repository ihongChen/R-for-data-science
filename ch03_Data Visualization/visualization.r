######## visualization ################
# packages 
library(tidyverse)

#### START ####
# ex1 : bigger car engine use more fuel than small engine?

View(mpg) # displ => engine size  , hwy => car fuel efficiency 

ggplot(data = mpg) + 
  geom_point(mapping = aes(x=displ,y=hwy))

# 3.2.4 ex:
ggplot(mpg)
dim(mtcars)
?mpg # cyl : number of cylinders
ggplot(data = mpg) + 
  geom_point(mapping = aes(x=cyl, y=hwy))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x=class, y=drv))

##### 3.4 aes ######
# color (3rd type)
ggplot(data = mpg) + 
  geom_point(mapping = aes(x=displ,y=hwy, color=class))

# size 
ggplot(data = mpg) + 
  geom_point(mapping = aes(x=displ,y=hwy, size=class))
#> Warning: Using size for a discrete variable is not advised.

# shape/ alpha
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, alpha = class))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, shape = class))
# color : blue
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), color = "blue")
?geom_point

## 3.3.1 ex

ggplot(mtcars, aes(wt, mpg)) +
  geom_point(shape = 21, colour = "black", fill = "white", size = 1, stroke = 0.5)

#### 3.5 facet ####

ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ,y=hwy)) +
  facet_wrap(~class,nrow = 2)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(drv ~ cyl)


ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(. ~ cyl)


## ex 3.5.1 ##
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = drv, y = cyl))


ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv ~ .)


ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_wrap(~drv)

#### 3.6 geom object #####

ggplot(data=mpg) + 
  geom_smooth(mapping=aes(x=displ,y=hwy)) 

ggplot(data = mpg) + 
  geom_point(mapping=aes(x=displ,y=hwy))

ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv))

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, group = drv))

## combine two type
ggplot(data = mpg) + 
  geom_point(mapping = aes(x=displ,y=hwy)) +
  geom_smooth(mapping = aes(x=displ,y=hwy)) # duplicated !!

ggplot(data = mpg, mapping = aes(x=displ,y=hwy)) + 
  geom_point() +
  geom_smooth()
  
ggplot(data = mpg, mapping = aes(x=displ,y=hwy)) + 
  geom_point(mapping = aes(color = class)) +
  geom_smooth()

ggplot(data = mpg, mapping = aes(x=displ,y=hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth(data = filter(mpg,class=="subcompact"),se=FALSE)

## 3.6.1 ex
ggplot(data = mpg, mapping = aes(x=displ,y=hwy)) + 
  geom_point(mapping = aes(color = class),show.legend = F) + 
  geom_smooth() # 比較show.legend=T, F結果

ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = drv)) + 
  geom_point() + 
  geom_smooth(se = FALSE)

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point() + 
  geom_smooth()

ggplot() + 
  geom_point(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_smooth(data = mpg, mapping = aes(x = displ, y = hwy))

ggplot(data = mpg , mapping = aes(x=displ,y=hwy)) + 
  geom_point() + 
  geom_smooth(data = filter(mpg,class=="subcompact"),se=FALSE) + 
  geom_smooth(data = filter(mpg,class=="compact"),se=FALSE)
  
#### 3.7 Statistical Transformation #####

View(diamonds)

ggplot(data=diamonds) + 
  geom_bar(mapping = aes(x=cut))

ggplot(data=diamonds) + 
  stat_count(mapping = aes(x=cut))

#  bar chart ->stat= "identity"
demo <- tribble(
  ~a,~b,
  "bar_1",20,
  "bar_2",30,
  "bar_3",40
  )
ggplot(data = demo) +
  geom_bar(mapping = aes(x = a,y=b), stat="identity")


# bar chart percentage %
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x=cut,y=..prop..,group=1))

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity),position = 'fill')

# stat summary 
ggplot(data = diamonds) +
  stat_summary(
    mapping = aes(x=cut,y=depth),
    fun.ymin = min,
    fun.ymax = max,
    fun.y = median
  )

##### 3.7.1  ex #####
?stat_summary # pointrange
ggplot(data = diamonds,mapping=aes(x=cut,y=depth)) + 
  geom_pointrange(aes(ymin=depth,ymax=depth))

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, y = ..prop..)) # without group=1

## geom_col
df <- data.frame(trt = c("a", "b", "c"), outcome = c(2.3, 1.9, 3.2))
ggplot(df,aes(x=trt,y=outcome)) +
  # geom_point()
  geom_col()

##### 3.8 Position Adjustment ######

