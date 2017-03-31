
# packages 
library(tidyverse)
## visualization ##
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
