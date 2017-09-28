## ch18. Pipe 

library(magrittr)

diamonds <- ggplot2::diamonds
diamonds2 <- diamonds %>% 
  dplyr::mutate(price_per_carat=price/carat)

pryr::object_size(diamonds)
pryr::object_size(diamonds2)
pryr::object_size(diamonds,diamonds2)

diamonds$carat[1] <- NA
pryr::object_size(diamonds)
pryr::object_size(diamonds2)
pryr::object_size(diamonds,diamonds2) # 4.32MB



# t pipe ------------------------------------------------------------------

rnorm(100) %>% 
  matrix(ncol=2) %T>% 
  plot() %>% 
  str()

# mtcars <- 
#   mtcars %>% 
#   transform(cyl=cyl*2)

mtcars %<>% transform(cyl=cyl*2)

