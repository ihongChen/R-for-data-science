
# Fast R  -----------------------------------------------------------------

## from :https://m-clark.github.io/docs/fastr.html

## from Strategies to Speedup R Code (https://datascienceplus.com/strategies-to-speedup-r-code/)

# create the data.frame 

col1 <- runif(1e5,0,2)
col2 <- rnorm(1e5,0,2)
col3 <- rpois(1e5,3)
col4 <- rchisq(1e5,2)
df <- data.frame(col1,col2,col3,col4)

## Original R code : before vectorization and pre-allocation

system.time(
  for (i in 1:nrow(df)){
    if ((df[i,"col1"] + df[i,"col2"] + df[i,"col3"]+ df[i,"col4"]) > 4){
      df[i,5] <- "greater_than_4" # assign 5th col
    } else{
      df[i,5] <- "lesser_than_4" # assign 5th col
    }
      
  }
)


## Vectorise and pre-allocate data structures

output <- character(nrow(df)) # initialze output vector

system.time({
  for (i in 1:nrow(df)){
    if ((df[i,"col1"] + df[i,"col2"] + df[i,"col3"] + df[i,"col4"])>4 ){
      output[i] <- "greater_than_4"
    }else{
      output[i] <- "lesser_than_4"
    }
  }
  df$output <- output
}
)

