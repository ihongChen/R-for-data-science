library(tidyverse)
# install.packages("stringr")
library(stringr)
# string ------------------------------------------------------------------

string1 <- "This is a string" # recommended 
string2 <- 'this is a string too'
double_quote <- "\""
single_quote <- '\''

writeLines(single_quote)
writeLines(double_quote)
x <- c("\"","\\")
writeLines(x)

x<- "\u00b5"

# 14.2.1 string length ----------------------------------------------------

library(stringr)
str_length(c("a","R for data science",NA))


# 14.2.2 combine strings  -------------------------------------------------

# str_c
str_c("x","y")
str_c("x","y","z")
str_c("x","y",sep=",")
str_c(x,"y",sep=",")

x <- c("abc",NA)
str_c(x,"y")

x1 <- c("abc","def")
str_c(x1,"y")

str_c("|-", x, "-|")
str_c("|-", str_replace_na(x), "-|")

str_c("prefix-",c("a","b","c"),"-suffix")

name<- "ihong"
time_of_day <- "morning"
birthday <- FALSE
str_c(
  "Good ",time_of_day," ",name,
  if (birthday) " and Happy Birthday",
  "."
)

str_c(c("x","y","z"),collapse = ", ")


# 14.2.3 subseting strings------------------------------------------------------------------
x <- c("Apple","Banana","Pear")
str_sub(x,1,3)
str_sub(x,-3,-1)
str_sub("a",1,5)

str_sub(x,1,1) <- str_to_lower(str_sub(x,1,1))
x
#### note 傳值 (call by value) ####
x <- c(1,1,2)
x2 <- x
x2[3] <- 4
x
###################################


# 14.2.4 locals -----------------------------------------------------------

x <- c("apple", "eggplant", "banana")
str_sort(x, locale = "en")  # English
str_sort(x,locale="haw")


# 14.2.5 ex ------------------------------------------------------------------
# ex1
paste(x,"y")
paste0(x,"y") 

str_c(x,"y",sep=" ") # same as : paste(x,"y")
str_c(x,"y")

# ex3
str_length(x)
str_length(c("哈嚕哈","你好嗎","a豬頭"))
str_sub(x,1,3)

middle_vec <- floor((str_length(x) +1)/2)
str_sub(x,middle_vec,middle_vec)

# ex4
str_wrap()
thanks_path <- file.path(R.home("doc"),"THANKS")
thanks <-str_c(readLines(thanks_path),collapse = "\n")
thanks
cat(str_wrap(thanks),"\n")
cat(str_wrap(thanks,width = 40),"\n")
cat(str_wrap(thanks,width = 60, indent=2),"\n")
cat(str_wrap(thanks,width = 60, exdent =2),"\n")

## str_trim 
str_trim("  String with trailing and leading white space\t")
str_trim("\n\nString with trailing and leading white space\n\n")

str_pad(c("a", "abc", "abcdef"), 10)


# 14.3.1 Regx -------------------------------------------------------------

x<-c("apple","banana","pear")
str_view(x,"an")
str_view(x,".a.")

dot <- "\\."
cat(dot)
writeLines(dot)
str_view(c("abc","a.c","bef"),dot)
x<-"a\\b"
writeLines(x)

str_view(x,"\\\\")

x <- c("apple pie", "apple", "apple cake")
str_view(x, "apple")


# 14.3.2 Anchors ----------------------------------------------------------

# ^ start , $ end
x<-c("apple","banana","pear")
str_view(x,"^a")
str_view(x,"a$")
str_view(x,"^apple$")


# 14.3.3 char classes -----------------------------------------------------

# \d : any digits, \s: any whitespace ,[abc]: match a,b or c
# [^abc] : match any except a b or c 

str_view(c("grey","gray"),"gr(e|a)y")


# 14.3.4 repetition -------------------------------------------------------

# ?:0 or 1 , +: 1 or more , *: 0 or more
x <- "1888 is the longest year in Roman numerals: MDCCCLXXXVIII"
str_view(x,"CC?")

str_view(x,"CC+")
str_view(x,"CC*")

str_view(x,"C[LX]+")
str_view(x,"C(L|X)+")

str_view(x,"C[LX]{3}")
str_view(x,"C{2,3}+")
str_view(x,"C{1,}")


