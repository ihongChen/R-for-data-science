
# Regx --------------------------------------------------------------------
## from : http://datascienceandr.org/articles/RegularExpression.html


head(flights$tailnum)
grep("^AA",flights$tailnum)
sum(substring(flights$tailnum,1,2) =='AA',na.rm=T)

head(grep("AA$", flights$tailnum,value=T))

# ans.grepl <- grepl('AA$',flights$tailnum)
head(grep("A{3}", flights$tailnum, value = TRUE))
head(grep("A.A", flights$tailnum, value = TRUE))

head(grep("N[13]{3}", flights$tailnum, value = TRUE))
head(grep("(12){2}", flights$tailnum, value = TRUE))
head(grep("(1[23]){2}", flights$tailnum, value = TRUE))




pirate_path <- tempfile(fileext = ".txt")
download.file("https://raw.githubusercontent.com/wush978/DataScienceAndR/course/02-RDataEngineer-01-Parsing/pirate-info-2015-09.txt", destfile = pirate_path)
pirate_info <- readLines(file(pirate_path, encoding = "BIG5"))
head(pirate_info)

head(
  regmatches(
    pirate_info, 
    regexec("日期：([0-9]{4})年([0-9]{1,2})月([0-9]{1,2})日", pirate_info)
  ))

# head(regexec("日期：([0-9]{4})年([0-9]{1,2})月([0-9]{1,2})日", pirate_info))

x <- "123(45)657"
pattern <- "((.*))" ## WRONG!!!
regmatches(x, regexec(pattern, x))
pattern2 <- "\\((.*)\\)"
regmatches(x, regexec(pattern2, x))

