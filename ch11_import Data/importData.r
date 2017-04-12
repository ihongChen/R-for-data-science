library(tidyverse)


# 11.3.2 string  ----------------------------------------------------------

charToRaw("Hadley")
charToRaw("你好")
x1 <- "El Ni\xf1o was particularly bad this year"
x2 <- "\x82\xb1\x82\xf1\x82\xc9\x82\xbf\x82\xcd"
print(x2)
parse_character(x1,locale = locale(encoding = "latin1"))
parse_character(x2,locale = locale(encoding = "Shift-JIS"))

guess_encoding(charToRaw(x1))
parse_character(x1,locale = locale(encoding = "ISO-8859-1"))



# 11.3.3 factors ----------------------------------------------------------

# factors 
fruit <- c("apple","banana")
parse_factor(c("apple","banana","banananaa"),levels=fruit)


# 11.3.4 date,datetime,times ----------------------------------------------

parse_datetime("2010-10-01T2010")
parse_datetime("20101001")

# parse_date("2010-01-10",format = "%Y-%m-%d")
parse_date("2010-01-10")

library(hms)
parse_time("01:10 am")
parse_time("20:10:01")

parse_date("01/02/15","%m/%d/%y")
parse_date("01/02/15","%d/%m/%y")
parse_date("01/02/15","%y/%m/%d")
# ex ----------------------------------------------------------------------

d1 <- "January 1, 2010"
d2 <- "2015-Mar-07"
d3 <- "06-Jun-2017"
d4 <- c("August 19 (2015)", "July 1 (2015)")
d5 <- "12/30/14" # Dec 30, 2014
t1 <- "1705"
t2 <- "11:15:10.12 PM"

parse_date(d1,"%B %d, %Y")
parse_date(d2,"%Y-%b-%d")
parse_date(d3,"%d-%b-%Y")
parse_date(d4,"%B %d (%Y)")
parse_date(d5,"%m/%d/%y")
parse_time(t1,"%H%M")
parse_time(t2,"%I:%M:%OS %p")


# 11.4 parsing file -------------------------------------------------------

guess_parser("2010-10-10")
guess_parser("20101010")
guess_parser("15:01")
guess_parser(c("TRUE","FALSE"))
guess_parser(c("1","2","3"))
guess_parser(c("12,345,121"))
str(parse_guess("2010-10-10"))

challenge <- read_csv(readr_example("challenge.csv"))
problems(challenge)

challenge <- read_csv(
  readr_example("challenge.csv"), 
  col_types = cols(
    # x = col_integer(),
    x = col_double(),
    # y = col_character()
    y = col_date()
  )
)
head(challenge)
tail(challenge)


# 11.4.3 other strategies -------------------------------------------------

challenge2 <- read_csv(readr_example("challenge.csv"),guess_max=1001)
challenge2

challenge3 <- read_csv(readr_example("challenge.csv"),
                       col_types = cols(.default = col_character()))
challenge3


# type_convert
df <- tribble(
  ~x,~y,
  "1","1.21",
  "2","2.32",
  "3","4.56"
)
type_convert(df)
