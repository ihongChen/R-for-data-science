
# Debugging, condition handling, and
## defensive programming -------------------------------------------------------------------



# traceback()
# browser()


# 9.2.1 determine the sequence of calls -----------------------------------

f <- function(a) g(a)
g <- function(b) h(b)
h <- function(c) i(c)
i <- function(d) "a" + d
f(10)


message2error <- function(code) {
  withCallingHandlers(code, message = function(e) stop(e))
}

f <- function() g()
g <- function() message('Hi')
g()
message2error(g())



# try catch ---------------------------------------------------------------

f1 <- function(x) {
  log(x)
  10
}
f1("x")

f2 <- function(x) {
  try(log(x))
  10
}
f2("a")

f3 <- function(x){
  try({
    a <- 1
    b <- "x"
    a + b
  })
}
f3(1)

success <- try(1 + 2)
failure <- try("a" + "b")
class(success)
class(failure)


elements <- list(1:10,c(-1,10),c(T,F),letters)
results <- lapply(elements,log)
results <- lapply(elements,function(x) try(log(x)))
results

# is.error <- function(x) class(x) == "try-error"
is.error <- function(x) inherits(x,"try-error")
succeed <- !sapply(results,is.error)
str(results[succeed])



# 9.3.2 tryCatch ----------------------------------------------------------

show_condition <- function(code){
  tryCatch(code,
           error = function(c) "error",
           warning = function(c) "warning",
           message = function(c) "message"
           )
}

show_condition(stop("!"))
show_condition(warning("?!"))
show_condition(message("?"))
show_condition(10)


try2 <- function(code,silent=F){
  tryCatch(code,error=function(c){
    msg <- conditionMessage(c)
    if(!silent) message(c)
    invisible(structure(msg,class="try-error"))
  })
}

try2(stop('Hi'),silent=T)


## ex --read.csv2
read.csv2 <- function(file,...){
  tryCatch(read.csv(file,...),error=function(c){
    c$message <- paste0(c$message,"(in",file,")")
  })
}

read.csv2('test')


i <- 1
while(i < 3) {
  tryCatch({
    Sys.sleep(0.5)
    message("Try to escape")
  }, interrupt = function(x) {
    message("Try again!")
    i <<- i + 1
  })
}


# 9.3.3 withCallingHandlers() ---------------------------------------------

f <- function() stop("!")
tryCatch(f(),error = function(e) 1)

withCallingHandlers(f(),error = function(e) 1)



# 9.3.4 custom message ----------------------------------------------------

condition <- function(subclass,message,call=sys.call(-1),...){
  structure(
    class = c(subclass,"condition"),
    list(message = message,call=call),
    ...
  )
}

is.condition <- function(x) inherit(x,"condition")

c <- condition(c("my_error","error"),"This is an error")
signalCondition(c)
stop(c)
warning(c)
message(c)


custom_stop <- function(subclass, message, call = sys.call(-1),
                        ...) {
  c <- condition(c(subclass, "error"), message, call = call, ...)
  stop(c)
}
my_log <- function(x) {
  if (!is.numeric(x))
    custom_stop("invalid_class", "my_log() needs numeric input")
  if (any(x < 0))
    custom_stop("invalid_value", "my_log() needs positive inputs")
  log(x)
}

tryCatch(customStop("my_error", "!"),
         error = function(c) "error",
         my_error = function(c) "my_error"
)
