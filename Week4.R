#three different ways to assign a value to a variable
x <- 10
x
x = 5
x
assign("x",25)
x

#count number of missing values in a vector
y <- c(1,2,NA,4,5,6,NA,9,2)
num_values <- sum(is.na(y))
num_values

#require returns logical code that indicates if a package was loaded successfully
#this is useful when we need to load a package from inside function code
ret_library <- library(MASS)
ret_library
detach("package:MASS")
ret_require <- require(MASS)
ret_require
 