## 1. Write a function that takes a vector as input and returns the number of missing values in the vector.

numMissing <- function(v){
  if (missing(v)) 
    stop("Need to provide a vector")
  sum(is.na(v))
}

v <- c(1,3,NA,6,NA,9,10,344,NaN)
numMissing(v)

## 2. Write a function that takes a data frame as input and returns a named vector with the number of missing values in each column of the data frame. (The names of the entries should be the corresponding column names of the data frame.) You may use the function from the previous question as part of your solution.

numMissingFrame <- function(df) {
  n <- vector(mode = "numeric", length = ncol(df))
  names(n) <-colnames(df)
  for(i in 1:length(colnames(df))){
    n[i] <- numMissing(df[,i])  
  }
  n
}

df <- data.frame(mpg=c(21.0,NA,22.8,NA,18.7),cyl=c(6,6,4,6,8),hp=c(110,NA,NA,NA,NA), gear=c(4,NA,4,3,3))
numMissingFrame(df)

## 3. Write a function that takes a numeric vector as input and uses it to determine the minimum, the maximum, the mean, the median, the first quartile, the third quartile, the standard deviation of the vector, and the number of missing values. Do not use any built-in functions to do this. Return a named list with the eight desired values in an order you deem best. (You may, if you like, use the function you wrote for question 1.)

get.min <- function(v){
  min <- v[1]
  for (i in 2:(length(v))) {
    if (i<=length(v) & v[i] < min) 
      min <- v[i] 
  }
  min
}

get.max <- function(v){
  max <- v[1]
  for (i in 2:(length(v))) {
    if (i<=length(v) & v[i] > max) 
      max <- v[i] 
  }
  max

}

get.mean <- function(v){
   sum(v)/length(v)
}

get.median <- function(v){
   v <- sort(v)
   ifelse (length(v) %% 2 == 0, (v[length(v)/2] + v[(length(v)/2)+1])/2 , v[(length(v)/2)+1])
}

get.quartile <- function(v,n){
  v <- sort(v)
  ifelse ((n == 1), get.median(v[1:(length(v)/2 - ifelse(length(v) %% 2 == 0,1,0))]), get.median(v[(length(v)/2)+ifelse(length(v) %% 2 == 0,1,0):length(v)])) 
}

get.stdev <- function(v){
  sqrt(sum((v-get.mean(v))^2)/(length(v)-1))
}
descr.numVector <- function(v){
  if (missing(v)) 
    stop("Need to provide a vector")
  if (!is.numeric(v))
    stop("Vector must be numeric")
  vec.names <- c("minimum", "maximum", "mean", "median", "first quartile", "third quartile", "standard deviation", "number of missing values")
  stat <- vector(mode = "list", length = length(vec.names))
  names(stat) <- vec.names 
  
  stat$`number of missing values` <- numMissing(v)
  if ((stat$`number of missing values`) > 0 )
    v <- v[!is.na(v)]
  stat$minimum <- get.min(v)
  stat$maximum <- get.max(v)
  stat$mean <- get.mean(v)
  stat$median <- get.median(v)
  stat$`first quartile` <- get.quartile(v,1) 
  stat$`third quartile` <- get.quartile(v,3) 
  stat$`standard deviation` <- get.stdev(v) 
 
  stat
}

## 4. Write a function that takes a character or factor vector and determines the number of distinct elements in the vector, the most commonly occurring element, the number of times the most commonly occurring element occurs, and the number of missing values. (Be sure to handle ties gracefully.) Have the function return a named list with the desired information in a logical order.
descr.charVector <- function(v){
  if (missing(v)) 
    stop("Need to provide a vector")

  vec.names <- c("number of distinct elements", "most common", "most common occurs", "number of missing values")
  stat <- vector(mode = "list", length = length(vec.names))
  names(stat) <- vec.names 
  
  stat$`number of missing values` <- numMissing(v)
  if ((stat$`number of missing values`) > 0 )
    v <- v[!is.na(v)]
  if (!is.factor(v))
    v<-as.factor(v)
  s<-summary(v)
  max <- get.max(s)
  stat$`number of distinct elements` <- length(levels(v)) 
  stat$`most common` <- levels(v)[which(s==max)] 
  stat$`most common occurs` <- max 
 
  stat
}
v<-c("a","b","c",NA,"d","c","c","d","e","f","d")
descr.charVector(v)

## 5. Write a function that takes a logical vector and determines the number of true values, the number of false values, the proportion of true values, and the number of missing values. Have the function return a named list with the desired information in a logical order.
descr.logicalVector <- function(v){
  if (missing(v)) 
    stop("Need to provide a vector")
  vec.names <- c("number of true values", "number of false values", "proportion of true values", "number of missing values")
  stat <- vector(mode = "list", length = length(vec.names))
  names(stat) <- vec.names
  
  stat$`number of missing values` <- numMissing(v)
  stat$`number of true values` <- sum(v==TRUE,na.rm=TRUE)
  stat$`number of false values` <- sum(v==FALSE,na.rm=TRUE)
  stat$`proportion of true values` <- round(sum(v==TRUE,na.rm=TRUE)/length(v),4) 
  stat
}
v<-c(TRUE, TRUE, NA, FALSE, TRUE, NA, TRUE)
descr.logicalVector(v)


## 6. Write a function that takes as its input a data frame and returns a summary of its columns using the functions you write for questions 3-5. You may assume that all columns will be of the three types in those questions. You are expected to use the functions you have written in the previous questions, so you do not have to write them again by scratch. Return the desired information in a format that you deem best. (One suggestion would be a named list of lists, but I leave it to your judgment.)
descr.dataframe <- function(df){
  if (missing(df) || !is.data.frame(df)) 
    stop("Need to provide a data frame")

  stat <- vector(mode = "list", length = ncol(df))
  names(stat) <- colnames(df) 

  for (i in 1:ncol(df)){
    if (class(df[,i]) == "numeric") {
      stat[[i]] <- (descr.numVector(df[,i]))
    } else if (class(df[,i]) == "logical") {
       stat[[i]] <- (descr.logicalVector(df[,i]))
    } else
        stat[[i]] <- (descr.charVector(df[,i]))
  }
  stat 
}
df <- data.frame(col1=c(21.0,NA,22.8,NA,18.7),col2=c("a","e","b","c","d"),col3=c(TRUE, NA, TRUE, FALSE, TRUE), col4=c(1,0,1,1,0))
descr.dataframe(df)