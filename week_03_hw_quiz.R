## 1. Write a function that takes a numeric vector and calculates the mean of the observations in the vector.
myMean <- function(v){
  if (missing(v) || !is.vector(v)) 
    stop("Need to provide a vector")
  if (!is.numeric(v))
    stop("Vector must be numeric")
  round(sum(v)/length(v),2)
}

v <- c(1,3,5,6,7)
myMean(v)

## 2. Modify your function in the previous question so that it can handle a numeric vector with missing values.
myMean <- function(v){
  if (missing(v) || !is.vector(v)) 
    stop("Need to provide a vector")
  if (!is.numeric(v)) {
    stop("Vector must be numeric")
  }
  round(sum(v,na.rm = TRUE)/length(v),2)
}

v <- c(1,3,5,NaN,7)
myMean(v)

## 3. Write a function that takes two numeric input values and calculates the greatest common divisor of the two numbers.
gcd <- function(a,b) ifelse (b==0, a, gcd(b, a %% b))
myGCD(108,30)

## 4. Write a function that implements Euclid’s algorithm (you may need to do a bit of research to find this algorithm) for finding the greatest common divisor of two numeric inputs.
myGCD <- function(a, b){
  if (missing(a) || missing(b) || !all.equal(a, as.integer(a)) 
    || !all.equal(b, as.integer(b)) || a <= 0 || b <= 0 )
    stop("Need to provide two positive integer numbers.")
  
  remainder = TRUE
  dividend <- max(a,b)
  divisor <- min(a,b)
  while (remainder) {
    quotent <- dividend %/% divisor
    remainder <- dividend %% divisor
    dividend <- divisor
    if (remainder) divisor <- remainder  
  }  

  return (divisor) 
}

myGCD(108,30)
myGCD(11111,25)
myGCD(-108,30)

## 5. Write a function that takes two numeric inputs x and y and calculates.
myCalc <- function(x,y) {
  if (missing(x) || missing(y)) 
    stop("Need to provide two numbers")
  x^2*y + 2*x*y - x*y^2
}
myCalc(2,8)

## 6. Read in the week-3-price-data.csv and week-3-make-model-data.csv files as data frames and then merge them by the ModelNumber key. Leave the “all” parameters as their defaults. How many observations end up in the result? Is this what you would have expected?

price <- read.table("C:\\Igor\\CUNY\\607_Data_Acq_Mgmt_R\\Week_03\\week-3-price-data.csv",sep = ",")
model <- read.table("C:\\Igor\\CUNY\\607_Data_Acq_Mgmt_R\\Week_03\\week-3-make-model-data.csv",sep = ",")
colnames(price) <- c("ID" ,"ModelNumber" ,"Color", "Mileage", "Price")
colnames(model) <- c("ModelNumber" ,"Make", "Model", "Year")
pm <- merge(price, model, by="ModelNumber")
nrow(pm)

## Result: 28 rows
## Yes, the number of rows was as expected. It's like an inner join in SQL

## 7. Use the data sets from the previous question, but this time merge them so that the rows from the price-data table all appear, even if there is no match in the make-model table.

pm <- merge(price, model, by="ModelNumber", all=TRUE)
nrow(pm)

## Result: 29 rows

## 8. Take your result from question 7 and subset it so that only the 2010 vehicles are included.

subset(pm, Year=="2010")

## 9. Take your result from question 7 and subset it so that only the red cars that cost more than $10,000 are included.

(red <- subset(pm, Color=="Red" & as.numeric(as.character(Price)) > 10000))

## 10. Take your result from question 9 and subset it s that the ModelNumber and Color columns are removed.

(red1 <- subset(red, select = -c(ModelNumber, Color)))

## 11. Write a function that takes as input a character vector and returns a numeric vector with the numbers of characters in each of the elements in the original vector.

word.length <- function(v) {
  nchar(v)
}
v<-c("blue", "red", "green", "violet")
word.length(v)

## 12. Write a function that takes two character vectors of equal length and concatenates them element by element with a space as the separator. Have the function die gracefully if the vectors are the same length.

concat.vectors <- function(a,b) {
  paste(a,b)
}

a<-c("blue", "red", "green", "violet")
b<-c("color", "color","color","color")
concat.vectors(a,b)
b<-c("color")
concat.vectors(a,b)

## 13. Write a function that takes a character vector and returns the substring of three characters that begins with the first vowel in the string. Have the function handle gracefully substrings where this isn’t possible.
v<-"Hello World"
(substr(v, regexpr("[aeiou]", v)[1],regexpr("[aeiou]", v)[1]+2))

## 14. Suppose you have a data frame where one column gives the month (in numeric format), the next gives the day, and the third column gives the year. Use R to create such a data frame (by hand is fine) and then add a fourth column with the date in date format.
df <- data.frame(month=c(1,2,4,3,8),day=c(21,14,5,3,14),year=c(2010,2011,2012,2010,2011))
df$date <- as.Date(paste(df$month,df$day,df$year),format="%m %d %Y")
df
str(df)

## 15. Illustrate the code necessary to take a string of MM-DD-YYYY format and convert it to a date.
(as.Date("06-02-2010",format="%m-%d-%Y"))

## 16. Illustrate the code necessary to take a date and extract the month of the date.
(format(as.Date("2010-05-03"), "%m"))

## 17. Create a sequence of all of the dates from January 1, 2005, to December 31, 2014.
s<-seq(as.Date("2005-01-01"),as.Date("2014-12-31"),1)