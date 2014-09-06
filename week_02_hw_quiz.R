## 1. Create a vector that contains 20 numbers. (You may choose whatever numbers you like, but make sure there are some duplicates.)
v <- c(5,4,3,2,1,4,4,22,24,-5,-1,24,19,67,33,76,19,97,18,0)
## 2. Use R to convert the vector from question 1 into a character vector.
ch <- as.character(v)
## 3. Use R to convert the vector from question 1 into a vector of factors.
f <- as.factor(v)
## 4. Use R to show how many levels the vector in the previous question has.
nlevels(f)
## 5. Use R to create a vector that takes the vector from question 1 and performs on it the formula 
v1 <- 3*(v^2)-4*v+1
## 6. Implement ordinary least-squares regression in matrix form: ??^ = (??????)-1??????. As a useful double check you should be able to run your regression on the matrices X and y to get ??^ below:
X<-matrix(c(1,1,1,1,1,1,1,1,5,4,6,2,3,2,7,8,8,9,4,7,4,9,6,4),nrow=8,ncol=3)
y<-matrix(c(45.2,46.9,31.0,35.3,25.0,43.1,41.0,35.1), nrow=8, ncol=1)
b <- (solve(t(X)%*% X) %*% t(X)) %*% y
## 7. Create a named list. That is, create a list with several elements that are each able to be referenced by name.
weekdays <- list(1,2,3,4,5)
names(weekdays) <- c("Mon","Tu", "Wed", "Th", "Fri")
## 8. Create a data frame with four columns – one each character, factor (with three levels), numeric, and date. Your data frame should have at least 10 observations (rows).
L3 <- LETTERS[1:3]
f <- sample(L3, 10, replace = TRUE)
df <- data.frame(Chars=c("a","b","ç","d","e","f","g","h","i","j"),Factor = f,Numbers=c(1:10), Dates=c(as.Date("1970-01-01"):(as.Date("1970-01-01")+9)))
## 9. Illustrate how to add a row with a value for the factor column that isn’t already in the list of levels. (Note: You do not need to accomplish this with a single line of code.)
df <- rbind(df,data.frame(Chars="k", Factor="D", Numbers=11, Dates=as.Date("1970-01-01")+10))
## 10. Show the code that would read in a CSV file called temperatures.csv from the current working directory.
csvfile <- read.table("temperatures.csv",sep = ",")
## 11. Show the code that would read in a TSV file called measurements.txt from a directory other than the working directory on your local machine
tsvfile <- read.table("C:\\CUNY\\measurements.txt",sep = "\t")
## 12. Show the code that will read in a delimited file with a pipe separator (the “|” symbol) from a website location. (You may make up an appropriate URL.)
pipefile <- read.table("http://data.princeton.edu/wws509/datasets/effort.dat",sep = "|")
## 13. Write a loop that calculates 12-factorial.
n <- 1
for (i in 1:12){
  n <- n*i
}
## 14. Use a loop to calculate the final balance, rounded to the nearest cent, in an account that earns 3.24% interest compounded monthly after six years if the original balance is $1,500.
n <- 1500
for (i in 1:72){
  n <- n + n*(0.0324/12)
}
n <- round(n,2)
## 15. Create a numeric vector of length 20 and then write code to calculate the sum of every third element of the vector you have created.
v <- c(5,4,3,2,1,4,4,22,24,-5,-1,24,19,67,33,76,19,97,18,0)
s <- sum(v[seq(3, length(v), 3)])
## 16. Use a for loop to calculate 
n <- 0
for (i in 1:10){
  n <- n+2^i
}
## 17. Use a while loop to accomplish the same task as in the previous exercise.
n <- 0
i <- 1
while (i<=10){
  n <- n+2^i
  i <- i+1
}
## 18. Solve the problem from the previous two exercises without using a loop.
sum(c(2)^c(1:10))
## 19. Show how to create a numeric vector that contains the sequence from 20 to 50 by 5.
v<-seq(20,50,5)
## 20. Show how to create a character vector of length 10 with the same word, “example”, ten times.
v<-rep("example",10)
## 21. Show how to take a trio of input numbers a, b, and c and implement the quadratic equation.
a <- readline("Enter value for a:")  
b <- readline("Enter value for b:")  
c <- readline("Enter value for c:")  
sprintf("The quadratic equation is: %dx^2+%dx+%d=0",as.numeric(a),as.numeric(b),as.numeric(c)) 