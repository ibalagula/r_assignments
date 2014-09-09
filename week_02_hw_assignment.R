## Suppose that you have five customers – James, Mary, Steve, Alex, and Patricia – in line at a store. Use R operations to perform the following tasks in sequence.
## a. Assign the five individuals to a vector called queue.
queue <- c("James", "Mary", "Steve", "Alex", "Patricia")
queue
## b. Update the queue for the arrival of a new patron named Harold.
queue[length(queue)+1] <- "Harold"
queue
## c. Update the queue to reflect the fact that James has finished checking out.
queue <- queue[which(!queue=="James")]
queue
## d. Update the queue to reflect the fact that Pam has talked her way in front of Steve with just one item.
queue <- c(queue[1:(which(queue=="Steve")-1)],"Pam",queue[(which(queue=="Steve")):length(queue)])
queue
## e. Update the queue to reflect the fact that Harold has grown impatient and left.
queue <- queue[which(!queue=="Harold")]
queue
## f. Update the queue to reflect the fact that Alex has grown impatient and left. (Do this as if you do not know what slot Alex currently occupies by number.)
queue <- queue[which(!queue=="Alex")]
queue
## g. Identify the position of Patricia in the queue.
which(queue=="Patricia")
## h. Count the number of people in the queue.
length(queue)
## 2. Modify your answer to quiz exercise 21 so that when you implement the quadratic equation, meaningful output is given whether there are one, two, or no solutions. (Hint: Use the discriminant.)
quadratic.eq <- function(a,b,c){
  valid.error = FALSE 
  if (!is.numeric(a) | a == 0) {
    print ("Parameter (a) must be numeric and not zero")
    valid.error = TRUE
  } 
  if (!is.numeric(b)) {
    print ("Parameter (b) must be numeric")
    valid.error = TRUE
  }
  if (!is.numeric(c)) {
    print ("Parameter (c) must be numeric")
    valid.error = TRUE
  }
  if (valid.error) { return (0)}

  print (sprintf('The quadratic equation is: %dx^2+(%d)x+(%d)=0',as.numeric(a),as.numeric(b),as.numeric(c)))

  discriminant <- b^2-4*a*c

  if (discriminant == 0) {
     x1 <- (-b+sqrt(discriminant))/(2*a) 
     print( sprintf("This quadratic equation has two identical real solutons x = %d", as.numeric(x1))) 
  } else if  (discriminant > 0) {
      x1 <- (-b+sqrt(discriminant))/(2*a)
      x2 <- (-b-sqrt(discriminant))/(2*a) 
      print(sprintf("This quadratic equation has two real solutons x1 = %d, x2 = %d", as.numeric(x1), as.numeric(x2))) 
  }  else  {
 
      print(sprintf("This quadratic equation has two imaginary solutons x1 = (-%d+i*sqrt(%d))/2*%d, x1 = (-%d-i*sqrt(%d))/2*%d", 
      as.numeric(b), as.numeric(discriminant),as.numeric(a),as.numeric(b), as.numeric(discriminant),as.numeric(a))) 
  } 
}
quadratic.eq(0,1,"abc")
quadratic.eq(1,1,0)
quadratic.eq(1,1,1)

## 3. Use R to determine how many numbers from 1 to 1000 are not divisible by any of 3,7, and 11.
count = 0
for (i in 1:1000) {
  if (i %% 3 != 0 & i %% 7 != 0 & i %% 11 != 0) {
    count <- count + 1
  }
}
print (count)
## 4. Write R code that takes three input constants f, g, and h and determines whether they form a Pythagorean Triple (such that the square of the largest input is equal to the sum of the squares of the other two constants).
is.triple <- function(f,g,h) {
  valid.error = FALSE 
  if (!is.numeric(f)) {
    print ("Parameter (f) must be numeric and not zero")
    valid.error = TRUE
  } 
  if (!is.numeric(g)) {
    print ("Parameter (g) must be numeric")
    valid.error = TRUE
  }
  if (!is.numeric(h)) {
    print ("Parameter (h) must be numeric")
    valid.error = TRUE
  }
  if (valid.error) { return (0)}

  v <- as.numeric(sort(c(f,g,h)))
  if (v[3]^2 == (v[1]^2 + v[2]^2) ) {
    print ("Numbers form a Pythagorean Triple")
  } else {
    print ("Numbers do not form a Pythagorean Triple")
  }  
}
is.triple(1,2,3)
is.triple(5,3,4)
