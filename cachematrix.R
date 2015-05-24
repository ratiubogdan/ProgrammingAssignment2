## There are 2 functions: one that contains a set of functions to handle matrix operations and the 
## second function will calculate and display the inverted matrix for the matrix provided

## makeCacheMatrix: will create a special data structure that will contain a list of 4 functions
##                  that are used to get, set the values of a matrix and get,set the values of the 
##                  inverted matrix for the matrix provided

makeCacheMatrix <- function(x = matrix()) {
  #inv -  short for inverted is a local variable that stores the inverted matrix.
  #       At first inv is initialized to NULL.
  inv <- NULL
  
  #set function: used to set a new value to the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  #get function: will return the currently set matrix
  get <- function() {
    x
  }
  
  #setinv function: will set the inverse of the currently set matrix. 
  #the inverse matrix will be passed as an argument to this function and it will sore it in the inv variable.
  setinv <- function(invert) {
    inv <<- invert
  }
  
  #getinv function: it returns the currently set inverse matrix. 
  getinv <- function() {
    inv
  }
  
  #we return a list of these 4 functions above
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Computes the inverted matrix for the matrix provided. It will first check to see if that matrix is not
## aleady computed. If it is, it will just display it without re-calculating it. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinv()     #get the inverted matrix
  if(!is.null(inv)) {
    message("getting cached data")  #if the inverted matrix is already available, return the cached copy
    return(inv)         #return the inverted matrix
  }
  data <- x$get()       #get the matrix value
  inv <- solve(data, ...) #call sove function that inverses the matrix
  x$setinv(inv)           #store the inverted matrix in the cache
  inv                     #return the inverted matrix
}


##Sample uses: (Used a trick to make a multi line comment )
'  

> source("cachematrix.R")
> m <- matrix(c(1,2,3,4), 2, 2)
> x <- makeCacheMatrix(m)
> cacheSolve(x)
[,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> m <- matrix(c(1,0,0,1),2,2)
> x$set(m)
> x$get()
[,1] [,2]
[1,]    1    0
[2,]    0    1
> cacheSolve(x)
[,1] [,2]
[1,]    1    0
[2,]    0    1
> cacheSolve(x)
getting cached data
[,1] [,2]
[1,]    1    0
[2,]    0    1
> 
'
##End sample uses

