
## 1. WHAT IS THIS
## The following functions can be used to create a 
## "special object" (a list) that stores a matrix and 
## cache's its inverse.


## 2. HOW TO USE
## If you want to use the functions you must create a new
## object and asign it the function makeCacheMatrix()

##     i.e. >mt <- makeCacheMatrix()

## Then, you can store a matrix inside the object (and get it)
## usign $set, and $get.

##     i.e. >mt$set(matrix(1:4, nrow = 2, ncol = 2))
##     i.e  >mt$get()

## Finally, if you want to get the inverse of the matrix, you 
## can use cacheSolve function.


##     i.e. >cacheSolve(mt)

## 3. makeCacheMatrix FUNCtION
## This function creates an special object (a list) which
## contains the functions required for set the matrix (set function), 
## get the matrix (get function), set the inverse of the matrix (setinv 
## function), and get the inverse of the matrix (getinv function). All
## the functions are added to a list that can be stored into an object.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL}
  
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## 4. cacheSolve FUNCTION
## This function can be used for get the inverse of the matrix.
## the function first checks to see if the inverse of the matrix
## has already been calculated in which case, the function gets 
## the inverse from cache. If the inverse of the matrix has not been 
## calculated, the function calculates the inverse and sets the inverse 
## of the matrix in the cache using the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
