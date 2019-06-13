# Example: Caching the Mean of a Vector
#The first function, makeVector creates a special "vector", which is really a list containing a function to

#set the value of the vector
#get the value of the vector
#set the value of the mean
#get the value of the mean

makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

#The following function calculates the mean of the special "vector" created with the above function. 
#However, it first checks to see if the mean has already been calculated. 
#If so, it gets the mean from the cache and skips the computation. 
#Otherwise, it calculates the mean of the data and sets the value of the mean in the cache via the setmean function.

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

## Below are two functions that are used to create a
## special object that stores a matrix and caches its inverse.

## The first function, `makeCacheMatrix` creates a
## list containing functions to

## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse
## 4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function calculates the inverse of the matrix
## created with the above function. However, it first checks to see if the
## inverse has already been calculated. If so, it get`s the inverse from the
## cache and skips the computation. Otherwise, it calculates the inverse of
## the matrix and sets the value of the inverse in the cache via the `setinverse`
## function.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  ## Return a matrix that is the inverse of 'x'
}

##  some code that I played around with to check the matrix functions...

aMatrix <- makeCacheMatrix(matrix(c(2,4,3,1), nrow=2, ncol=2))
aMatrix$get()
anInverseMatrix <- cacheSolve(aMatrix)
anActualMatrix <- aMatrix$get()
anActualMatrix %*% anInverseMatrix