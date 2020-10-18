#Program only valid for square, invertible matrix, prompts user to re-enter information if not a square matrix

#Loading matrixcalc library to easily test if a matrix is singular
library(matrixcalc) 

#Creating a matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
#Creating a function to calculate the inverse of the matrix, first checking if it has already been calculated
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    print(m) #printing instead of return to see the cached information
  }
  matrx <- x$get()
  if (is.square.matrix(matrx))
  {
    inv <- solve (matrx)
    x$setinverse(inv)
    inv
  }
  #Giving instructions to the user instead of just an error message
  else print("Please enter a square, invertible matrix to makeCacheMatrix function and try again.")
}
  
