## The program creates first of all a special type of matrix and based on this structure
## it can calculate it for the first time or keep it for future events.

## This function makes a cachematrix, because there were no restrictions in the arguments
## the user will have to explicitly introduce a matrix in this function.
## A correct input for this function is : result <- makeCacheMatrix(matrix(nrow=3,ncol=3,c(1,1,0,1,0,1,0,1,0)))
# By the way, its important to say that in the word "makeCacheMatrix", both "C" and "M" are
# capitalized.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
      x <<- y
      m <<- NULL
  }
  
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  
  #At the end a cachematrix is not other thing than a struct containing the matrix and its
  #respective evaluations.
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
}


## This function returns a matrix that is the inverse of 'x'.
## It will only accept a "matrix" created by the method "makeCacheMatrix"
## Depending on the previous calculation, this method will reply with the answer
## (if it's the first time) or with a message ('if the calculation was previously done')

## It is important to say that in the word "cacheSolve", letter "S" is capitalized.
cacheSolve <- function(x, ...) {
        
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }

  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m

}

