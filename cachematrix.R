## The following function calculates the mean of the special "vector"
## created with the above function. However, it first checks to see if the
## mean has already been calculated. If so, it `get`s the mean from the
## cache and skips the computation. Otherwise, it calculates the mean of
## the data and sets the value of the mean in the cache via the `setmean`
## function.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  ## assignation function
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function computes the inverse of the special only if the matrix values have been changed

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  ## if the inverse is cached it is not neccesary to execute solve
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setinverse(inverse)
  inverse
}

# TEST CASE
#> mat<-matrix(c(5, 7, 13, 4, 1, 7, 14, 3, 11), nrow=3, ncol=3)
#> mat <- makeCacheMatrix(A)
#> mat$get()
#[,1] [,2] [,3]
#[1,]    5    4   14
#[2,]    7    1    3
#[3,]   13    7   11
# Case 1: Without using Cache
#> cacheSolve(mat)
#[,1]        [,2]         [,3]
#[1,] -0.03311258  0.17880795 -0.006622517
#[2,] -0.12582781 -0.42052980  0.274834437
#[3,]  0.11920530  0.05629139 -0.076158940
# Case 2: using Cache
#> cacheSolve(mat)
#getting cached data
#[,1]        [,2]         [,3]
#[1,] -0.03311258  0.17880795 -0.006622517
#[2,] -0.12582781 -0.42052980  0.274834437
#[3,]  0.11920530  0.05629139 -0.076158940

