## Matrix inversion can be can be resource intensive and caching the matrix inverse
## will prevent the need to repeatedly compute the matrix inverse.

## makeCacheMatrix creates a matrix that is capable of caching its inverse.
## This function will:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix())
{
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}




## cacheSolve computes the inverse of the matrix.  
## If the inverse has been previously calculated and the matrix hasn't been changed,
## then cacheSolve will retrieve the inverse of the matrix from the cache rather than 
## recalculate the inverse of the matrix.  If the inverse has not been calculated, the 
## function will compute the inverse and store it in the cache using the setinverse function.
## This assumes the matrix is invertable.

cacheSolve <- function(x,...) 
{
  i <- x$getinverse()
  if(!is.null(i)){
        message("getting cached data")
        return(i)
  }
    data <- x$get()
    i <- solve (data, ...)
    x$setinverse(i)
    i
}

## Test
## x = rbind(c(1,-1/2), c(-1/2,1))
## m = makeCacheMatrix(x)
#  m$get()

##      [,1] [,2]
## [1,]  1.0 -0.5
##n[2,] -0.5  1.0

## No matrix inverse in cache in the first run
## > cacheSolve(m)
##        [,1]      [,2]
## [1,] 1.3333333 0.6666667
## [2,] 0.6666667 1.3333333

##  > cacheSolve(m)
## getting cached data
##      [,1]      [,2]
## [1,] 1.3333333 0.6666667
## [2,] 0.6666667 1.3333333