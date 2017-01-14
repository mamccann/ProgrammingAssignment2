## Put comments here that give an overall description of what your
## functions do. 

## MakeCacheMatrix creates a special matrix object that can cache its inverse.
## CacheSOlve computes the inverse of the special matrix returned by 
## the function "makeCacheMatrix"

## Write a short comment describing this function
## To save timely computations, the inverse of a matrix is cached once solved so that it can later be retrieved.
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set <- function(y) {
    x<<-y
    inv<<-NULL
  }
  get<-function()x
  setInverse<-function(inverse)inv<<- inverse
  getInverse <-function()inv
  list(set=set,
       get=get,
       setInverse=setInverse,
       getInverse = getInverse)

}


## Write a short comment describing this function
## this function will compute the inverse of the matrix created in the above function or retreive the inverse if it has already been created.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getInverse()
  if(!is.null(inv)){
    message("finding cached data")
    return(inv)
  }
  mat<- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
  
}
