## I write a pair of functions that cache the inverse
## of a matrix.
## Matrix inversion is usually a costly computation
## and their maybe some benefit to caching the inverse
## of a matrix rather than compute it repeatedly.

## makeCacheMatrix: This function creates a special "matrix"
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  cachedinverse<-NULL
  set<-function(y){
    x<<-y
    cachedinverse<<-NULL
  }
  get<-function()x
  setinverse<-function(solve) cachedinverse<<-solve
  getinverse<-function() cachedinverse
  list(set=set,get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## cacheSolve: This function computes the inverse of the 
## special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix
## has not changed), 

## then the cachesolve should retrieve the 
## inverse from the cache. 

cacheSolve <- function(x, ...) {
  ## Get Matrix "m" from the Cache
  m<-x$getinverse()
  ## check if not null, Return value read from the Cache
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## if "m" is NULL,get original matrix
  data<-x$get()
  m<-solve(data,...)
  x$setinverse(m)
  ## Return a matrix that is the inverse of 'x'
  m
}
