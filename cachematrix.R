

## Objective is to make a cache matrix from a given matrix and establish its mean
## It comprises of two steps
## First Step creates the matrix that can cache its inverse
## Second Step returns the value of the inverse if calculated in Setp 1

##---------------------------------------------------------------

## Step 1 : Create cache of inverse 

makeCacheMatrix <- function(x = matrix()) {
  
  cacheMatrix <- NULL  ## Initialising cache matrix and assigning Null Value

  ## Establishing the definition 
  set <- function(y) {
    x <<- y
    cacheMatrix <<- NULL
  }
  get <- function() x
  
  ## definition Setcache
  
  setcache <- function(inverse) cacheMatrix <<- inverse
  
  ## definition Getcache
  
  getcache <- function() cacheMatrix
  
  ## Listing the methods
  
  list(set = set,
       get = get,
       setcache = setcache,
       getcache = getcache)
  
}

##-----------------------------------------------------------------------------

## Step 2: Return the value of inverse in the cache

cacheSolve <- function(x, ...) {

  ## checking the content
  
  cacheMatrix <- x$getCache()

  ## returning the results
  
  if (!is.null(cacheMatrix)) {
      return(cacheMatrix)
  }
  
  ## Calculating alternate result
  
  else {
    dMatrix <- x$getMatrix()
    cacheMatrix <- solve(dMatrix, ...)
    x$setCache(cacheMatrix)
    return(cacheMatrix)
  
}

}