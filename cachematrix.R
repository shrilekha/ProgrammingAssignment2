## The below functions create a special matrix object designed to compute 
## the inverse of a matrix and store it in the cache via lexical scoping in R

## This function creates a special matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) 
{
    inv <- NULL
    set <- function(y)
    {
        x <<- y
        inv <<- NULL
    }
    get <- function()
    {
        x
    }
    setinv <- function(inversematrix)
    {
        inv <<- inversematrix
    }
    getinv <- function()
    {
        inv
    }
    list(set=set,get=get, setinv = setinv, getinv = getinv)
    
}


## This function computes the inverse of the special matrix 
## returned by makeCacheMatrix
cacheSolve <- function(x, ...) 
{
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv() 
    
    ## checking if cached data exists 
    if (!is.null(inv))
    {
        message("getting cached data")
        return(inv)
    }
    
    ## Fetching the matrix and computing the inverse, if not already cached. 
    data <- x$get()
        ## solve() returns the inverse of a matrix in R 
    inv <- solve(data, ...) 
    ## Storing in the special matrix object/cache 
    x$setinv(inv)
    inv
  
}