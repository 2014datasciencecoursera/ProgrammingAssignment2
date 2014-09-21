## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix : This function creates a special "matrix" object that can cache its inverse. 
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## Write a short comment describing this function

## makeCacheMatrix returns list of 4 functions: set, get, setinverse, getinverse
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
  
  #set matrix to be used as x
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
  
  #return matrix to be used as x
  get <- function() x
  
  #set inverse metrix of x
  setinverse <- function(inverse) m <<- inverse
  
  #return inverse matrix of x
  getinverse <- function() m
  
  #list of 4 functions
  list(set=set, get=get, setinverse=setinverse,getinverse=getinverse)
}


## Write a short comment describing this function

## argument x is a list that makeCahceMatrix makes. 
## x contains the matrix m we want to calculate its inverse.
## cacheSolve calculate the inverse of m
## if precalcuated inverse of m exists in the cache, 
## cacheSolve returns the it. the function does not calculate it again.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    m <- x$getinverse()
    #check if there is precalucated inverse of the matrix
    if(!is.null(m)){
      
      #if there is one, return the precalculated one.
      message("getting cashed data")
      return(m)
    }
    
    #if not, get the matrix to be calculated from x
    data <- x$get()
    #get the inverse of the matrix(data)
    m <- solve(data,...)
    #set the inverse matrix as the result of previous line
    x$setinverse(m)
    #return the calculated result
    m
}
