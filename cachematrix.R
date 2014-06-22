## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Function that creates a list of functions to get/set matrix and get/set inverse
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
		#set matrix value
		set <- function(y) {
                x <<- y
                m <<- NULL
        }
		
		#get matrix value
        get <- function() {
			x
		}
		
		#set inverse of the matrix
        setinverse <- function(inverse) {
			m <<- inverse
		}
        
		#get inverse of the matrix
		getinverse <- function() {
			m
		}
		
		#create the list of functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Function that checks if a cached inverse already exists and returns it in case it does. If it does not exist the inverse is calculated and put to cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		m <- x$getinverse()
		
		#in case the inverse already exists in cache
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
		
		#get data
        data <- x$get()
		
		#calculate inverse
        m <- solve(data, ...)
		
		#put inverse into cache
        x$setinverse(m)
        m
}
