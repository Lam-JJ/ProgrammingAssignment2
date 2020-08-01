## Function makeCacheMatrix receive an  matrix argument
## to initialise the setter and getter, by 
## which nested function be used.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    #Assign y to x in parent environment
    set <- function(y = matrix()){
        x <<- y
        m <<- NULL
    }
    
    #Retrieve matrix x in parent environment
    get <- function() x 
    
    #Assign m as the inverse function
    setinverse <- function(inv) m <<- inv
    
    #Retrieve mean-assigned "m"
    getinverse <- function() m
    
    list(set = set, #gives the name 'set' to set() 
         get = get, #gives the name 'get' to get()
         setinverse = setinverse, 
         #gives the name 'setmean' to setmean() 
         getinverse = getinverse)
    #gives the name 'getmean' to getmean() 
}


## cacheSolve make used of the cached matrix in parent 
## environment to find previously known value, or else
## returns the inverse of the matrix by using solve()
## function.

cacheSolve <- function(x, ...) {
    #Assign m with mean data from argument x
    m <- x$getinverse()
    
    #check the availbility of mean value from x
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    #Produces new mean from new x argument
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
    ## Return a matrix that is the inverse of 'x'
}
