## The overall two functions create a special object that stores a matrix and caches
## its inverse

## The "makeCacheMatrix" function creates a special matrix, which is a list containing 
## a function to: 

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set,
             get = get, 
             setinv = setinv,
             getinv = getinv)
}


## This following function computes the inverse of the special "matrix" returned by 
## the above function. If the inverse has already been calculated (and the matrix has
## not changed), the function should retrieve the inverse. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if (!is.null(inv)) {
                message("retrieving cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        
        return(inv)
}

## Here is just a quick test of the functions:

M <- matrix(c(1,2,3,4), 2,2)
M
M1 <- makeCacheMatrix(M)

cacheSolve(M1)

## The functions work well 

