## makeCacheMatrix function creates a special matrix, which is really a list 
## containing 4 functions:
## F 1. set the value of the matrix
## F 2. get the value of the matrix
## F 3. set the value of the inverse of the matrix
## F 4. get the value of the inverse of the matrix
## Input : a matrix
## Output : a list

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function (y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve function returns the inverse of a matrix
## Process: if the inverse is already cached, this value is retrieved
##          else the inverse is computed within the function
## Input: the list obtained with makeCacheMatrix function
## Output: matrix containing the inverse of the matrix 

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}


## Example of use of both functions below
## matrice <- matrix(c(-3,5,6,-1,2,2,1,-1,-1),nrow=3,ncol=3)
## z<-makeCacheMatrix(matrice)
## cacheSolve(z)
## cacheSolve(z)
