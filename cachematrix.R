##The following two functions are created to solve the problem
##that stores a matrix and caches its inverse. 

##The first below function, 'makeCacheMatrix' creates a special “matrix”
##Steps are listed in comments
## Step1- Setting the value of makeCacheMatrix
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL                       
        set <- function(y) {            
                x <<- y
                i <<- NULL
        }
##Step2 -Get the value of the matrix
        get <- function() x
##Step3- Set the value of the inverse
        setinverse <- function(inverse) i <<- inverse
##Step4- Get the value of the invesre
        getinverse <- function() i
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}

##This function computes the inverse of the special “matrix” returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
## Return the inverse if its already set
                if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
##Get the matrix from our object
        data <- x$get()
## Calculate the inverse using matrix multiplication
        i <- solve(data, ...)
## Set the inverse to the object
        x$setinverse(i)
## Return the matrix
        i
}
