
## the following consists of 2 functions, and do 2-steps work 
## 1) creates a special "matrix" object that can cache its inverse 
## 2) computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## this function create a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        
        ##IM stands for InverseMatrix, set IM to null
        IM <- NULL
        
	##set the value of the matrix
	set <- function(y) {
                x <<- y
                IM <<- NULL
        }
	
	##get the value of the matrix
        get <- function() x
	
	##set the value of inverse matrix
        setInverseMatrix <- function(solve) IM <<- solve
	
	##get the value of inverse matrix        
	getInverseMatrix <- function() IM
        list(set = set, get = get,
             setInverseMatrix = setInverseMatrix,
             getInverseMatrix = getInverseMatrix)

}


## computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## get InverseMatrix from above function
        IM <- x$getInverseMatrix()
        
        ## if InverseMatrix is cached, print "getting cahed data"
        if(!is.null(IM)) {
                message("getting cached data")
                return(IM)
        }
        
        ## if InverseMatrix is not cached, get the matrix from above function.
        data <- x$get()
        
        ## calculate the InverseMatrix, set it to cache and output
        IM <- solve(data, ...)
        x$setInverseMatrix(IM)
        IM
}

## this is the test
a<-matrix(c(40,60,90,80),nrow=2,ncol=2)
b<-makeCacheMatrix(a)
c<-cacheSolve(b)
