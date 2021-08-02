##Two functions that cache the inverse of a matrix


## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
              ## initialize the inverse property
              m <- NULL
              
              ## setting the matrix
              set <- function(matrix) {
                x <<- matrix
                m <<- NULL
              }
              
              ## getting the matrix
              get <- function(){
                ## return the matrix
                x
              }
              
              ## set the inverse of matrix
              
              setinverse <- function(inverse){
                m <<- inverse
              }
              
              ## get the inverse of matrix
              
              getinverse <- function(){
                
                ## return inverse
                m
              }
              
              ## return the list of methods
              
              list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
              
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        
        if(!is.null(i)){
          message("getting cached data")
          return(i)
        }
        
        ## Get the matrix from our object
        data <- x$get()
        
        ## Calculate the inverse using matrix multiplication
        i <- solve(data) %*% data
        
        ## Set the inverse to the object
        x$setInverse(i)
        
        i
        
}
## recommitting the project