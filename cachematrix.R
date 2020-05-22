#This function creates a matrix that can cache its inverse

makeCacheMatrix <- function(m = matrix()) {
        inv <- NULL
        #set the matrix
        set <- function(y) {
                m <<- y
                inv <<- NULL
        }
        #get the matrix m
        get <- function(){ m}
        #set the inverse of the matrix 
        setinverse <- function(inverse) {inv <<- inverse}
        #Obtain the inverse of the matrix
        getinverse <- function() {inv}
        #list of methods
        list(set=set, 
             get=get, 
             setinverse=setinverse, 
             getinverse=getinverse)
}
#return the inverse matrix return by makeCacheMatrix. The cacheSolve returns
#the inverse from the chache


cacheSolve <- function(m, ...) {
        #return the inverse of m  
        inv <- m$getinverse()
        #return inverse
        if(!is.null(inv)) {
                return(inv)
        }
        #get matrix from object
        data <- m$get()
        #calulate the inverse
        inv <- solve(data)
        #set inverse
        m$setinverse(inv)
        #return matrix
        inv
}

