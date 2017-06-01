## those couple of functions allow user to save resources in case of time consuming matrix
## inversion, in fact once the inverse matrix is computed it is stored in cache memory, ready
## to be used when necessary

## first function creates and returns a list of function 
## to set and get the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    # initializes "inverse" with the value of NULL
    inverse <- NULL
    #function to store in cache the initial value of matrix and the NULL value of "inverse"
    set <- function(y){
        x <<- y
        inverse <<- NULL
    }
    #function returning the original matrix
    get <- function(){
        x
    }
    #function to give a value to "inverse" function in cache memory
    setinverse <- function(inv){
        inverse <<- inv
    }
    #funct returning the inverse matrix
    getinverse <- function(){
        inverse
    }
    #ist of the 4 functions
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## second function is called giving as arg the original matrix ad returning the inverse
## if the inverse matrix has been already stored in cache the computation is not repeated,
## if not, the result is saved in cache

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data)
    x$setinverse(inverse)
    inverse
}
