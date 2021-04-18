## Place the comments here that will make a great total description of the different
## functions that will be visible

## Make a short comment that related to the function

makeCacheMatrix <- function(x = matrix()) {
     ## The function will then create a speacial matrix for the function to be inverse
makeCacheMatrix <- function(x = matrix()) { ## Make an argument that is default for the matrix
inv<-NULL ## It will initialize the inv to be the NULL that makes the value of the inverse matrix
        set<-function(y){ ## Will assign the function as new
                x<<-y ## It is in the value of the matrix in a parent environment form
                inv<<-NULL ## The new matrix should be used if it resets to inv to NULL
}
        get<-function()x ## It describes the function
        setinverse<-function(inverse)inv<<-inverse ## It will assign the value of the inv to be n the parent environment form
        getinverse<-function()inv ## It will call the value of the inv form
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) ## It is needed in the function to be reffered
        

## Make a short comment that is relted to the function
## The function will compute the inverse special matrix that will then be returned on by the makeCacheMatrix function that can be seen at the upper part
cacheSolve <- function(x, ...) {
        ## Take back a matrix that is in the inverse of 'x'
        inv<-x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
        return(inv) 
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
