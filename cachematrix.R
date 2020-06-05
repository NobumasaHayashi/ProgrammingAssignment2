## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        ##Initialize the valuable I
        I <- NULL
        ##Set function: set the value of the matrix 
        set <- function(y){
                x<<-y
                I<<-NULL
        }
        ##get function: get the value of the matrix 
        get <- function() x
        ##setInv function: set the value of the inverse matrix
        setInv <- function(INV) I<<-INV
        ##getInv function: get the value of the inverse matrix
        getInv <- function() I
        ##produce a list of the above functions
        list(set = set, get = get,
             setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## get the inverse matrix from makeCacheMatrix function
        I<- x$getInv()
        ## If the inverse matrix exist, this function show the message below
        ## and return the inverse matrix
        if(!is.null(I)){
                message('getting cached data')
                return(I)
        }
        ## Otherwise, this function calculate the inverse function by 
        ## getting the original matrix from makeCacheMatrix function
        data<-x$get()
        I <- solve(data)
        ## Store the inverse matrix into makeCacheMatrix function
        x$setInv(I)
        ## Return a matrix that is the inverse of 'x'
        I
}
