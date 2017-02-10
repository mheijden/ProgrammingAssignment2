## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    Inv <- NULL
    set <- function(y){ # set is made to return NULL for Inv while x is filled with new matrix
        x <<- y         #   fill the X of the function one level up (makeCacheMatrix)
        Inv <<- NULL    
    }
    get <- function() x                      # get is to return the original matrix > return x
    setInver <- function(imatr) Inv<<-imatr  # capture the inverted matrix if new
    getInver <- function() Inv               # return the inverted matrix if available
    list(set=set, get =get, setInver =setInver, getInver = getInver) 
        # the list makes the interfaces (functions to get or set values) adressable
}


## Write a short comment describing this function
# if there is an inverse available fetch it into Inv, return that and end function
# otherwise get the data stored in the list interface of makeCacheMatrix
#   invert the matrix and push that into the list interface of makeCacheMatrix
#   and output the Inv(erted matrix)

cacheSolve <- function(x ,...) {
    Inv <- x$getInver()   # calling the listitem as a function to get the value
    if(!is.null(Inv)){    # for testing whether the value is already available
        message("cached Inverse")
        return(Inv)       # and if availabel return the value
    }
    
    data<-x$get()         # otherwise fetch the original
    Inv<-solve(data, ...) # actual action of inverting
    x$setInver(Inv)   # pushing the value into makeCacheMatrix
    Inv
    ## Return a matrix that is the inverse of 'x'
}
