## This file contains two functions that saves computational power for the costly computation of
## calculating an inverse matrix. The first function allows the user to create a special matrix
## object that can cache it inverse. The second function calculates then checks whether or not
## the value of a given matrix has been calculated. If so, it returns that value. If not, it
## goes through the process of calculating the matrix. 

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {

    i<-NULL

    ## Pairs the matrix with a value i for storing
    set <-function(y) {
        x <<- y	
        i <<-  NULL
    }
       
    ## Gets the function in question from the local environment   
    get <- function () { 
        x
    }

    ## Solves for the inverse of the function
    setInverse <- function(solve) {
        i <<- solve(x)
    }

    ## Calls the calculated inverse based on the value i, bringing together the three other functions created in the function using the list command
    getInverse <- function () { 
        i
    }
    
    list (set, get, setInverse,getInverse)

}


## Use cache to get the inverse of the matrix
cacheSolve <- function(x, ...) {

    i <- getInverse(x) ##Stores the value of the martix in question as i with the command


    if(!is.null(i)) {
        ## Gets and returns the cached value
        message("getting cached data")
        return(i)
    }

    ## Calculates the inverse in the event the inverse is NOT stored in the cache
    data <- x$get()
    i<-solve(data,...)
    x$setInverse(i)

    ## Returns the value of the calculated inverse
    i
}

## Thanks for taking the time to evaluate! Hope it made sense!     


