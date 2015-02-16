
## Matrix inversion is a time & resource consuming exercise especially if the function needs to be called repeatedly inside a loop.
## In order to optimize efficiency, it makes sense to compute the inverse of a matrix once and store it in cache memory.
## We can then get the matrix inverse from the stored location instead of calculating it over and over again.

## The functions makeCacheMatrix & cacheSolve help us with the above mentioned objective


## makeCacheMatrix is a wrapper function which creates the functions required to calculate the inverse of a matrix and  
## store it in memory. The matrix for which the inverse is to be calculated is passed as the argument to this function.

makeCacheMatrix <- function (x=matrix()){
        
        Inv <- NULL
        
        set_matrix <- function (y){     ## function to set the matrix for which inverse is to be computed
                x <<- y                 ## creating pointer to the cache location of x  
                Inv <<- NULL            ## creating pointer to the cache location of the Inverse
        }
        
        get_matrix <- function() x      ## function to get the matrix for which inverse is to be computed
        
        set_inverse <- function () {    ## function to calculate and set the inverse
                Inv <<- matrix(NA, nrow(x), ncol(x))
                Inv <<- solve(x)
        }
        
        get_inverse <- function () Inv  ## function to return the inverse stored in cache
        
        list(set_matrix = set_matrix, get_matrix = get_matrix,
             set_inverse = set_inverse, get_inverse = get_inverse)
}

## cacheSolve function returns a matrix that is the inverse of the matrix that is passed to it as an argument.  The other 
## argument to be passed to this function is the output of makeCacheMatrix() 

cacheSolve <- function(y=matrix(), list) {       ## The output of the makeCacheMatrix() is passed as argument to the function via the object 'list'
        x <- list$get_matrix()                  ## get the matrix for which the inverse has been computed and stored in memory
        z <- y  
        
        if(!identical(x, z))            ## check whether the matrix passed via y is identical to x (from the makeCacheMatrix())
        message ("Matrix passed as argument not identical to the argument of makeCacheMatrix()")
           
        else {
        Inv <- list$get_inverse()      
                
        if (is.null(Inv)){              ## if inverse has not been computed yet, compute now
                x <- list$get_matrix()
                Inv <- list$set_inverse()
                return(Inv)
        }
                                
                else {
                        message("returning cached inverse")
                        Inv
                }
        }
                
}
