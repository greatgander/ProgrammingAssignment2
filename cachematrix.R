##makeCacheMatrix creates list of function to set, get, setInverse and getInverse as req'd

## assign x matrix class
makeCacheMatrix <- function(x = matrix())   {
    
    ## make null matrix to host inverse
    Inv <- NULL

    ## set function created to assign y to x
    set <- function(y)  {
    ## assign y argument to x  
      x <<- y
      y
    
    ## reset I to null if x redefined
      Inv <<- NULL
                        }
    ## get matrix, argument free function
    get <- function() x
  
    ## sets the inverse using solve (inverse) function
    setInverse <- function(solve) Inv <<- solve
  
    ## retrieves inverse, argument free function
    getInverse <- function() Inv
 
  ## makes list of the functions created above
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
    
                                          }
## cacheSolve function checks to see whether inverse of matrix has already been calculated and retrieves this if so
## else calculates the inverse

cacheSolve <- function(x, ...) {
  
  ## retrieve the Inverse function created by setInverse above
  Inv <- x$getInverse()
  
  ## if inverse has been created then Inv will not be null, and is retrieved message returned stating this, inverse returned 
  if(!is.null(Inv)) {
    message("getting cached data")
    return(Inv)
  
                    }
  ## if inverse has not been created then Inv will be null, and x is passed to solve function so calculate inverse
  message("newly calculating data")
  data <- x$get()
  Inv <- solve(data, ...)
  
  ## inverse is set
  x$setInverse(Inv)
  
  ## new inverse value returned
  Inv
                    }

