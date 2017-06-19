## Calculates an inverse of the matrix and caches the result. 
#  So that the next time when the function is called it checks if the output is already available
#  If "yes" it outputs cached result or else it calculates the inverse of the matrix and outputs that



## Receives a matrix as input and functions to calculate inverse of a matrix
#  / returns input and solved matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y){ #To input a matrix
    x <<- y
    m <<- NULL
  }
  
  get <- function() x    # Returns the input matrix
    
  setsolve <- function(solve)  m <<- solve #Inverse of the matrix is calculated 
   
  getsolve <- function() m # Returns inverse of a matrix

  #Output list containing Input/Inverse matrix and Set/Solve Input matrix functions
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## makeCacheMatrix object is provided as an input argument
## Verifies if the matrix is already solved/not solved

## Outputs the result by calculating if already not solved or 
#  just retrieves and outputs the result if already solved .

cacheSolve <- function(x, ...) {
    
    m <- x$getsolve() # Returns inverse matrix if already found else 
                      # NULL is returned and inverse is yet to be calculated
    
    if(!is.null(m)) { # Verifies if already solved or not
        message("Retrieving cached data")
        return(m) #Returns cached output // and program terminates here if cached output is found
    }
    data <- x$get() #Retreive input matrix
    
    m <- solve(data, ...) #solve function is initiated for the input data(matrix)
    
    x$setsolve(m) #setsolve function of makeCacheMatrix object is called and inverse of the matrix is calculated
    
    m #Returns inverse of a matrix and program terminates here if cached output is not available
        
}




















