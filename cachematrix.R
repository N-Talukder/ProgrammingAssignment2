## here are two functions which are interdependent. When a numeric matrix is
## fed to the makeCacheMatrix(), then only the output can be sent as an input to
## the cacheSolve() function which looks if the matrix's inverse was calculated 
## once, and if yes, then it returns the cached value and if not, it calculates 
## a new one and returns and also saves it.

## This makes a copy of the matrix fed to it and tries to remember the inverse
## matrix of it, if ever calclualted. It also stores some handles in the returned
## list output so that values can be stored to the makeCacheMatrix() output, if 
## the inverse matrix is calculated from outside.

makeCacheMatrix <- function(matrx = matrix()) {
  
  inv_matrx <- NULL
  
  set <- function(y){
    
    inv_matrx <<- NULL
    matrx <<- y
  }
  
  get <- function() matrx

  setinv <- function(solve) inv_matrx <<-solve
  
  getinv <- function() inv_matrx
  
  list (set = set,
        get = get,
        getinv = getinv,
        setinv = setinv)

}


## the following function takes list output from the above makeCacheMatrix() 
## funciton only. It checks if the inverse of the matrix was previously 
## calculated. If yes, it retrieves the calculated inverse matrix from the 
## list output of the makeCacheMatrix() and returns it with a printed message. 
## If not, then it calclulates a new inverse matrix for that matrix and stores 
## the new calculated inverse matrix to the makeCacheMatrix output usind some 
## handles stored in the list output of that funciton makeCacheMatrix

cacheSolve <- function(cachedM, ...) {
        ## Returns a matrix that is the inverse of 'x'
  
        inv_matrx <- cachedM$getinv()
        
        if(!is.null(inv_matrx)) {
          message("getting cached data")
          return(inv_matrx)
        }
        
        data <- cachedM$get()
        inv_matrx <- solve(data)
        cachedM$setinv(inv_matrx)
        inv_matrx
}
