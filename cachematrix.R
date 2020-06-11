## this set of functions catches the inverse of a matrix Matrix.

## firstly, this function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
       
                inverted.mat <- NULL                        #set the default values of x and inverted.mat
                set <- function(y) {
                        x <<- y        #assign the input argument(y) to x (in the parent environment)
                        inverted.mat <<- NULL     #assign NULL to inverted.mat in the parent environment. It clears any value of inverted.mat that had been catched by a prior execution of catchmean()
                }
                get <- function() x           #retrieve the value x from the parent environment
                setinverse <- function(solve) inverted.mat <<- solve       #assign the input value(solve) to inverted.mat. By doing that, it becomes possible to access to this value after setinverse() completes.
                getinverse <- function() inverted.mat    #get inverted.mat as the output
                # assigns each of these functions as an element within a list(), and returns it to the parent environment.
                list(set = set, get = get,       #gives the name 'set' to the set() function defined above
                     setinverse = setinverse,
                     getinverse = getinverse)
        }



## Next,this function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. 
## If the inverse has already been calculated (and the matrix has not changed), then `cacheSolve` should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverted.mat <- x$getinverse()
                if(!is.null(inverted.mat)) {
                        message("getting cached data")
                        return(inverted.mat)
                }
                data <- x$get()   #gets the vector from the input object
                inverted.mat <- solve(data, ...)
                x$setinverse(inverted.mat)  #  returns the value of the inverse matrix to the parent environment.
                inverted.mat
        }
        

