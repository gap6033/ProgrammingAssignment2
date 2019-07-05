## 1. makeCacheMatrix() takes in a matrix argument and the cacheSolve() takes the
##    makeCacheMatrix() function, together with its argument, as its argument.
## 2. When the functions are run for the first time, cacheSolve() calculates the 
##    inverse of the matrix argument and makeCacheMatrix() caches (stores) this value.
## 3. When the function are run for the 2nd time with the same argument, cacheSolve()
##    doesn't have to caclulate the inverse again (thereby saving time), it merely 
##    retrieves the value from makeCacheMatrix() where it has been cached.
## 4. When you put in a new matrix argument in makeCacheMatrix(), the entire process
##    of calculation and caching is repeated.


## makeCachematrix() generates a list of 4 functions defined inside in its function body.

makeCacheMatrix <- function(x = matrix()) { #assigns the matrix argument.
        inv_matrix <- NULL # the intial value of inverse matrix.
        set <- function(y){ #resets the value of previously cached matrix.
                x <<- y # resets the previous matrix in parent environment to "y".
                inv_matrix <<- NULL #resets the inverse of previous matrix to NULL in the parent environment.
        }
        get <- function() x #displays the the matrix as output .
        setinverse <- function(inverse) inv_matrix <<- inverse #assigns the inverse of the matrix calculated in cacheSolve()
        getinverse <- function() inv_matrix #displays the inverse matrix calculated in cacheSolve() as output.
        list("set" = set, "get" = get, #the output of makeCacheMatrix(), a list of the above 4 defined functions.
        "setinverse" = setinverse, "getinverse" = getinverse)
}

## CacheSolve() Retrieves the matrix data from makeCacheMatrix and calculates its inverse. If it is
## processing a particular matrix argument for the first time, it calculates its inverse,
## else it merely retrieves the already calculated and cached inverse matrix from the
## makeCacheMatrix().

cacheSolve <- function(x, ...) {
        inv_matrix <- x$getinverse()## retrieves inverse matrix from makeCacheMatrix
        if (!is.null(inv_matrix)){ #If the above value is not null, the function is not
                print("getting cached data")## executed beyong the if condition.
                inv_matrix
        }
        data <- x$get()#retrives matrix data, if the if-condition is false
        inv_matrix <- solve(data)#calculates the inverse of the retrieved matrix data
        x$setinverse(inv_matrix)#assigns the calculated inverse matrix to inv_matrix
        inv_matrix#the inverse matrix generated as output
}

