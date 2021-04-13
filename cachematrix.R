## The functions below are used to get the vectors of a matrixes by using caching to reduce calculation effort

## This makeCacheMatrix function is used to create a special "matrix": set the matrix, get the matrix, set the vector, get the vector

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setvector <- function(vector) m <<- vector
        getvector <- function() m
        list(set = set, get = get,
             setvector = setvector,
             getvector = getvector)

}


##The following function calculates the vector of the matrix 
##created with the above function. However, it first checks to see if the vector
## has already been calculated. If so, it gets the vector from the cache and skips 
##the computation. Otherwise, it calculates the vector of the data and sets the value 
##of the vector in the cache via the setvector function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getvector()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setvector(m)
        m
}

## Take a example matrix to test the functions
 x <- matrix(1:4,2,2)
example <- makeCacheMatrix(x)
example$get()
example$getvector() ## return "NULL"
cacheSolve(example)
example$getvector() ## retun to the reversed matrix