#
# R-Programming Homework Assignment 2
# Due on 12/21/2014
#
# assign and inverse a matrix, and store the solved matrix in cache
#
# In the following operation :
#   mat * X = C
#   X = mat_inv * C
#
# 'mat' is the given matrix
# 'mat_inv' is the inverse of 'mat'
# You can use the following steps to calculate mat_inv :
#
# mat_cached <- makeCacheMatrix(mat)
# mat_inv <- cacheSolve(mat_cached)
#

# Function makeCacheMatrix(matrix)
#
# This function creates a special "matrix" object that can cache its inverse matrix.
# There are 4 functions associated with the return object:
#
# set() : store a matrix in cache
# get() : retrieve the cached matrix
# setsolve() : store a matrix inverse in cache
# getsolve() : retrieve the cached matrix inverse
#
makeCacheMatrix <- function(mat = matrix()) {
    # sanity check
    if (! is.matrix(mat)) {
        stop("Input should be a matrix.")
    }
    
    if (length(mat) == 0) {
        stop("Matrix size should be larger than 0.")
    }
    
    if (ncol(mat) != nrow(mat)) {
        stop("Matrix has to be square to solve.")
    }
    
    if (sum(sapply(mat, function(g) is.na(g))) > 0) {
        stop("Matrix can not contain any NA.")
    }
    
    # initialization
    if (length(mat) == 0) {
        stop("Matrix size should be larger than 0")
    }
    mat_inv <- NULL
    # set function
    set <- function(m) {
        mat <<- m
        mat_inv <<- NULL
    }
    # get function
    get <- function() { mat }
    # setsolve() function to store calculated inverse matrix in cached
    setsolve <- function(s) { mat_inv <<- s }
    # getsolve() function to retrieve cached data if found
    getsolve <- function() { mat_inv }
    # return function attributes
    list( set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

# Function cacheSolve(mat, ...)
#
# This function searches the cache for a stored matrix inverse associated with
# object 'x', or calculates the inverse and stores in the cache if not found
# or if the matrix was changed.
#
# The matrix inverse is returned.
#
cacheSolve <- function(x, ...) {
    # sanity check
    if (length(x) != 4 || is.null(names(x)) || names(x)[1] != "set") {
        stop("Input must be a cacheMatrix type object.")
    }
    
    # Get inverse matrix from cache
    mat_inv <- x$getsolve()
    if (! is.null(mat_inv)) {
        message("Getting cached inverse matrix")
        return(mat_inv)
    }
    
    # Invert the matrix
    data <- x$get()
    mat_inv <- solve(data, ...)
    
    # store in cache
    x$setsolve(mat_inv)
    
    # return the solved inverse matrix
    mat_inv
}
