## Put comments here that give an overall description of what your
## functions do

## 1.Create a matrix
## 2.Create a makeCheckMatrix object using the matrix
## 3.Check to see if we have already cached the matrix
## 4.If we have a cache version then return the cached inverted version of the matrix
## 5.If we do not have a cached version then invert the matrix using solve() 
##                                      then store the result of the inverstion into the cache

## -----------------------------------------------------------------------------
## A big thank you to Hussain Boxwala's post "Explanation of the example functions for newbies!"
## https://class.coursera.org/rprog-010/forum/thread?thread_id=364
## This helped me understand what was required.

## Also http://www.bluebit.gr/matrix-calculator/ an online matrix calculator
## helped me confirm that I was getting the right result.

## -----------------------------------------------------------------------------
# My test script
## --------------
## source("makeCacheMatrix.R")
## source("cacheSolve.R")
## rm(mt)
## rm(mat)
## mt <- matrix(1:4, nrow=2, ncol=2)
## mat<-makeCacheMatrix(mt)
## cacheSolve(mat)
## cacheSolve(mat)
##
## My test script output
## ---------------------
## > cacheSolve(mat)
## cache empty, storing
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(mat)
## using cache
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## I also ran other tests using the following
## mt <- matrix(rnorm(9), nrow=3, ncol=3)
## mt <- matrix(rnorm(625), nrow=25, ncol=25)

## -----------------------------------------------------------------------------

## Write a short comment describing this function
# Store matix. Return matrix when requested. Initially matrix = NULL
makeCacheMatrix <- function(x = matrix()) {
    mx <- mx
    imx <- NULL
    
    set <- function(y)  mx <<- y      #Store matrix
    
    get <- function() mx              #return matrix
    
    setmatrix <- function(smat){     
        imx <<- smat                  #Cache matrix
    }
    
    getmatrix <- function() {         #Return cached matrix
        if(!is.null(imx)) {
            imx
        }else{
            NULL
        }
    }
    
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
}


## Write a short comment describing this function
##   Test to see if stored matrix exists
##   if it does then retrieve. Return retrieved matrix.
##   If it doesn not calculate inverse and store. Return inverse matrix.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    matm <- x$getmatrix()         #Try and get inverted matrix from cache
    
    if(!is.null(matm)) {          #Is it cached?
        #Get cached matrix
        message("using cache")
        return(matm)
    }else{
        #Cache has not been set
        message("cache empty, storing")
        data <- x$get()            #get matric from x
        invmat <- solve(data)      #invert matrix using solve
        x$setmatrix(invmat)        #store inverted matrix in cache
        return(invmat)
    }
}
