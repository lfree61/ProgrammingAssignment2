## Together these two function of makeCacheMatrix and cacheSolve will cache a matrix 'x'
## for quicker access (with makeCacheMatrix) and will return the inverse of 
## that matrix (with cacheSolve).

## This function makeCacheMatrix creates and stores a matrix into matrix1 

makeCacheMatrix <- function(x = matrix()) {
        matrix1<- NULL
        set<- function(y){
                x<<- matrix()
                matrix1<<-NULL
        }
        ## above set defines x as a matrix and creates the cached matrix (matrix1)
        
        get<-function()x
        setsolve<- function(solve) matrix1<<- solve
        getsolve<- function() matrix1
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## cacheSolve returns the inverse of matrix 'x' 

cacheSolve <- function(x) {
        matrix1<- x$getsolve()
        if(!is.null(matrix1)){
                message("getting cached matrix")
                return(matrix1)
                ## Returns "getting cached matrix" and inverse of matrix 'x' if 
                ##matrix was previously cached.
        }
        data<-x$get()
        matrix1<-solve(data)
        x$setsolve(matrix1)
        matrix1
        ## Returns a matrix that is the inverse of 'x' if cached matrix was not previously cached.
}
