## The following functions creates a special matrix (a lista really) and then calculates
## the inverse of that matrix. For that I create first the "makeCacheMatrix" which adds to the Matrix
## special attributes to save the matrix inverse in cache when it was previously calculated.
## Then I create the "cacheSolve" function, whichs calculates de inverse of a matrix but only when
## it's the first time that the inverse is being calculated, otherwise it returns the value saved in cache


## Receives a Matrix like argument and returns a special "Matrix" (list)

makeCacheMatrix <- function(x = matrix()) {
            m<-NULL
            set<-function(y){
                  x<<-y
                  m<<-NULL
            }
            get<-function() x
            setinv<-function(inv) m<<-inv
            getinv<-function() m
            list(set = set, get = get,
                 setinv = setinv,
                 getinv = getinv)
}


## Receives a Matrix like argument and returns its inverse, calculated when it doesn't exist or
## obtained from cache when it had already been previously calculated

cacheSolve <- function(x, ...) {
        m<-x$getinv()
        if(!is.null(m)){
          message("getting cached data")
          return(m)
        }
        data<-x$get()
        m<-solve(data,...)
        x$setinv(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
