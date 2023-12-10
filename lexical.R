library(MASS)
makecCacheMatrix<-function(x=matrix()){
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function()x
  set<-function(inverse)inv<<-inverse
  getinv<-function(){
    inver<-ginv(x)
    inver%*%x
  }
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}

cachesolve<-function(x,...){
  inv<-x$getinv()
  if(!is.null(inv)){
    meassage("getting cached data!")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data,...)
  x$setinv(inv)
  inv
}
