data_build <- function(f1=NULL,X = NULL, Y = NULL){
  
  etiq=rep(0, length(X))
  data <- data.frame(Y,X,etiq)
  for (i in 1:length(f1)) {
    data[f1[[i]],3]=i
  }
  return(data)
}