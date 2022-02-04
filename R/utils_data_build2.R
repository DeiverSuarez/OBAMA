data_build2 <- function(f1=NULL,X = NULL, Y = NULL, Z=NULL){
  
  etiq=rep(0, length(X))
  data <- data.frame(X,Y,Z,etiq)
  for (i in 1:length(f1)) {
    data[f1[[i]],4]=i
  }
  data1=data[order(data$etiq),]
  return(data1)
}