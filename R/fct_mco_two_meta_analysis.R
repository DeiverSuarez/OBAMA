#' @param data treatments, number of new lines for test.
#' @param NF number of checks per augmented block.
#' @param p number of blocks (Augmented blocks).
#' @param q1 esto.
#' @return List of parameters with information about of number of blocks, treatments, vector of treatments,
#' number of sites, plot numbers, locations names and the seed number.
#' 
#' @return RCBD layout
#' 
#' @author 
#' Deiver Suarez Gomez
#' 
#' @references Citar articulo
#' 
#' 
#' @examples 
#' # Example1: El siguiente ejemplo...
#' 
#' @export
mco_two_meta_analysis <- function(data1 = NULL, data2 = NULL, NF, measurePM_m = NULL, q = NULL) {
  filedata1 <- data1
  filedata2 <- data2
  wnv_e1 = as.data.frame(filedata1)
  wnv_e2 = as.data.frame(filedata2)
  #
  wnv_e1N=colnames(wnv_e1)
  wnv_e2N=colnames(wnv_e2)
  interAllPD=intersect(wnv_e1N,wnv_e2N)
  wnv_e11=wnv_e1[,interAllPD]
  wnv_e22=wnv_e2[,interAllPD]
  #
  wnv1=wnv_e11[order(wnv_e11[,2]),]
  wnv11=data.frame(t(wnv1[,-c(1,2)]))
  names(wnv11)=wnv1[,1]
  #
  wnv12=wnv_e22[order(wnv_e22[,2]),]
  wnv112=data.frame(t(wnv12[,-c(1,2)]))
  names(wnv112)=wnv12[,1]
  #
  M1 <- PMs_M(data=wnv11, measurePM_m = measurePM_m, q = q, n_control=table(wnv1[,2])[1], n_enfermedad=table(wnv1[,2])[2])
  M2 <- PMs_M(data=wnv112, measurePM_m = measurePM_m, q = q, n_control=table(wnv12[,2])[1], n_enfermedad=table(wnv12[,2])[2])
  dfPM <- data.frame(M1,M2)
  exit <- MCO_two_meta_analysis(data1=data1, data2=data2, NF = NF, M1 = dfPM$M1, M2 = dfPM$M2)
  return(exit)  
}





