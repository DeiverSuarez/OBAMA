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
mco_four_meta_analysis <- function(data1 = NULL, data2 = NULL, data3 = NULL, data4 = NULL, NF, measurePM_m = NULL, q = NULL) {
 
  wnv_e1=data.frame(data1)
  wnv_e2=data.frame(data2)
  wnv_e3=data.frame(data3)
  wnv_e4=data.frame(data4)
  #
  wnv_e1N=colnames(wnv_e1)
  wnv_e2N=colnames(wnv_e2)
  wnv_e3N=colnames(wnv_e3)
  wnv_e4N=colnames(wnv_e4)
  #
  inter_e1_e2=intersect(wnv_e1N,wnv_e2N)
  inter_e3_e4=intersect(wnv_e3N,wnv_e4N)
  #
  interAllPD=intersect(inter_e1_e2,inter_e3_e4)
  wnv_e11=wnv_e1[,interAllPD]
  wnv_e22=wnv_e2[,interAllPD]
  wnv_e33=wnv_e3[,interAllPD]
  wnv_e44=wnv_e4[,interAllPD]
  #
  wnv1=wnv_e11[order(wnv_e11$disease.state),]
  wnv11=data.frame(t(wnv1[,-c(1,2)]))
  names(wnv11)=wnv1[,1]
  #
  wnv12=wnv_e22[order(wnv_e22$disease.state),]
  wnv112=data.frame(t(wnv12[,-c(1,2)]))
  names(wnv112)=wnv12[,1]
  #
  wnv13=wnv_e33[order(wnv_e33$disease.state),]
  wnv113=data.frame(t(wnv13[,-c(1,2)]))
  names(wnv113)=wnv13[,1]
  #
  wnv14=wnv_e44[order(wnv_e44$disease.state),]
  wnv114=data.frame(t(wnv14[,-c(1,2)]))
  names(wnv114)=wnv14[,1]
  #
  M1 <- PMs_M(data=wnv11, measurePM_m = measurePM_m, q = q, n_control=table(wnv1[,2])[1], n_enfermedad=table(wnv1[,2])[2])
  M2 <- PMs_M(data=wnv112, measurePM_m = measurePM_m, q = q, n_control=table(wnv12[,2])[1], n_enfermedad=table(wnv12[,2])[2])
  M3 <- PMs_M(data=wnv113, measurePM_m = measurePM_m, q = q, n_control=table(wnv13[,2])[1], n_enfermedad=table(wnv13[,2])[2])
  M4 <- PMs_M(data=wnv114, measurePM_m = measurePM_m, q = q, n_control=table(wnv14[,2])[1], n_enfermedad=table(wnv14[,2])[2])
  dfPM <- data.frame(M1,M2,M3,M4)

  exit <- MCO_four_meta_analysis(data1=data1, data2=data2, data3=data3, data4=data4, NF = NF, M1 = dfPM$M1, M2 = dfPM$M2, M3 = dfPM$M3, M4 = dfPM$M4)
  return(exit)  
}