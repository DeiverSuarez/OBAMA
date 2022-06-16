#' mco_two_meta_diseases Function
#'
#' @description This function models two macroarray or RNA-seq gene expression datasets,
#' generating a list of genes with maximal changes in expression.

#' @param data1 Gene expression data set.Gene expression data set.
#' @param data2 Gene expression data set.Gene expression data set.
#' @param NF Number of frontiers Pareto-efficient.
#' @param measurePM_m Three performance measures (1 = mean, 2 = median, 3 = quantail).
#' @param q if measurePM_m = 3, q take the value of the quantile, for examnple q1 = 75.
#'
#' @author Deiver Suárez-Gómez [aut],
#'         Isis Narváez-Bandera [aut],
#'         Clara E. Isaza [aut],
#'         Mauricio Cabrera-Ríos [aut]  
#'        
#' @return A  list with 2 elements.
#' \itemize{
#' \item \code{GeneFrontier} is a genes list with maximum expression changes belonging to each Pareto efficient frontier.
#' \item \code{PlotFrontier} is a 2D plot of the Pareto efficient frontiers with the maximum expression genes.
#'
#' }        
#'
#' @references [1] Camacho-Cáceres, K. I. et al. Multiple criteria optimization joint analyses of microarray experiments in lung cancer: from existing microarray data to new knowledge. Cancer Med. 4, 1884–1900 (2015).
#'             [2] Narváez-Bandera, I., Suárez-Gómez, D., Isaza, C. E. & Cabrera-Ríos, M. Multiple Criteria Optimization (MCO): A gene selection deterministic tool in RStudio. PLoS One 17, e0262890 (2022).
#'
#'
#' @examples
#' # Example1: El siguiente ejemplo...
#' #' m1 <- mco_two_meta_analysis(data1 = dataExample1,
#'                        data2 = dataExample2,
#'                        NF = 3,
#'                        measurePM_m = 1,
#'                        q = NULL)
#'
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





