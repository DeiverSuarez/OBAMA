#' MCO one diseases 
#' 
#' 
#' @description This function models a macroarray or RNA-seq gene expression dataset, 
#' generating a list of genes with maximal changes in expression. 
#' @param data Gene expression data set.
#' @param NF Number of frontiers Pareto-efficient.
#' @param measurePM1 One performance measures (1 = mean, 2 = median, 3 = quantail)
#' @param measurePM2 Two performance measures (1 = mean, 2 = median, 3 = quantail)
#' @param q1 if measurePM1 = 3, q1 take the value of the quantile, for examnple q1 = 75.
#' @param q2 if measurePM2 = 3, q2 take the value of the quantile, for examnple q1 = 95.
#' @return  list of genes with maximum expression changes belonging to each Pareto efficient frontier,
#'a plot of the Pareto efficient frontiers with the maximum expression genes.
#' 
#' 
#' 
#' @author Deiver Suárez-Gómez [aut],
#'         Isis Narváez-Bandera [aut],
#'         Clara E. Isaza [aut],
#'         Mauricio Cabrera-Ríos [aut]  
#' 
#' @references [1] Camacho-Cáceres, K. I. et al. Multiple criteria optimization joint analyses of microarray experiments in lung cancer: from existing microarray data to new knowledge. Cancer Med. 4, 1884–1900 (2015).
#'             [2] Narváez-Bandera, I., Suárez-Gómez, D., Isaza, C. E. & Cabrera-Ríos, M. Multiple Criteria Optimization (MCO): A gene selection deterministic tool in RStudio. PLoS One 17, e0262890 (2022).
#' 
#' 
#' @examples 
#' # Example1: El siguiente ejemplo...
#' library(OBAMA)
#' X <- dataExample
#' m1 <- mco_one_diseases(data = X, 
#'                        NF = 5, 
#'                        measurePM1 = 1, 
#'                        measurePM2 = 2, 
#'                        q1 = NULL, 
#'                        q2 = NULL)
#' m1$final2
#' 
#' @export
mco_one_diseases <- function(data = NULL, NF, measurePM1 = NULL, measurePM2 = NULL, q1 = NULL, q2 = NULL) {
  dfPM <- PMs(data=data, measurePM1=measurePM1, measurePM2 = measurePM2, q1=q1, q2 = q2)
  dfPM <- as.data.frame(dfPM)
  exit <- MCO_one_disease(Data=data,NF=NF,M1 = dfPM$M1 ,M2=dfPM$M2)
  return(exit)  
}

# Isis prueba