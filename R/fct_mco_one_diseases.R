#' mco_one_diseases Function
#' 
#' @description It generates a augmented randomized complete block design (ARCBD).
#'
#' @param data treatments, number of new lines for test.
#' @param NF number of checks per augmented block.
#' @param measurePM1 number of blocks (Augmented blocks).
#' @param measurePM2 serpentine or cartesian. serpentine by default.
#' @param q1 esto
#' @param q2 esto...
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
#' m1 <- mco_one_diseases(data = dataExample, 
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