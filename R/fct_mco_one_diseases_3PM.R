#' mco_one_diseases_3PM Function
#' 
#' @description It generates a augmented randomized complete block design (ARCBD).
#'
#' @param data treatments, number of new lines for test.
#' @param NF number of checks per augmented block.
#' @param measurePM1 number of blocks (Augmented blocks).
#' @param measurePM2 serpentine or cartesian. serpentine by default.
#' @param measurePM3 esto..
#' @param q1 esto
#' @param q2 esto...
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
#' m1 <- mco_one_diseases_3PM(data = dataExample, 
#'                        NF = 5, 
#'                        measurePM1 = 1, 
#'                        measurePM2 = 2, 
#'                        measurePM3 = 3,
#'                        q1 = NULL, 
#'                        q2 = NULL,
#'                        q3 = NULL)
#' m1$final2
#' 
#' @export
mco_one_diseases_3PM <- function(data = NULL, NF, measurePMs31 = NULL, measurePMs32 = NULL, measurePMs33 = NULL, q1 = NULL, q2 = NULL, q3 = NULL) {
  dfPM <- PMs2(data=data, measurePMs31=measurePMs31, measurePMs32 = measurePMs32, measurePMs33 = measurePMs33, q1=q1, q2 = q2, q3 = q3)
  dfPM <- as.data.frame(dfPM)
  exit <- MCO_one_disease_3PM(Data=data, NF=NF, M1 = dfPM$M1, M2=dfPM$M2, M3=dfPM$M3)
  return(exit)  
}