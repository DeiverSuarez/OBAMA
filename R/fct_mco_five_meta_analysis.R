#' mco_five_meta_analysis
#'
#' @description This function models two macroarray or RNA-seq gene expression datasets,
#' generating a list of genes with maximal changes in expression.

#' @param data1 Gene expression data set.Gene expression data set.
#' @param data2 Gene expression data set.Gene expression data set.
#' @param data3 Gene expression data set.Gene expression data set.
#' @param data4 Gene expression data set.Gene expression data set.
#' @param data5 Gene expression data set.Gene expression data set.
#' @param NF Number of frontiers Pareto-efficient to calculate.
#' @param measurePM_m Three performance measures (1 = mean, 2 = median, 3 = quantail).
#' @param q if measurePM_m = 3, q take the value of the quantile, for examnple q1 = 75.
#'
#' @author Deiver Suárez-Gómez [aut],
#'         Isis Narváez-Bandera [aut],
#'         Clara E. Isaza [aut],
#'         Mauricio Cabrera-Ríos [aut]  
#'        
#' @return A list with 2 elements.
#' \itemize{
#' \item \code{GeneFrontier} is a genes list with maximum expression changes belonging to each Pareto efficient frontier.
#' \item \code{PlotFrontier} is a 3D plot of the Pareto efficient frontiers with the maximum expression genes.
#'
#' }        
#'
#' @references [1] Camacho-Cáceres, K. I. et al. Multiple criteria optimization joint analyses of microarray experiments in lung cancer: from existing microarray data to new knowledge. Cancer Med. 4, 1884–1900 (2015).
#'             [2] Narváez-Bandera, I., Suárez-Gómez, D., Isaza, C. E. & Cabrera-Ríos, M. Multiple Criteria Optimization (MCO): A gene selection deterministic tool in RStudio. PLoS One 17, e0262890 (2022).
#'
#'
#' @examples
#' # Example5: El siguiente ejemplo...
#' #' m1 <- mco_five_meta_analysis(data1 = dataExample1,
#'                        data2 = dataExample2,
#'                        data3 = dataExample3,
#'                        data4 = dataExample4,
#'                        data5 = dataExample5,
#'                        NF = 3,
#'                        measurePM_m = 1,
#'                        q = NULL)
#'
#'
#' @export
mco_five_meta_analysis <- function(data1 = NULL, 
                                   data2 = NULL, 
                                   data3 = NULL,
                                   data4 = NULL, 
                                   data5 = NULL, 
                                   NF, 
                                   measurePM_m = NULL, 
                                   q = NULL
                                   ) 
  {
wnv_e1=data.frame(data1)
wnv_e2=data.frame(data2)
wnv_e3=data.frame(data3)
wnv_e4=data.frame(data4)
wnv_e5=data.frame(data5)

wnv_e1N=colnames(wnv_e1)
wnv_e2N=colnames(wnv_e2)
wnv_e3N=colnames(wnv_e3)
wnv_e4N=colnames(wnv_e4)
wnv_e5N=colnames(wnv_e5)

inter_e1_e2=intersect(wnv_e1N,wnv_e2N)
inter_e3_e4=intersect(wnv_e3N,wnv_e4N)

interAllPD1=intersect(inter_e1_e2,inter_e3_e4)
interAllPD=intersect(interAllPD1,wnv_e5N)

wnv_e11=wnv_e1[,interAllPD]
wnv_e22=wnv_e2[,interAllPD]
wnv_e33=wnv_e3[,interAllPD]
wnv_e44=wnv_e4[,interAllPD]
wnv_e55=wnv_e5[,interAllPD]
#
wnv1=wnv_e11[order(wnv_e11$disease.state),]
wnv11=data.frame(t(wnv1[,-c(1,2)]))
names(wnv11)=wnv1[,1]

wnv12=wnv_e22[order(wnv_e22$disease.state),]
wnv112=data.frame(t(wnv12[,-c(1,2)]))
names(wnv112)=wnv12[,1]

wnv13=wnv_e33[order(wnv_e33$disease.state),]
wnv113=data.frame(t(wnv13[,-c(1,2)]))
names(wnv113)=wnv13[,1]

wnv14=wnv_e44[order(wnv_e44$disease.state),]
wnv114=data.frame(t(wnv14[,-c(1,2)]))
names(wnv114)=wnv14[,1]

wnv15=wnv_e55[order(wnv_e55$disease.state),]
wnv115=data.frame(t(wnv15[,-c(1,2)]))
names(wnv115)=wnv15[,1]

#
M1 <- PMs_M(data=wnv11, 
            measurePM_m = measurePM_m,
            q = q, 
            n_control=table(wnv1[,2])[1],
            n_enfermedad=table(wnv1[,2])[2]
            )
M2 <- PMs_M(data=wnv112,
            measurePM_m = measurePM_m,
            q = q, 
            n_control=table(wnv12[,2])[1],
            n_enfermedad=table(wnv12[,2])[2]
            )
M3 <- PMs_M(data=wnv113,
            measurePM_m = measurePM_m,
            q = q, 
            n_control=table(wnv13[,2])[1],
            n_enfermedad=table(wnv13[,2])[2]
            )
M4 <- PMs_M(data=wnv114, 
            measurePM_m = measurePM_m, 
            q = q,
            n_control=table(wnv14[,2])[1],
            n_enfermedad=table(wnv14[,2])[2]
            )
M5 <- PMs_M(data=wnv115, 
            measurePM_m = measurePM_m, 
            q = q, 
            n_control=table(wnv15[,2])[1],
            n_enfermedad=table(wnv15[,2])[2]
            )
dfPM <- data.frame(M1,
                   M2,
                   M3,
                   M4,
                   M5
                   )

exit <- MCO_five_meta_analysis(data1=data1,
                               data2=data2, 
                               data3=data3,
                               data4=data4,
                               data5=data5, 
                               NF = NF, 
                               M1 = dfPM$M1,
                               M2 = dfPM$M2, 
                               M3 = dfPM$M3, 
                               M4 = dfPM$M4, 
                               M5 = dfPM$M5
                               )
return(exit)  

}