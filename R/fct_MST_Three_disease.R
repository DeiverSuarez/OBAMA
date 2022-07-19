#' MST_three_diseases Function
#'
#' @description This function find the most correlated acyclic path between gene
#' changes based on the pairwise Pearson correlation coefficient using three omics datasets.
#'
#' @param data_expresion1 Gene expression data set.Gene expression data set.
#' @param data_expresion2 Gene expression data set.Gene expression data set.
#' @param data_expresion3 Gene expression data set.Gene expression data set.
#' @param data_gene list of genes with maximal changes in expression.
#'
#'
#' @author Deiver Suárez-Gómez [aut],
#'         Isis Narváez-Bandera [aut],
#'         Clara E. Isaza [aut],
#'         Mauricio Cabrera-Ríos [aut]  
#'        
#' @return is a maximum correlation between gene pairs.
#'          
#' @references Isaza, C. et al. Biological signaling pathways and potential mathematical network representations: biological discovery through optimization. Cancer Med. 7, 1875–1895 (2018).
#
#'  
#' @examples
#' # Example9: El siguiente ejemplo...
#' #' m1 <- MST_three_disease(data_expresion1 = dataExample1,
#'                        data_expresion2 = dataExample2,
#'                        data_expresion3 = dataExample3,
#'                        data_gene = geneList)
#'
#'
#' @export
MST_three_disease <- function(data_expresion1 = NULL, 
                              data_expresion2 = NULL, 
                              data_expresion3 = NULL, 
                              data_gene = NULL
                              ) 
  {
  
  Data_femaleGSE43190 <- data_expresion1
  geo.final43190 <- Data_femaleGSE43190
  geo.final43190 = geo.final43190[order(geo.final43190$disease.state),]
  
  GSE46681_PBMCs_Severe_female <- data_expresion2
  geo.final_46681_sev <- GSE46681_PBMCs_Severe_female
  geo.final_46681_sev = geo.final_46681_sev[
                         order(geo.final_46681_sev$disease.state),]
  
  GSE46681_PBMCs_Asymp_female <- data_expresion3
  geo.final_46681_Asymp <- GSE46681_PBMCs_Asymp_female
  geo.final_46681_Asymp = geo.final_46681_Asymp[
                          order(geo.final_46681_Asymp$disease.state),]
  
  ResultsMeta3D_WNV_Fem <- data_gene
  ResultsMeta3D <- ResultsMeta3D_WNV_Fem
  ResultsMeta3D$Gene <- as.character(ResultsMeta3D$Gene)
  
  MST_Front10_GSE43190 <- geo.final43190[,c("disease.state",
                                            ResultsMeta3D$Gene)]
  MST_Front10_46681_sev <- geo.final_46681_sev[,c("disease.state",
                                                  ResultsMeta3D$Gene)]
  MST_Front10_46681_Asymp <- geo.final_46681_Asymp[,c("disease.state",
                                                      ResultsMeta3D$Gene)]
  
  ContImpGenes <- MST_Front10_GSE43190[
                  MST_Front10_GSE43190$disease.state == "control",]
  ContImpGenes = ContImpGenes[,-1]
  row.names(ContImpGenes)= c(1:nrow(ContImpGenes))
  ContImpGenes <- dplyr::as_tibble(ContImpGenes)              
  class(ContImpGenes)
  
  DiseaseImpGenes <-  MST_Front10_GSE43190[
                       MST_Front10_GSE43190$disease.state == "disease",]
  DiseaseImpGenes = DiseaseImpGenes[,-1]
  row.names(DiseaseImpGenes)= c(1:nrow(DiseaseImpGenes))
  DiseaseImpGenes <- dplyr::as_tibble(DiseaseImpGenes)
  class(DiseaseImpGenes)
  
  ContImpGenes_SCZ <- MST_Front10_46681_sev[
                      MST_Front10_46681_sev$disease.state == "control",]
  ContImpGenes_SCZ = ContImpGenes_SCZ[,-1]
  row.names(ContImpGenes_SCZ)= c(1:nrow(ContImpGenes_SCZ))
  ContImpGenes_SCZ <- dplyr::as_tibble(ContImpGenes_SCZ)              
  class(ContImpGenes_SCZ)

  DiseaseImpGenes_SCZ <-  MST_Front10_46681_sev[
                        MST_Front10_46681_sev$disease.state == "disease",]
  DiseaseImpGenes_SCZ = DiseaseImpGenes_SCZ[,-1]
  row.names(DiseaseImpGenes_SCZ)= c(1:nrow(DiseaseImpGenes_SCZ))
  DiseaseImpGenes_SCZ <- dplyr::as_tibble(DiseaseImpGenes_SCZ)
  class(DiseaseImpGenes_SCZ)
  
  ContImpGenes_Asymp <- MST_Front10_46681_Asymp[
                        MST_Front10_46681_Asymp$disease.state == "control",]
  ContImpGenes_Asymp = ContImpGenes_Asymp[,-1]
  row.names(ContImpGenes_Asymp)= c(1:nrow(ContImpGenes_Asymp))
  ContImpGenes_Asymp <- dplyr::as_tibble(ContImpGenes_Asymp)              
  class(ContImpGenes_Asymp)

  DiseaseImpGenes_Asymp <-  MST_Front10_46681_Asymp[
                           MST_Front10_46681_Asymp$disease.state == "disease",]
  DiseaseImpGenes_Asymp = DiseaseImpGenes_Asymp[,-1]
  row.names(DiseaseImpGenes_Asymp)= c(1:nrow(DiseaseImpGenes_Asymp))
  DiseaseImpGenes_Asymp <- dplyr::as_tibble(DiseaseImpGenes_Asymp)
  class(DiseaseImpGenes_Asymp)
  
  ContFem = ContImpGenes
  DiseFem <- DiseaseImpGenes
  
  ContFem_SCZ = ContImpGenes_SCZ
  DiseFem_SCZ <- DiseaseImpGenes_SCZ
  
  ContFem_Asymp = ContImpGenes_Asymp
  DiseFem_Asymp <- DiseaseImpGenes_Asymp
  
  w=list(c())
  for (k in 1:dim(ContFem)[2]) {
    X=list()
    X[1] = ContFem[,k]
    X[2] = DiseFem[,k]
    y=matrix(0,length(X[[1]]),length(X[[2]]))
    for (i in 1:length(X[[1]])) {
      for (j in 1:length(X[[2]])) {
        y[i,j]=X[[1]][i]-X[[2]][j]
      }
    }
    w[[k]]=c(as.vector(t(y)))
  }
  dfFem <- data.frame(t(matrix(unlist(w), 
                               nrow=length(w), 
                               byrow=T
                               )
                        )
                      )
  colnames(dfFem)<-colnames(DiseFem)
  dfFem[1:10,1:10]
  
  W=list(c())
  for (k in 1:dim(ContFem_SCZ)[2]) {
    X=list()
    X[1] = ContFem_SCZ[,k]
    X[2] = DiseFem_SCZ[,k]
    y=matrix(0,length(X[[1]]),length(X[[2]]))
    for (i in 1:length(X[[1]])) {
      for (j in 1:length(X[[2]])) {
        y[i,j]=X[[1]][i]-X[[2]][j]
      }
    }
    W[[k]]=c(as.vector(t(y)))
  }
  dfFem_SCZ <- data.frame(t(matrix(unlist(W),
                                   nrow=length(W),
                                   byrow=T)
                            )
                          )
  colnames(dfFem_SCZ)<-colnames(DiseFem_SCZ)
  dfFem_SCZ[1:10,1:10]
  
  P=list(c())
  for (k in 1:dim(ContFem_Asymp)[2]) {
    X=list()
    X[1] = ContFem_Asymp[,k]
    X[2] = DiseFem_Asymp[,k]
    y=matrix(0,length(X[[1]]),length(X[[2]]))
    for (i in 1:length(X[[1]])) {
      for (j in 1:length(X[[2]])) {
        y[i,j]=X[[1]][i]-X[[2]][j]
      }
    }
    P[[k]]=c(as.vector(t(y)))
  }
  dfFem_Asymp <- data.frame(t(matrix(unlist(P), 
                                     nrow=length(P), 
                                     byrow=T)
                              )
                            )
  colnames(dfFem_Asymp)<-colnames(DiseFem_Asymp)
  dfFem_Asymp[1:10,1:10]
  
  corrFem = cor(dfFem) + cor(dfFem_SCZ) + cor(dfFem_Asymp)
  absCorFem= abs(corrFem)
  transforFem = 3 - absCorFem
  colnames(transforFem)<-1:ncol(transforFem)
  row.names(transforFem)<-1:ncol(transforFem)
  RoundCorrFem =round(corrFem,4)
  
  flattenCorrMatrix <- function(cormat) {
    ut <- upper.tri(cormat)
    data.frame(
      row = rownames(cormat)[row(cormat)[ut]],
      column = rownames(cormat)[col(cormat)[ut]],
      cor  =(cormat)[ut]
    )
  }
  arcosFem = flattenCorrMatrix(transforFem)
  arcosFem = as.matrix(arcosFem)
  # to convert columns as numerics
  apply(arcosFem, 2, as.numeric)
  sapply(arcosFem, as.numeric)
  class(arcosFem) <- "numeric"
  storage.mode(arcosFem) <- "numeric"
  

  nodesFem <- 1:ncol(transforFem)
  X = optrees::getMinimumSpanningTree(nodesFem, 
                                      arcosFem, 
                                      algorithm = "Prim", 
                                      show.graph = FALSE)
  library(DataCombine)
  data <- data.frame(X$tree.arcs) 
  Replaces <- data.frame(from = as.factor(seq(1:ncol(transforFem))), 
                         to =colnames(MST_Front10_GSE43190[-1]
                                      )
                         )
  data$ept1 <-as.factor(data$ept1)
  data$ept2 <-as.factor(data$ept2)
  data$weight <-as.factor(data$weight)
  data1 <- FindReplace(data = data, Var = "ept1", 
                       replaceData = Replaces, 
                       from = "from", 
                       to = "to", 
                       exact = TRUE
                       )
  data2 <- FindReplace(data = data1, Var = "ept2", 
                       replaceData = Replaces, 
                       from = "from", 
                       to = "to", 
                       exact = TRUE
                       )
  return(data2) 
}
  