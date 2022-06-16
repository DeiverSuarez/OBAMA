#' MST_two_meta_diseases Function
#'
#' @description This function find the most correlated acyclic path between gene
#' changes based on the pairwise Pearson correlation coefficient using two omics datasets.
#'
#' @param data_expresion1 Gene expression data set.Gene expression data set.
#' @param data_expresion2 Gene expression data set.Gene expression data set.
#' @param data_gene list of genes with maximal changes in expression.
#'
#'
#' @author Deiver Suárez-Gómez [aut],
#'         Isis Narváez-Bandera [aut],
#'         Clara E. Isaza1 [aut],
#'         Mauricio Cabrera-Ríos1 [aut]  
#'        
#' @return is a maximum correlation between gene pairs.
#'          
#' @references Isaza, C. et al. Biological signaling pathways and potential mathematical network representations: biological discovery through optimization. Cancer Med. 7, 1875–1895 (2018).
#
#'  
#' @examples
#' # Example8: El siguiente ejemplo...
#' #' m1 <- MST_tow_disease(data_expresion1 = dataExample1,
#'                        data_expresion2 = dataExample2,
#'                        data_gene = geneList)
#'
#'
#' @export
MST_tow_disease <- function(data_expresion1 = NULL, data_expresion2 = NULL, data_gene = NULL) {
  
  geo.final17612 <- data_expresion1
  geo.final17612 = geo.final17612[order(geo.final17612$disease.state),]
  
  geo.final_SCZ <- data_expresion2
  geo.final_SCZ = geo.final_SCZ[order(geo.final_SCZ$disease.state),]
  
  Front10_GSE17612 <- data_gene
  Front10_GSE17612$Gene <- as.character(Front10_GSE17612$Gene)
  MST_Front10_GSE17612 <- geo.final17612[,c("disease.state",Front10_GSE17612$Gene)]
  MST_Front10_SCZ <- geo.final_SCZ[,c("disease.state",Front10_GSE17612$Gene)]
#
  ContImpGenes <- MST_Front10_GSE17612[MST_Front10_GSE17612$disease.state == "control",]
  ContImpGenes = ContImpGenes[,-1]
  row.names(ContImpGenes)= c(1:nrow(ContImpGenes))
  ContImpGenes <- dplyr::as_tibble(ContImpGenes)               
  class(ContImpGenes)
#
  DiseaseImpGenes <-  MST_Front10_GSE17612[MST_Front10_GSE17612$disease.state == "disease",]
  DiseaseImpGenes = DiseaseImpGenes[,-1]
  row.names(DiseaseImpGenes)= c(1:nrow(DiseaseImpGenes))
  DiseaseImpGenes <- dplyr::as_tibble(DiseaseImpGenes)
  class(DiseaseImpGenes)
  
  ContImpGenes_SCZ <- MST_Front10_SCZ[MST_Front10_SCZ$disease.state == "control",]
  ContImpGenes_SCZ = ContImpGenes_SCZ[,-1]
  row.names(ContImpGenes_SCZ)= c(1:nrow(ContImpGenes_SCZ))
  ContImpGenes_SCZ <- dplyr::as_tibble(ContImpGenes_SCZ)               
  class(ContImpGenes_SCZ)
  
  DiseaseImpGenes_SCZ <-  MST_Front10_SCZ[MST_Front10_SCZ$disease.state == "disease",]
  DiseaseImpGenes_SCZ = DiseaseImpGenes_SCZ[,-1]
  row.names(DiseaseImpGenes_SCZ)= c(1:nrow(DiseaseImpGenes_SCZ))
  DiseaseImpGenes_SCZ <- dplyr::as_tibble(DiseaseImpGenes_SCZ)
  class(DiseaseImpGenes_SCZ)
  
  ContFem = ContImpGenes
  DiseFem <- DiseaseImpGenes
  
  ContFem_SCZ = ContImpGenes_SCZ
  DiseFem_SCZ <- DiseaseImpGenes_SCZ
  
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
  dfFem <- data.frame(t(matrix(unlist(w), nrow=length(w), byrow=T)))
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
  dfFem_SCZ <- data.frame(t(matrix(unlist(W), nrow=length(W), byrow=T)))
  colnames(dfFem_SCZ)<-colnames(DiseFem_SCZ)
  dfFem_SCZ[1:10,1:10]
  
  corrFem = cor(dfFem) + cor(dfFem_SCZ)
  absCorFem= abs(corrFem)
  transforFem = 2- absCorFem
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
  X = optrees::getMinimumSpanningTree(nodesFem, arcosFem, algorithm = "Prim",  show.graph = FALSE)
  
  data <- data.frame(X$tree.arcs) 
  library(DataCombine)
  # Create replacements data frame
  Replaces <- data.frame(from = as.factor(seq(1:ncol(transforFem))), to =colnames(MST_Front10_GSE17612[-1]))
  data$ept1 <-as.factor(data$ept1)
  data$ept2 <-as.factor(data$ept2)
  data$weight <-as.factor(data$weight)
  data1 <- FindReplace(data = data, Var = "ept1", replaceData = Replaces, from = "from", to = "to", exact = TRUE)
  data2 <- FindReplace(data = data1, Var = "ept2", replaceData = Replaces, from = "from", to = "to", exact = TRUE)
  
  return(data2) 
}