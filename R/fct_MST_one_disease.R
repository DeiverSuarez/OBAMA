#' MST_one_disease Function
#'
#' @description This function find the most correlated acyclic path between gene
#' changes based on the pairwise Pearson correlation coefficient using one omics datasets.
#'
#' @param data_expresion Gene expression data set.Gene expression data set.
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
#' # Example7: El siguiente ejemplo...
#' #' m1 <- MST_one_disease(data_expresion = dataExample1,
#'                        data_gene = geneList)
#'
#'
#' @export
MST_one_disease <- function(data_expresio = NULL, data_gene = NULL) {
  
  geo.final62333 <- data_expresio 
  geo.final62333 = geo.final62333[order(geo.final62333$disease.state),]
  Front10_GSE62333 <- data_gene 
  MST_Front10_GSE62333 <- geo.final62333[,c("disease.state",Front10_GSE62333$Gene)]
  #
  ContImpGenes <- MST_Front10_GSE62333[MST_Front10_GSE62333$disease.state == "control",]
  ContImpGenes <- ContImpGenes[,-1]
  row.names(ContImpGenes)= c(1:nrow(ContImpGenes))
  ContImpGenes <- dplyr::as_tibble(ContImpGenes)             
  #
  DiseaseImpGenes <-  MST_Front10_GSE62333[MST_Front10_GSE62333$disease.state == "disease",]
  DiseaseImpGenes <- DiseaseImpGenes[,-1]
  row.names(DiseaseImpGenes)= c(1:nrow(DiseaseImpGenes))
  DiseaseImpGenes <- dplyr::as_tibble(DiseaseImpGenes)
  #
  ContFem <- ContImpGenes
  DiseFem <- DiseaseImpGenes
  #
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
  #
  corrFem = cor(dfFem)
  absCorFem= abs(corrFem)
  transforFem = 1- absCorFem
  colnames(transforFem)<-1:ncol(transforFem)
  row.names(transforFem)<-1:ncol(transforFem)
  RoundCorrFem =round(corrFem,4)
  #
  flattenCorrMatrix <- function(cormat) {
    ut <- upper.tri(cormat)
    data.frame(
      row = rownames(cormat)[row(cormat)[ut]],
      column = rownames(cormat)[col(cormat)[ut]],
      cor  = (cormat)[ut]
    )
  }
  arcosFem = flattenCorrMatrix(transforFem)
  arcosFem = as.matrix(arcosFem)
  # to convert columns as numerics
  #apply(arcosFem, 2, as.numeric)
  #sapply(arcosFem, as.numeric)
  class(arcosFem) <- "numeric"
  storage.mode(arcosFem) <- "numeric"
  #
  nodesFem <- 1:ncol(transforFem)
  X <- optrees::getMinimumSpanningTree(nodesFem, arcosFem, algorithm = "Prim",  show.graph = FALSE)
  #
  data <- data.frame(X$tree.arcs) 
  Replaces <- data.frame(from = as.factor(seq(1:ncol(transforFem))), to =colnames(MST_Front10_GSE62333[-1]))
  data$ept1 <-as.factor(data$ept1)
  data$ept2 <-as.factor(data$ept2)
  data$weight <-as.factor(data$weight)
  data1 <- DataCombine::FindReplace(data = data, Var = "ept1", replaceData = Replaces, from = "from", to = "to", exact = TRUE)
  data2 <- DataCombine::FindReplace(data = data1, Var = "ept2", replaceData = Replaces, from = "from", to = "to", exact = TRUE)
  return(data2) 
}