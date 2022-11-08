MCO_one_disease <- function(Data,NF,M1,M2){
  
  Data=as.data.frame(Data)
  Data=Data[order(Data[,2]),]
  Data1=data.frame(t(Data[,-c(1,2)]))
  names(Data1)=Data[,1]
  
  
  MCO <- function(M1,M2,z = NULL){
    W <- 1000
    if (length(z) != 0){M1[z] = max(M1)}
    if (length(z) != 0){M2[z] = max(M2)}
    
    delta_1 <- W*matrix(1, length(M1), length(M1))
    LM1 <- 1:length(M1)
    for(i in LM1){
      for(j in LM1){
        if(M1[i] < M1[j]) delta_1[i,j] <- -1
        if(M1[i] == M1[j]) delta_1[i,j] <- 0
      }
    }
    delta_2 = W*matrix(1, length(M2), length(M2))
    LM2 <- 1:length(M2)
    for(i in LM2){
      for(j in LM2){
        if(M2[i] < M2[j]) delta_2[i,j] <- -1
        if(M2[i] == M2[j]) delta_2[i,j] <- 0
      }
    }
    alpha <- delta_1+delta_2
    
    Gamma_m1 <- replace(alpha,alpha==0 | alpha==W,W)
    Gamma_m2 <- replace(Gamma_m1,Gamma_m1==2*W ,2*W)
    Gamma_m <- replace(Gamma_m2,Gamma_m2 != 2*W & Gamma_m2 != W,0)
    
    Beta <- apply(Gamma_m, 1, sum)
    B_selec <- Beta<2*W
    grep('TRUE', B_selec, value=FALSE)
  }
  
  MCO_partition <- function(M1,M2,h=NULL){
    X=M1;Y=M2
    genes= rownames(Data1)
    m=100
    
    if (length(h)!=0){X[h]=max(X)}
    if (length(h)!=0){Y[h]=max(Y)}
    
    M1_x=split(X,as.numeric(gl(length(X),trunc(length(X)/m),length(X)))) 
    M2_y=split(Y,as.numeric(gl(length(Y),trunc(length(Y)/m),length(Y)))) 
    names=split(genes,as.numeric(gl(length(genes),trunc(length(genes)/m),length(genes)))) 
    LZ <- length(M1_x)
    z <- vector(mode = "list",length = LZ)
    LM1_x <- 1:LZ
    for (i in LM1_x){
      z[[i]] <- MCO(data.frame(M1_x[i])[,1],data.frame(M2_y[i])[,1])
    }
    front <- vector(mode = "list",length = LZ)
    for (i in LM1_x) {
      front[[i]] <- as.character(names[[i]][z[[i]]])
    }
    #print("######fronter locals######")
    #print(front)
    Lfront <- length(unlist(front))
    LFront <- 1:Lfront
    position <- vector(mode = "numeric",length = Lfront)
    for (i in LFront){ 
      position[i] <- which(genes==unlist(front)[i])
    }
    # print("######position######")
    # print(position)
    f1 <- MCO(X[position],Y[position])
    #print("######name gen########")
    r <- unlist(front)[f1]
    #print(r)
    #return(r)
    LR <- length(r)
    position1 <- vector(mode = "numeric",length = LR)
    Lr <- 1:LR
    for (i in Lr){ 
      position1[i]=which(genes==r[i])
    }
    position1
  }
  MCO_general <- function(M1,M2){
    F_acu=list()
    fr=list()
    f1=MCO_partition(M1,M2)
    F_acu[[1]]=f1
    fr[[1]]=f1
    
    LNF <- 2:NF
    for (i in LNF) {
      fr[[i]]=MCO_partition(M1,M2,h=c(F_acu[[i-1]]))
      F_acu[[i]]=c(F_acu[[i-1]],fr[[i]])
    }
    fr
  }
  N_front <- MCO_general(M1,M2)
  
  ###############nombres a las fronteras
  LNF1 <- 1:NF
  final <- front <- vector(mode = "list",length = NF)
  for (i in LNF1){final[[i]] <- row.names(Data1)[N_front[[i]]] }
  nam <- paste0("Frontier",1:NF)
  names(final) <- nam
  
  ########################################
  #####crear la lista para imprimir#######
  
  frontier <- N_front
  LN_front <- 1:length(frontier)
  for (i in LN_front) {frontier[[i]]=rep(i,length(frontier[[i]]))}
  
  final2 <- data.frame(Gene=unlist(final,use.names = FALSE),Frontier=unlist(frontier,use.names = FALSE)) ## la lista esta aqui!
  
  #write.csv(final2, file="2Genes_terminal_ileum_autismo_Vs_control.csv")  #guardar en csv
  ########################################
  
  # f1=N_front
  # X=M1
  # Y=M2
  # etiq=rep(0, length(X))
  # data <- data.frame(Y,X,etiq)
  # for (i in 1:length(f1)) {
  #   data[f1[[i]],3]=i
  # }
  # grafica <- ggplot2::ggplot(data , ggplot2::aes(X, Y, group = as.factor(etiq) ,color=as.factor(etiq))) +
  #   ggplot2::geom_point()+ggplot2::labs(x = "PM1:Median", y = "PM2:Mean")+
  #   ggplot2::geom_line(ggplot2::aes(linetype=as.factor(etiq)),show.legend = FALSE )+
  #   ggplot2::theme(legend.position = "none") + ggplot2::guides(fill=FALSE, color=FALSE) + ggplot2::theme_bw()
  
  #if(alternative=="front"){return(final)}
  return(list(final2=final2, f1 = N_front, X = M1, Y = M2))
  
  
  
  
  
}