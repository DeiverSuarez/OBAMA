MCO_two_meta_analysis <- function(data1, data2 , NF, M1, M2){
  if(NF==1){NF=2}
  else NF=NF
  wnv_e1=as.data.frame(data1)
  wnv_e2=as.data.frame(data2)
  
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
  
  MCO <- function(M1,M2,z=NULL){
    W=1000
    if (length(z)!=0){M1[z]=max(M1)}
    if (length(z)!=0){M2[z]=max(M2)}
    
    delta_1 = W*matrix(1, length(M1), length(M1))
    for(i in 1:length(M1)){
      for(j in 1:length(M1)){
        if(M1[i] < M1[j]) delta_1[i,j] <- -1
        if(M1[i] == M1[j]) delta_1[i,j] <- 0
      }
    }
    delta_2 = W*matrix(1, length(M2), length(M2))
    for(i in 1:length(M2)){
      for(j in 1:length(M2)){
        if(M2[i] < M2[j]) delta_2[i,j] <- -1
        if(M2[i] == M2[j]) delta_2[i,j] <- 0
      }
    }
    alpha <- delta_1+delta_2
    Gamma_m1 <- replace(alpha,alpha==0 | alpha==W, 2/2*W)
    Gamma_m2 <- replace(Gamma_m1,Gamma_m1==2*W ,2*W)
    Gamma_m <- replace(Gamma_m2,Gamma_m2 != 2*W & Gamma_m2 != 2/2*W,0)
    #
    Beta <- apply(Gamma_m, 1, sum)
    B_selec <- Beta<2*W
    return(grep('TRUE', B_selec, value=FALSE))
  }
  
  ######   
  
  MCO_partition <- function(M1,M2,h=NULL){
    X=M1;Y=M2
    genes= rownames(wnv11)
    m=100
    if (length(h)!=0){X[h]=max(X)}
    if (length(h)!=0){Y[h]=max(Y)}
    M1_x=split(X,as.numeric(gl(length(X),trunc(length(X)/m),length(X)))) 
    M2_y=split(Y,as.numeric(gl(length(Y),trunc(length(Y)/m),length(Y)))) 
    names=split(genes,as.numeric(gl(length(genes),trunc(length(genes)/m),length(genes)))) 
    z=list(c())
    
    for (i in 1: length(M1_x)){
      z[[i]]=MCO(data.frame(M1_x[i])[,1],data.frame(M2_y[i])[,1])
    }
    front=list(c())
    for (i in 1:length(M1_x)) {
      front[[i]]=as.character(names[[i]][z[[i]]])
    }
    #print("######fronter locals######")
    #print(front)
    position=c()
    for (i in 1:length(unlist(front))){ 
      position[i]=which(genes==unlist(front)[i])
    }
    #print("######position######")
    #print(position)
    f1=MCO(X[position],Y[position])
    #print("######name gen########")
    r=unlist(front)[f1]
    print(r)
    
    position1=c()
    for (i in 1:length(r)){ 
      position1[i]=which(genes==r[i])
    }
    return(position1)
  } 
  
  #####
  
  MCO_general <- function(M1,M2){
    F_acu=list(c())
    fr=list(c())
    f1=MCO_partition(M1,M2)
    F_acu[[1]]=f1
    fr[[1]]=f1
    
    for (i in 2:NF) {
      fr[[i]]=MCO_partition(M1,M2,h=c(F_acu[[i-1]]))
      F_acu[[i]]=c(F_acu[[i-1]],fr[[i]])
    }
    
    return(fr)
  } 
  #cat("fontiers Stady 1:\n")
  N_front=MCO_general(M1,M2)
  
  ###############nombres a las fronteras
  
  final=list(c())
  for (i in 1:NF){final[[i]] <- row.names(wnv11)[N_front[[i]]] }
  nam <- paste0("Frontier",1:NF)
  names(final) <- nam
  
  ########################################
  #####crear la lista para imprimir#######
  
  frontier=N_front
  for (i in 1:length(frontier)) {frontier[[i]]=rep(i,length(frontier[[i]]))}
  
  final2=data.frame(Gene=unlist(final,use.names = FALSE),Frontier=unlist(frontier,use.names = FALSE)) ## la lista esta aqui!
  
  return(list(final2=final2, F1 = N_front, X = M1, Y = M2))
  
}