MCO_one_disease_3PM <- function(Data,NF,M1,M2,M3){
  
  Data=as.data.frame(Data)
  Data=Data[order(Data$disease.state),]
  Data1=as.data.frame(t(Data[,-c(1,2)]))
  names(Data1)=Data[,1]
  
  ###  
  
  MCO <- function(M1,M2,M3,z=NULL){
    W=1000
    if (length(z)!=0){M1[z]=max(M1)}
    if (length(z)!=0){M2[z]=max(M2)}
    if (length(z)!=0){M3[z]=max(M3)}
    
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
    
    
    delta_3 = W*matrix(1, length(M3), length(M3))
    for(i in 1:length(M3)){
      for(j in 1:length(M3)){
        if(M3[i] < M3[j]) delta_3[i,j] <- -1
        if(M3[i] == M3[j]) delta_3[i,j] <- 0
      }
    }
    
    
    alpha <- delta_1+delta_2+delta_3
    
    Gamma_m1 <- replace(alpha,alpha==0 | alpha==W |  alpha==2*W,3/2*W)
    Gamma_m2 <- replace(Gamma_m1,Gamma_m1==3*W ,3*W)
    Gamma_m <- replace(Gamma_m2,Gamma_m2 != 3*W & Gamma_m2 != 3/2*W,0)
    
    Beta <- apply(Gamma_m, 1, sum)
    B_selec <- Beta<3*W
    return(grep('TRUE', B_selec, value=FALSE))
  }
  
  ######   
  
  MCO_partition <- function(M1,M2,M3,h=NULL){
    X=M1;Y=M2;Z=M3
    genes= rownames(Data1)
    m=100
    
    if (length(h)!=0){X[h]=max(X)}
    if (length(h)!=0){Y[h]=max(Y)}
    if (length(h)!=0){Z[h]=max(Z)}
    
    M1_x=split(X,as.numeric(gl(length(X),trunc(length(X)/m),length(X)))) 
    M2_y=split(Y,as.numeric(gl(length(Y),trunc(length(Y)/m),length(Y)))) 
    M3_z=split(Z,as.numeric(gl(length(Z),trunc(length(Z)/m),length(Z)))) 
    names=split(genes,as.numeric(gl(length(genes),trunc(length(genes)/m),length(genes)))) 
    z=list(c())
    for (i in 1: length(M1_x)){
      z[[i]]=MCO(data.frame(M1_x[i])[,1],data.frame(M2_y[i])[,1],data.frame(M3_z[i])[,1])
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
    f1=MCO(X[position],Y[position],Z[position])
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
  
  MCO_general <- function(M1,M2,M3){
    #NF=as.numeric(input$NFro1)
    F_acu=list(c())
    fr=list(c())
    f1=MCO_partition(M1,M2,M3)
    F_acu[[1]]=f1
    fr[[1]]=f1
    
    for (i in 2:NF) {
      fr[[i]]=MCO_partition(M1,M2,M3,h=c(F_acu[[i-1]]))
      F_acu[[i]]=c(F_acu[[i-1]],fr[[i]])
    }
    
    return(fr)
  } 
  
  N_front=MCO_general(M1,M2,M3)
  
  ###############nombres a las fronteras
  final=list(c())
  for (i in 1:NF){final[[i]] <- row.names(Data1)[N_front[[i]]] }
  nam <- paste0("Frontier",1:NF)
  names(final) <- nam
  
  ########################################
  #####crear la lista para imprimir#######
  
  frontier=N_front
  for (i in 1:length(frontier)) {frontier[[i]]=rep(i,length(frontier[[i]]))}
  
  final2=data.frame(Gene=unlist(final,use.names = FALSE),Frontier=unlist(frontier,use.names = FALSE)) ## la lista esta aqui!
  
  #write.csv(final2, file="2Genes_terminal_ileum_autismo_Vs_control.csv")  #guardar en csv
  ######################################## 
  
  # f11=N_front
  # X=M1
  # Y=M2
  # Z=M3
  # 
  # etiq=rep(0, length(X))
  # data <- data.frame(X,Y,Z,etiq)
  # for (i in 1:length(f11)) {
  #   data[f11[[i]],4]=i
  # }
  # data1=data[order(data$etiq),]
  # Num_col=dim(table(data1$etiq))
  # 
  # 
  # PM1 = data1$X
  # PM2 = data1$Y
  # PM3 = data1$Z
  # 
  # 
  # 
  # grafica <- plot_ly(x=PM1, y=PM2, z=PM3, type="scatter3d", mode="markers", color=data1$etiq, 
  #                    #colors = c("gray","purple",  "red", "green", "cyan", "blue")
  #                    colors = c("gray", "purple", "red", "green",  "cyan","blue", "coral","deepskyblue", "orange",  "yellow", "pink")  
  # )     %>%
  #   layout(
  #     title = "MCO",
  #     scene = list(
  #       xaxis = list(title = "PM1:median"),
  #       yaxis = list(title = "PM2:mean"),
  #       zaxis = list(title = "PM3:3thQuantile") 
  #     ))
  # 
  # if(alternative=="imprimir"){return(list(final2=final2,grafica=grafica))}
  
  return(list(final2=final2, f1 = N_front, X = M1, Y = M2, Z = M3))
  
}