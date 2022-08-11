#' @importFrom stats median quantile
#' 
#MPStady21 <- eventReactive(input$button21,{
  
PMs2 <- function(data=NULL, 
                 measurePMs31 = 1, 
                 measurePMs32 = 1, 
                 measurePMs33 = 1,
                 q1 = 75, 
                 q2 = 75, 
                 q3 = 75)
  {
  
    filedata <- data  
  
    wnv=as.data.frame(filedata)
    wnv1=wnv[order(wnv$disease.state),]
    wnv11=data.frame(t(wnv1[,-c(1,2)]))
    names(wnv11)=wnv1[,1]
    
    Data <- wnv11; n_control <- table(wnv1$disease.state)[1]; 
    n_enfermedad <- table(wnv1$disease.state)[2]
    
    if(measurePMs31==1){
      median_control <- apply(Data[,1:n_control], 1, mean)
      names(median_control)=NULL
      median_disease <- apply(Data[,n_control+1:n_enfermedad], 1, mean)
      names(median_disease)=NULL
    }
    if(measurePMs32==1){
      mean_control <- apply(Data[,1:n_control], 1,mean)
      names( mean_control)=NULL
      mean_disease <- apply(Data[,n_control+1:n_enfermedad], 1, mean)
      names(mean_disease)=NULL
    }
    if(measurePMs33==1){
      mean3_control <- apply(Data[,1:n_control], 1,mean)
      names( mean3_control)=NULL
      mean3_disease <- apply(Data[,n_control+1:n_enfermedad], 1, mean)
      names(mean3_disease)=NULL
    }
    ###        
    if(measurePMs31==2){
      median_control <- apply(Data[,1:n_control], 1, median)
      names(median_control)=NULL
      median_disease <- apply(Data[,n_control+1:n_enfermedad], 1, median)
      names(median_disease)=NULL
    }
    if(measurePMs32==2){
      mean_control <- apply(Data[,1:n_control], 1,median)
      names( mean_control)=NULL
      mean_disease <- apply(Data[,n_control+1:n_enfermedad], 1, median)
      names(mean_disease)=NULL
    }
    if(measurePMs33==2){
      mean3_control <- apply(Data[,1:n_control], 1,median)
      names( mean3_control)=NULL
      mean3_disease <- apply(Data[,n_control+1:n_enfermedad], 1, median)
      names(mean3_disease)=NULL
    }
    ###
    if(measurePMs31==3){
      median_control <- apply(Data[,1:n_control], 1, 
                              quantile,probs=c(as.numeric(q1)/100))
      names(median_control)=NULL
      median_disease <- apply(Data[,n_control+1:n_enfermedad], 1, 
                              quantile,probs=c(as.numeric(q1)/100))
      names(median_disease)=NULL
    }
    if(measurePMs32==3){
      mean_control <- apply(Data[,1:n_control], 1,
                            quantile,probs=c(as.numeric(q2)/100))
      names( mean_control)=NULL
      mean_disease <- apply(Data[,n_control+1:n_enfermedad], 1,
                            quantile,probs=c(as.numeric(q2)/100))
      names(mean_disease)=NULL
    }
    if(measurePMs33==3){
      mean3_control <- apply(Data[,1:n_control], 1,
                             quantile,probs=c(as.numeric(q3)/100))
      names( mean3_control)=NULL
      mean3_disease <- apply(Data[,n_control+1:n_enfermedad], 1, 
                             quantile,probs=c(as.numeric(q3)/100))
      names(mean3_disease)=NULL
    }
    
   
    ####
    df_abs_median <- abs(median_control-median_disease )
    df_abs_mean <- abs(mean_control-mean_disease)
    df_abs_mean3 <- abs(mean3_control-mean3_disease)
    ###
    Max_df_abs_median <- max(df_abs_median)
    Min_df_abs_median <- min(df_abs_median)
    Max_df_abs_mean <- max(df_abs_mean)
    Min_df_abs_mean <- min(df_abs_mean)
    Max_df_abs_mean3 <- max(df_abs_mean3)
    Min_df_abs_mean3 <- min(df_abs_mean3)
    ###
    Trans_df_abs_median <- (Max_df_abs_median+Min_df_abs_median)-df_abs_median
    Trans_df_abs_median
    Trans_df_abs_mean <- (Max_df_abs_mean+Min_df_abs_mean)-df_abs_mean
    Trans_df_abs_mean
    Trans_df_abs_mean3 <- (Max_df_abs_mean3+Min_df_abs_mean3)-df_abs_mean3
    Trans_df_abs_mean3
    measures_perfo <- data.frame( Trans_df_abs_median,
                                  Trans_df_abs_mean,
                                  Trans_df_abs_mean3)
    names(measures_perfo) <- c("M1","M2","M3")
    return(measures_perfo)
}
  
