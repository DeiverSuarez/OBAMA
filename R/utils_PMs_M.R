PMs_M <- function(data=NULL, measurePM_m = 3, q = 75, n_control, n_enfermedad){
  
  Data=data
  if(measurePM_m==1){
    median_control <- apply(Data[,1:n_control], 1, mean)
    names(median_control)=NULL
    median_disease <- apply(Data[,n_control+1:n_enfermedad], 1, mean)
    names(median_disease)=NULL
  }
  ###        
  if(measurePM_m==2){
    median_control <- apply(Data[,1:n_control], 1, median)
    names(median_control)=NULL
    median_disease <- apply(Data[,n_control+1:n_enfermedad], 1, median)
    names(median_disease)=NULL
  }
  ###
  if(measurePM_m==3){
    median_control <- apply(Data[,1:n_control], 1, quantile,probs=c(as.numeric(input$ValueqPM11)/100))
    names(median_control)=NULL
    median_disease <- apply(Data[,n_control+1:n_enfermedad], 1, quantile,probs=c(as.numeric(input$ValueqPM11)/100))
    names(median_disease)=NULL
  }
  ####
  df_abs_median <- abs(median_control-median_disease )
  ###
  Max_df_abs_median <- max(df_abs_median)
  Min_df_abs_median <- min(df_abs_median)
  ###
  Trans_df_abs_median <- (Max_df_abs_median + Min_df_abs_median) - df_abs_median
  ###
  measures <- Trans_df_abs_median
  return(as.vector(measures))
  
  
  

}




