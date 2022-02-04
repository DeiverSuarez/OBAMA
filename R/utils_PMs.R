#' @importFrom stats median quantile
PMs <- function(data=NULL,measurePM1 = 1, measurePM2 = 1, q1 = 75, q2 = 75){
       filedata <- data
      wnv = as.data.frame(filedata)
      wnv1 = wnv[order(wnv[,2]),]
      wnv11 = as.data.frame(t(wnv1[,-c(1,2)]))
      names(wnv11) = wnv1[,1]
      
      
      Data <- wnv11; n_control <- table(wnv1[,2])[1]; n_enfermedad <- table(wnv1[,2])[2]
      
      if(measurePM1==1){
        median_control <- apply(Data[,1:n_control], 1, mean)
        names(median_control)=NULL
        median_disease <- apply(Data[,n_control+1:n_enfermedad], 1, mean)
        names(median_disease)=NULL
      }
      if(measurePM2==1){
        mean_control <- apply(Data[,1:n_control], 1,mean)
        names( mean_control)=NULL
        mean_disease <- apply(Data[,n_control+1:n_enfermedad], 1, mean)
        names(mean_disease)=NULL
      }
      ###        
      if(measurePM1==2){
        median_control <- apply(Data[,1:n_control], 1, median)
        names(median_control)=NULL
        median_disease <- apply(Data[,n_control+1:n_enfermedad], 1, median)
        names(median_disease)=NULL
      }
      if(measurePM2==2){
        mean_control <- apply(Data[,1:n_control], 1,median)
        names( mean_control)=NULL
        mean_disease <- apply(Data[,n_control+1:n_enfermedad], 1, median)
        names(mean_disease)=NULL
      }
      ###
      if(measurePM1==3){
        median_control <- apply(Data[,1:n_control], 1, quantile,probs=c(as.numeric(q1)/100))
        names(median_control)=NULL
        median_disease <- apply(Data[,n_control+1:n_enfermedad], 1, quantile,probs=c(as.numeric(q1)/100))
        names(median_disease)=NULL
      }
      if(measurePM2==3){
        mean_control <- apply(Data[,1:n_control], 1,quantile,probs=c(as.numeric(q2)/100))
        names( mean_control)=NULL
        mean_disease <- apply(Data[,n_control+1:n_enfermedad], 1, quantile,probs=c(as.numeric(q2)/100))
        names(mean_disease)=NULL
      }
      
      ####
      df_abs_median <- abs(median_control-median_disease )
      df_abs_mean <- abs(mean_control-mean_disease)
      ###
      Max_df_abs_median <- max(df_abs_median)
      Min_df_abs_median <- min(df_abs_median)
      Max_df_abs_mean <- max(df_abs_mean)
      Min_df_abs_mean <- min(df_abs_mean)
      ###
      Trans_df_abs_median <- (Max_df_abs_median+Min_df_abs_median)-df_abs_median
      Trans_df_abs_median
      Trans_df_abs_mean <- (Max_df_abs_mean+Min_df_abs_mean)-df_abs_mean
      Trans_df_abs_mean
      measures_perfo <- data.frame( Trans_df_abs_median,Trans_df_abs_mean)
      names( measures_perfo) <- c("M1","M2")
      return(measures_perfo)
}