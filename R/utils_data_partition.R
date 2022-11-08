data_partition <- function(Data_Exp,Data_SEX){
  
GSE99039_SEX <- Data_SEX
geo.final99039 <- Data_Exp
geo.final99039_sex=merge(GSE99039_SEX,geo.final99039,by= 1 )

geo.final99039_sex=geo.final99039_sex[order(geo.final99039_sex[,2]),]
n_Female <- table(geo.final99039_sex[,2])[1]
n_Male <- table(geo.final99039_sex[,2])[2]
geo.final99039_Female = geo.final99039_sex[c(1:n_Female),-2]
geo.final99039_Male = geo.final99039_sex[((n_Female+1):nrow(geo.final99039_sex)),-2]

geo.final99039_sex=geo.final99039_sex[order(geo.final99039_sex[,3]),]
n_control <- table(geo.final99039_sex[,3])[1]
n_disease <- table(geo.final99039_sex[,3])[2]
geo.final99039_Control = geo.final99039_sex[c(1:n_control),-3]
geo.final99039_Disease = geo.final99039_sex[((n_control+1):nrow(geo.final99039_sex)),-3]

return(list(geo.final99039_Female = geo.final99039_Female, geo.final99039_Male = geo.final99039_Male
            , geo.final99039_Control = geo.final99039_Control, geo.final99039_Disease = geo.final99039_Disease))
}