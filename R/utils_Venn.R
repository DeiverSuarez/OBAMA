Venn <- function(Data,c,t){
  VennDengue <- Data
  
  Dengue= as.data.frame(VennDengue[,c:t])
  Venn <- venn::venn(Dengue, ilab=TRUE, zcolor = "style", ilcs = 1.2,sncs = 1.3)
  return(list(Venn=Venn))
}