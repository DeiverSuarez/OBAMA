#' genes_enrichr 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#library(enrichR)
genes_enrichr <- function (feature_name, database) {
  websiteLive <- getOption("enrichR.live")
  if (websiteLive) {
    enrichR::listEnrichrSites()
    enrichR::setEnrichrSite("Enrichr") # Human genes   
  }
  if (websiteLive) dbs <- enrichR::listEnrichrDbs()
  dbs <- c("GO_Molecular_Function_2023", "GO_Cellular_Component_2023", "GO_Biological_Process_2023")
  if (websiteLive) {
    enriched <- enrichR::enrichr(feature_name, dbs)
  }
  if (websiteLive) enriched1 <- enriched[[dbs[database]]]
  if (websiteLive) {
    plot <- enrichR::plotEnrich(enriched[[database]], showTerms = 20, numChar = 40, y = "Count", orderBy = "P.value")
  }
  return(list(plot = plot, enriched = enriched1))
}