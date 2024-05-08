#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  mod_Individuals_server("Individuals_ui_1")
  mod_Individual_by_Sex_server("Individual_by_Sex_ui_1")
  mod_Meta_analysis_server("Meta_analysis_ui_1")
  ################## MST ###########################################
  mod_MST_one_condition_server("MST_one_condition_ui_1")
  mod_MST_Meta_analysis_server("MST_Meta_analysis_ui_1")
  ################## OGF ###########################################
  mod_OGF_Optimal_Group_Formation_server("OGF_Optimal_Group_Formation_1")
  ################## Enrichment ####################################
  mod_Gene_Ontology_server("Gene_Ontology_1")
}
