#' OGF_Optimal_Group_Formation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_OGF_Optimal_Group_Formation_ui <- function(id){
  ns <- NS(id)
  tagList(
    
 
  )
}
    
#' OGF_Optimal_Group_Formation Server Functions
#'
#' @noRd 
mod_OGF_Optimal_Group_Formation_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_OGF_Optimal_Group_Formation_ui("OGF_Optimal_Group_Formation_1")
    
## To be copied in the server
# mod_OGF_Optimal_Group_Formation_server("OGF_Optimal_Group_Formation_1")
