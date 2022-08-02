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
    
    sidebarLayout(
      sidebarPanel(width = 4,
                   actionButton(ns("button_MST"),
                                "Run",
                                style="color: #fff; 
                                background-color: #337ab7; 
                                border-color: #2e6da4"),
                   hr(),
                   downloadButton(ns("downloadData.MST"),
                                  "Save My MST Results")
      ),
      
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Decision variables",
                             DT::DTOutput(ns("infoMST_one"))
                             ),
                    )
        )
      )
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
