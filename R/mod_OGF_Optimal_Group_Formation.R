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
                   textInput(
                     ns("Celd"),
                     label = h5("Possible group to be formed"),
                     value = "5"),
                   textInput(
                     ns("NM"),
                     label = h5("Maximum number of processes allowed in each group"),
                     value = "4"),
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
                             #DT::DTOutput(ns("infoMST_one"))
                             verbatimTextOutput("default")
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
    
    virtualenv_dir = Sys.getenv('VIRTUALENV_NAME')
    python_path = Sys.getenv('PYTHON_PATH')
    
    # Load virtualenv
    #virtualenv_dir <- Sys.getenv("VIRTUALENV_NAME")
    #python_path <- Sys.getenv("PYTHON_PATH")
    #reticulate::use_python(python_path, required = TRUE)
    #reticulate::use_virtualenv(virtualenv_dir, required = TRUE)
    
    # Load Python code
    #reticulate::source_python("src/my_python_script.py")
    
    
    # Load Python code
    reticulate::source_python('/Users/deiversuarezgomez/Documents/Git_repositories/OBAMA/prue.py')
    output$default <- renderText({
      return(OFP())
    })
 
  })
}
    
## To be copied in the UI
# mod_OGF_Optimal_Group_Formation_ui("OGF_Optimal_Group_Formation_1")
    
## To be copied in the server
# mod_OGF_Optimal_Group_Formation_server("OGF_Optimal_Group_Formation_1")
