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
                   actionButton(ns("button_OGF"),
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
                             DT::DTOutput(ns("OGF_ouput"))
                             #verbatimTextOutput(ns("OGF_ouput"))
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
    
    OGF <- reactive({
      set.seed(10)
      url <- "http://127.0.0.1:5000/OGF"
      body_list <- list("a" = matrix(rbinom(32, 1, 0.5), 8, 4), "C" = 3, "NM" = 5)
      response <- httr::content(httr::POST(url = url, body = body_list, encode = "json"))
      decisions <- unlist(response$decisons)
      decisions_w <- decisions[grepl("w_", decisions)]
      OGF_matrix_decisions <- matrix(data = NA, nrow = length(decisions_w), ncol = 4, byrow = TRUE)
      for(i in 1:length(decisions_w)){
        OGF_matrix_decisions[i,] <- as.vector(unlist(strsplit(decisions_w[i], split = "_")))
      }
      OGF_matrix_decisions <- as.data.frame(OGF_matrix_decisions)
      names(OGF_matrix_decisions) <- c("Variable_decision","Gen","Process", "Group")
      OGF_matrix_decisions$Gen <- as.numeric(OGF_matrix_decisions$Gen)+1
      OGF_matrix_decisions$Process <- as.numeric(OGF_matrix_decisions$Process)+1
      OGF_matrix_decisions$Group <- as.numeric(OGF_matrix_decisions$Group)+1
      objective_value <- response$objective_value
      return(list(OGF_matrix_decisions = OGF_matrix_decisions,objective_value = objective_value))
    }) %>% 
      bindEvent(input$button_OGF)
    
    output$OGF_ouput <- DT::renderDT(
      DT::datatable(OGF()$OGF_matrix_decisions)
    )
    
    
  })
}
    
## To be copied in the UI
# mod_OGF_Optimal_Group_Formation_ui("OGF_Optimal_Group_Formation_1")
    
## To be copied in the server
# mod_OGF_Optimal_Group_Formation_server("OGF_Optimal_Group_Formation_1")
