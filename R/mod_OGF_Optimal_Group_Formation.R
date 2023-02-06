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
                   fileInput(
                     ns("fileOGF"),
                     accept = c('text/csv',
                                'text/comma-separated-values,text/plain',
                                '.csv'),
                     label = h5("Incidence matrix")
                   ),
                   actionButton(ns("button_OGF"),
                                "Run",
                                style="color: #fff; 
                                background-color: #337ab7; 
                                border-color: #2e6da4"),
                   hr(),
                   downloadButton(ns("downloadData.MST"),
                                  "Save My OGF Results")
      ),
      
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Decision variables",
                             DT::DTOutput(ns("OGF_ouput")),
                             DT::DTOutput(ns("OGF_data"))
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
    
    filedataOGF <- reactive({
      req(input$fileOGF)
      fileInput <- load_file(input$fileOGF$name, input$fileOGF$datapath)
      fileInput <- as.data.frame(fileInput)
      return(list(fileInput = fileInput))
    })
    
    data_infoOGF <- reactive({
      req(filedataOGF()$fileInput)
      Nrows <- dim(filedataOGF()$fileInput)[1]
      Ncols <- dim(filedataOGF()$fileInput)[2]
      SummaryData <- data.frame(list(N = c(Nrows, Ncols)))
      rownames(SummaryData) <- c("Nrows", "Ncols")
      list(SummaryData = SummaryData)
    })
    
    output$OGF_data <- DT::renderDataTable({
      df <- data_infoOGF()$SummaryData
      DT::datatable(df)
    })
    
    
    OGF <- reactive({
      set.seed(10)
      gorups = as.numeric(input$Celd)
      mpg= as.numeric(input$NM)
      #incidence_matrix <- matrix(rbinom(32, 1, 0.5), 8, 4)
      incidence_matrix=as.matrix(filedataOGF()$fileInput) 
      OGF_groups(incidence_matrix ,gorups, mpg)
      
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
