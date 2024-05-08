#' Gene_Ontology UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Gene_Ontology_ui <- function(id){
  options(shiny.maxRequestSize=30*1024^8)
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(width = 4,
                   fileInput(
                     ns("feacture"),
                     accept = c('text/csv',
                                'text/comma-separated-values,text/plain',
                                '.csv'),
                     label = h5("Genes")
                   ),
                   radioButtons(
                     ns("GO_ontologies"), 
                     "GO ontologies:",  
                     choices = list("GO_Molecular_Function"=1,
                                    "GO_Cellular_Component"=2,
                                    "GO_Biological_Process"=3
                                    )
                   ),
                   
                   ),
      
      mainPanel(
        tabsetPanel(
          type = "tabs",
          tabPanel("Genes",
                   DT::DTOutput(ns("Genesinfo"))
                   ),
          tabPanel("Enrichmnt", 
                   DT::DTOutput(ns("GO_enrichr"))
                   ),
          tabPanel("Plot", 
                   plotOutput(ns("GO_plot"))
          )
          )
        )
      
      )
 
  )
}
    
#' Gene_Ontology Server Functions
#'
#' @noRd 
mod_Gene_Ontology_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    filedata <- reactive({
      req(input$feacture)
      fileInput <- load_file(input$feacture$name, input$feacture$datapath)
      fileInput <- as.data.frame(fileInput)
      return(list(fileInput = fileInput))
    })
    
    data_info <- reactive({
      req(filedata()$fileInput)
      Data <- filedata()$fileInput
      list(Data = Data)
    })
    
    output$Genesinfo<- DT::renderDataTable({
      df <- data_info()$Data
      DT::datatable(df)
    })
    
    
    GO_genes_enrichr <- reactive({
      req(filedata()$fileInput)
      feature_name <- filedata()$fileInput[,1]
      if(input$GO_ontologies == 1) {
      genes_enrichr <- genes_enrichr(feature_name = feature_name, database = 1)
      }
      if(input$GO_ontologies == 2) {
        genes_enrichr <- genes_enrichr(feature_name = feature_name, database = 2)
      }
      if(input$GO_ontologies == 3) {
        genes_enrichr <- genes_enrichr(feature_name = feature_name, database = 3)
      }
      plot <- genes_enrichr$plot
      enriched <- genes_enrichr$enriched
      return(list(plot = plot, enriched = enriched))
    })
    
    output$GO_enrichr <- DT::renderDataTable({
      df <- GO_genes_enrichr()$enriched
      DT::datatable(df)
    })
    
    output$GO_plot <- renderPlot({
      GO_genes_enrichr()$plot
    })
 
  })
}
    
## To be copied in the UI
# mod_Gene_Ontology_ui("Gene_Ontology_1")
    
## To be copied in the server
# mod_Gene_Ontology_server("Gene_Ontology_1")
