#' String UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_String_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(width = 4,
                   fileInput(
                     ns("feacture_string"),
                     accept = c('text/csv',
                                'text/comma-separated-values,text/plain',
                                '.csv'),
                     label = h5("Genes")
                   ),
                   
                   selectInput(ns("opcion_enriched"), label = "Enriched:",
                               choices = c("COMPARTMENTS"=1, "Component"=2,
                                           "DISEASES"=3, "Function"=4,
                                           "InterPro"=5, "KEGG"=6, "Keyword"=7, 
                                           "NetworkNeighborAL"=8, "PMID"=9,
                                           "Process"=10, "RCTM"=11, "SMART"=12, 
                                           "TISSUES"=13, "WikiPathways"=14),
                               selected = 6)
                   ),
      
      mainPanel(
        tabsetPanel(
          type = "tabs",
          tabPanel("Genes",
                   DT::DTOutput(ns("Genesinfo_string"))
          ),
          tabPanel("Enrichmnt", 
                   DT::DTOutput(ns("result_string_enrichr"))
          ),
          tabPanel("Plot", 
                   #plotOutput(ns("GO_plot"))
          )
        )
      )
      
    )
  )
}
    
#' String Server Functions
#'
#' @noRd 
mod_String_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    filedata <- reactive({
      req(input$feacture_string)
      fileInput <- load_file(input$feacture_string$name,
                             input$feacture_string$datapath)
      fileInput <- as.data.frame(fileInput)
      return(list(fileInput = fileInput))
    })
    
    data_info <- reactive({
      req(filedata()$fileInput)
      Data <- filedata()$fileInput
      list(Data = Data)
    })
    
    output$Genesinfo_string <- DT::renderDataTable({
      df <- data_info()$Data
      DT::datatable(df)
    })
    
    string_enrichr <- reactive({
      req(filedata()$fileInput)
      feature_name <- filedata()$fileInput[,1]
      enriched <- rbioapi::rba_string_enrichment(ids = feature_name, species = 9606)
      
      if(input$opcion_enriched == 1) {
        genes_enrichr_strig <- enriched$COMPARTMENTS
      }
      if(input$opcion_enriched == 2) {
        genes_enrichr_strig <- enriched$Component
      }
      if(input$opcion_enriched == 3) {
        genes_enrichr_strig <- enriched$DISEASES
      }
      if(input$opcion_enriched == 4) {
        genes_enrichr_strig <- enriched$Function
      }
      if(input$opcion_enriched == 5) {
        genes_enrichr_strig <- enriched$InterPro
      }
      if(input$opcion_enriched == 6) {
        genes_enrichr_strig <- enriched$KEGG
      }
      if(input$opcion_enriched == 7) {
        genes_enrichr_strig <- enriched$Keyword
      }
      if(input$opcion_enriched == 8) {
        genes_enrichr_strig <- enriched$NetworkNeighborAL
      }
      if(input$opcion_enriched == 9) {
        genes_enrichr_strig <- enriched$PMID
      }
      if(input$opcion_enriched == 10) {
        genes_enrichr_strig <- enriched$Process
      }
      if(input$opcion_enriched == 11) {
        genes_enrichr_strig <- enriched$RCTM
      }
      if(input$opcion_enriched == 12) {
        genes_enrichr_strig <- enriched$SMART
      }
      if(input$opcion_enriched == 13) {
        genes_enrichr_strig <- enriched$TISSUES
      }
      if(input$opcion_enriched == 14) {
        genes_enrichr_strig <- enriched$WikiPathways
      }
      
      
      return(list(genes_enrichr_strig = genes_enrichr_strig))
    })
    
    output$result_string_enrichr <- DT::renderDataTable({
      df <- string_enrichr()$genes_enrichr_strig
      
      if (is.null(df) || nrow(df) == 0) {
        df <- data.frame(Messege = "Data Unavailable")
      } else {
        if (!is.data.frame(df)) {
          df <- as.data.frame(df)
        }
      }
      
      return(DT::datatable(df))
    })
    
    
    
    
 
  })
}
    
## To be copied in the UI
# mod_String_ui("String_1")
    
## To be copied in the server
# mod_String_server("String_1")
