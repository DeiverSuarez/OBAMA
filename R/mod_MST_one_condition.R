#' MST_one_condition UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_MST_one_condition_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(width = 4,
      
      fileInput(ns("MST_expresion"),accept = c('text/csv','text/comma-separated-values,text/plain','.csv'),
                label = h5("Gene Expression Data")),
      fileInput(ns("MST_Genes"),accept = c('text/csv','text/comma-separated-values,text/plain','.csv'),
                label = h5("Genes of Interest")),
      actionButton(ns("button_MST"),"Run",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
      hr(),
      downloadButton(ns("downloadData.MST"), "Save My MST Results")
      ),
      
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("MST table",
                             DT::DTOutput(ns("MST_one_desease"))  
                            ),
                    tabPanel("MST diagrama",
                             networkD3::forceNetworkOutput(ns("MST_diagram"),width = "100%", height = "600px")
                             
                    )
                  )
              )
    )
 
  )
}
    
#' MST_one_condition Server Functions
#'
#' @noRd 
mod_MST_one_condition_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    filedata_MST_expression <- reactive({
      req(input$MST_expresion)
      fileInput <- load_file(input$MST_expresion$name, input$MST_expresion$datapath)
      fileInput <- as.data.frame(fileInput)
      return(list(fileInput = fileInput))
    })
    
    filedata_MST_genes <- reactive({
      req(input$MST_Genes)
      fileInput <- load_file(input$MST_Genes$name, input$MST_Genes$datapath)
      fileInput <- as.data.frame(fileInput)
      return(list(fileInput = fileInput))
    })
    
    MST <- eventReactive(input$button_MST,{
      wnv1 <- as.data.frame(filedata_MST_expression()$fileInput)
      wnv2 <- as.data.frame(filedata_MST_genes()$fileInput)
      outEval <- MST_one_disease(data_expresio = wnv1, data_gene = wnv2)
      return(list(outEval=outEval))
    })
    
    output$MST_one_desease <- DT::renderDataTable({
      df <- as.data.frame(MST())
      df[,3] = 1 - as.numeric(as.character(df[,3]))
      names(df) <- c("Gene1", "Gene2", "Weight")
      DT::datatable(df)
    })
    
    
    #download_Result.MST
    output$downloadData.MST <- downloadHandler(
      filename = function() {
        paste("Result_MST", ".csv", sep = "")
        
      },
      content = function(file) {
        write.csv(MST(), file, row.names = FALSE)
      }
    )
    
    MST_digram <- eventReactive(input$button_MST,{
      Data=as.data.frame(MST())
      diagram <- networkD3::simpleNetwork(Data=Data, linkDistance = 30, charge = -30, fontSize = 11,linkColour = "red",nodeColour ="blue"  )
      return(diagram)
    })
      
    
    
    
    output$MST_diagram <- networkD3::renderForceNetwork({
    MST_digram()
    })
    
    #plotly::plotlyOutput
      
    
    # output$MST_diagram <- renderPlot({
    #   MST_digram()
    # })
 
  })
}
    
## To be copied in the UI
# mod_MST_one_condition_ui("MST_one_condition_ui_1")
    
## To be copied in the server
# mod_MST_one_condition_server("MST_one_condition_ui_1")
