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
                   downloadButton(ns("downloadData_GO"), "Save My GO Results"),
                   downloadButton(ns("downloadGroup1_GO"), "Save group1"),
                   downloadButton(ns("downloadGroup2_GO"), "Save group2")
                   
                   
                   ),
      
      mainPanel(
        tabsetPanel(
          type = "tabs",
          tabPanel("Genes",
                   DT::DTOutput(ns("Genesinfo"))
                   ),
          tabPanel("Enrichment",
                   tabsetPanel(
            type = "tabs",
            tabPanel("Table",
                     DT::DTOutput(ns("GO_enrichr"))
                     ),
            tabPanel("Group1",
                     DT::DTOutput(ns("GO_group1"))
            ), 
            tabPanel("Group2",
                        DT::DTOutput(ns("GO_group2"))
            )
            ),
                   
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
    
    GO_Group1 <- reactive({
      req(GO_genes_enrichr()$enriched)
      
      df <- GO_genes_enrichr()$enriched
      data <- df[,c(1,9)]
      
      # Separar los genes en filas individuales
      data_long <- data %>%
        tidyr::separate_rows(Genes, sep = ";")
      
      # Agrupar los términos que comparten genes
      grouped_data <- data_long %>%
        dplyr::group_by(Genes) %>%
        dplyr::summarise(Terms = paste(Term, collapse = ","))
      
      return(list(grouped_data = grouped_data))
    })
    
    
    GO_Group2 <- reactive({
      req(GO_genes_enrichr()$enriched)
      
      df <- GO_genes_enrichr()$enriched
      data <- df[,c(1,9)]
      
      data <- data %>%
        dplyr::rowwise() %>%
        dplyr::mutate(Genes = paste(sort(unlist(strsplit(Genes, ";"))), collapse = ";")) %>%
        dplyr::ungroup()
      
      # Agrupar términos con los mismos genes
      grouped_data <- data %>%
        dplyr::group_by(Genes) %>%
        dplyr::summarise(Terms = paste(Term, collapse = "; "), num_genes = dplyr::n_distinct(Genes)) %>%
        dplyr::arrange(desc(num_genes))
      
      return(list(grouped_data = grouped_data[,c(1,2)]))
    })
    
    
    
    output$GO_enrichr <- DT::renderDataTable({
      df <- GO_genes_enrichr()$enriched
      DT::datatable(df)
    })
    
    output$GO_group1 <- DT::renderDataTable({
      df <- GO_Group1()$grouped_data
      DT::datatable(df)
    })
    
    output$GO_group2 <- DT::renderDataTable({
      df <- GO_Group2()$grouped_data
      DT::datatable(df)
    })
    
    output$GO_plot <- renderPlot({
      GO_genes_enrichr()$plot
    })
    
    
    #download_Result.GO. 
    output$downloadData_GO <- downloadHandler(
      filename = function() {
          paste("Result_GO", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(GO_genes_enrichr()$enriched, file, row.names = FALSE)
      }
    )
    
    #download_Result.group1
    output$downloadGroup1_GO <- downloadHandler(
      filename = function() {
        paste("Result_group1", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(GO_Group1()$grouped_data, file, row.names = FALSE)
      }
    )
    
    #download_Result.group1
    output$downloadGroup2_GO <- downloadHandler(
      filename = function() {
        paste("Result_group2", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(GO_Group2()$grouped_data, file, row.names = FALSE)
      }
    )
 
  })
}
    
## To be copied in the UI
# mod_Gene_Ontology_ui("Gene_Ontology_1")
    
## To be copied in the server
# mod_Gene_Ontology_server("Gene_Ontology_1")
