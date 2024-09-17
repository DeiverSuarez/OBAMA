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
                               selected = 6),
                   textInput(ns("p_valueStr"),label = h5("p-value"),
                             value = "0.001"),
                   
                   downloadButton(ns("downloadData_String"), "Save My String Results"),
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
          tabPanel("Binary Matrix", 
                   DT::DTOutput(ns("matrixTableStr"))
          )
          # tabPanel("Plot", 
          #          plotOutput(ns("GO_plot"))
          # )
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
      
      mi_dataframe_modificado <- genes_enrichr_strig
      mi_dataframe_modificado[] <- lapply(mi_dataframe_modificado, function(x) {
        if (is.list(x)) {
          sapply(x, function(y) paste(y, collapse = ","))
        } else {
          x
        }
      })
      
      genes_enrichr_strig2 <- mi_dataframe_modificado
      genes_enrichr_strig1  <- genes_enrichr_strig2[genes_enrichr_strig2$p_value <as.numeric(input$p_valueStr)  , ]
      
      return(list(genes_enrichr_strig1 = genes_enrichr_strig1))
    })
    
    output$result_string_enrichr <- DT::renderDataTable({
      df <- string_enrichr()$genes_enrichr_strig1
      
      if (is.null(df) || nrow(df) == 0) {
        df <- data.frame(Messege = "Data Unavailable")
      } else {
        if (!is.data.frame(df)) {
          df <- as.data.frame(df)
        }
      }
      
      DT::datatable(df)
    })
    
    #download_Result.String. 
    output$downloadData_String <- downloadHandler(
      filename = function() {
        paste("Result_String", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(data.frame(string_enrichr()$genes_enrichr_strig1), file, row.names = FALSE)
      }
    )
    
    MatrixStr <- reactive({
      df <- as.data.frame(string_enrichr()$genes_enrichr_strig1)
      data <- df[,c(10, 6)]
      colnames(data) <- c("Terms", "Genes")
      genes <- unique(unlist(strsplit(data$Genes, ",")))
      terms <- data$Term
      
      # Inicializar la matriz binaria
      matrix <- matrix(0, nrow = length(genes), ncol = length(terms))
      rownames(matrix) <- genes
      colnames(matrix) <- terms
      
      # Llenar la matriz binaria
      for (i in seq_len(nrow(data))) {
        term <- data$Term[i]
        gene_list <- unlist(strsplit(data$Genes[i], ","))
        matrix[gene_list, term] <- 1
      }
      
      # Convertir la matriz a un data frame para mejor visualización
      matrix_df <- as.data.frame(matrix)
      return(list(matrix_df = matrix_df))
      
    })
    
    
    output$matrixTableStr <- DT::renderDT({
      DT::datatable(MatrixStr()$matrix_df)
    })
    
    
    # output$GO_plot <- renderPlot({
    #   req(filedata()$fileInput)
    #   feature_name <- filedata()$fileInput[,1]
    #   
    #   proteins_mapped <- unlist(strsplit(feature_name, ","))
    #   
    #   graph <- rbioapi::rba_string_network_image(
    #     ids = proteins_mapped,
    #     image_format = "image",
    #     species = 9606,
    #     save_image = TRUE,
    #     required_score = 500,
    #     add_color_nodes = 5,
    #     add_white_nodes = 5,
    #     network_flavor = "actions"
    #   )
    #   
    #   plot(graph)
    # })
    
    
    
 
  })
}
    
## To be copied in the UI
# mod_String_ui("String_1")
    
## To be copied in the server
# mod_String_server("String_1")
