#' MST_Meta_analysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_MST_Meta_analysis_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(width = 4,
                   fluidRow(
                     column(
                       6,
                       style=list("padding-right: 28px;"),
                       radioButtons(
                         ns("Cases_MST"), 
                         "Number of datasets:",
                         choices = list("Two"=2,"Three"=3,"Four"=4,"Five"=5))
                       )
                     ),
                   
                   conditionalPanel(condition = "input.Cases_MST==2", ns=ns,
                   fileInput(ns("MST_expresion21"),accept = c('text/csv','text/comma-separated-values,text/plain','.csv'),
                             label = h5("Gene Expression Data 1")),
                   fileInput(ns("MST_expresion22"),accept = c('text/csv','text/comma-separated-values,text/plain','.csv'),
                             label = h5("Gene Expression Data 2")),
                   fileInput(ns("MST_Genes2"),accept = c('text/csv','text/comma-separated-values,text/plain','.csv'),
                             label = h5("Genes of Interest ")),
                   actionButton(ns("button_MST2"),"Run",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                   hr(),
                   downloadButton(ns("downloadData.MST2"), "Save My MST Results")
                   ),
                   
                   conditionalPanel(condition = "input.Cases_MST==3", ns=ns,
                                    fileInput(ns("MST_expresion31"),accept = c('text/csv','text/comma-separated-values,text/plain','.csv'),
                                              label = h5("Gene Expression Data 1")),
                                    fileInput(ns("MST_expresion32"),accept = c('text/csv','text/comma-separated-values,text/plain','.csv'),
                                              label = h5("Gene Expression Data 2")),
                                    fileInput(ns("MST_expresion33"),accept = c('text/csv','text/comma-separated-values,text/plain','.csv'),
                                              label = h5("Gene Expression Data 3")),
                                    fileInput(ns("MST_Genes3"),accept = c('text/csv','text/comma-separated-values,text/plain','.csv'),
                                              label = h5("Genes of Interest")),
                                    actionButton(ns("button_MST3"),"Run",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                    hr(),
                                    downloadButton(ns("downloadData.MST3"), "Save My MST Data")
                                    
                                    ),
                   conditionalPanel(condition = "input.Cases_MST==4", ns=ns,
                                    fileInput(ns("MST_expresion41"),accept = c('text/csv','text/comma-separated-values,text/plain','.csv'),
                                              label = h5("Gene Expression Data 1")),
                                    fileInput(ns("MST_expresion42"),accept = c('text/csv','text/comma-separated-values,text/plain','.csv'),
                                              label = h5("Gene Expression Data 2")),
                                    fileInput(ns("MST_expresion43"),accept = c('text/csv','text/comma-separated-values,text/plain','.csv'),
                                              label = h5("Gene Expression Data 3")),
                                    fileInput(ns("MST_expresion44"),accept = c('text/csv','text/comma-separated-values,text/plain','.csv'),
                                              label = h5("Gene Expression Data 4")),
                                    fileInput(ns("MST_Genes4"),accept = c('text/csv','text/comma-separated-values,text/plain','.csv'),
                                              label = h5("Genes of Interest")),
                                    actionButton(ns("button_MST4"),"Run",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                    hr(),
                                    downloadButton(ns("downloadData.MST4"), "Save My MST Data")
                                    
                   ),
                   
                   conditionalPanel(condition = "input.Cases_MST==5", ns=ns,
                                    fileInput(ns("MST_expresion51"),accept = c('text/csv','text/comma-separated-values,text/plain','.csv'),
                                              label = h5("Gene Expression Data 1")),
                                    fileInput(ns("MST_expresion52"),accept = c('text/csv','text/comma-separated-values,text/plain','.csv'),
                                              label = h5("Gene Expression Data 2")),
                                    fileInput(ns("MST_expresion53"),accept = c('text/csv','text/comma-separated-values,text/plain','.csv'),
                                              label = h5("Gene Expression Data 3")),
                                    fileInput(ns("MST_expresion54"),accept = c('text/csv','text/comma-separated-values,text/plain','.csv'),
                                              label = h5("Gene Expression Data 4")),
                                    fileInput(ns("MST_expresion55"),accept = c('text/csv','text/comma-separated-values,text/plain','.csv'),
                                              label = h5("Gene Expression Data 5")),
                                    fileInput(ns("MST_Genes5"),accept = c('text/csv','text/comma-separated-values,text/plain','.csv'),
                                              label = h5("Genes of Interest")),
                                    actionButton(ns("button_MST5"),"Run",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                    hr(),
                                    downloadButton(ns("downloadData.MST5"), "Save My MST Data")
                                    
                   ),
                   
                   
                   
                   
      ),
      
      mainPanel(
        conditionalPanel(condition = "input.Cases_MST==2", ns=ns,
        tabsetPanel(type = "tabs",
                    tabPanel("SummaryData",
                             DT::DTOutput(ns("SummaryMST_meta2")),
                             DT::DTOutput(ns("SummaryMST_meta21"))
                    ),
                    tabPanel("MST table",
                             DT::DTOutput(ns("MST_Meta_analysis2"))  
                    ),
                    tabPanel("MST diagram",
                             networkD3::forceNetworkOutput(ns("MST_diagram_Meta_analysis2"),width = "100%", height = "600px")
                             )
                    )
        ),
        conditionalPanel(condition = "input.Cases_MST==3", ns=ns,
                         tabsetPanel(type = "tabs",
                                     tabPanel("SummaryData",
                                              DT::DTOutput(ns("SummaryMST_meta3")),
                                              DT::DTOutput(ns("SummaryMST_meta31")),
                                              DT::DTOutput(ns("SummaryMST_meta32")) 
                                     ),
                                     tabPanel("MST table",
                                              DT::DTOutput(ns("MST_Meta_analysis3"))  
                                     ),
                                     tabPanel("MST diagram",
                                              networkD3::forceNetworkOutput(ns("MST_diagram_Meta_analysis3"),width = "100%", height = "600px")
                                     )
                         )
        ),
        conditionalPanel(condition = "input.Cases_MST==4", ns=ns,
                         tabsetPanel(type = "tabs",
                                     tabPanel("SummaryData",
                                              DT::DTOutput(ns("SummaryMST_meta4")),
                                              DT::DTOutput(ns("SummaryMST_meta41")),
                                              DT::DTOutput(ns("SummaryMST_meta42")),
                                              DT::DTOutput(ns("SummaryMST_meta43")) 
                                     ),
                                     tabPanel("MST table",
                                              DT::DTOutput(ns("MST_Meta_analysis4"))  
                                     ),
                                     tabPanel("MST diagram",
                                              networkD3::forceNetworkOutput(ns("MST_diagram_Meta_analysis4"),width = "100%", height = "600px")
                                     )
                         )
        ),
        conditionalPanel(condition = "input.Cases_MST==5", ns=ns,
                         tabsetPanel(type = "tabs",
                                     tabPanel("SummaryData",
                                              DT::DTOutput(ns("SummaryMST_meta5")),
                                              DT::DTOutput(ns("SummaryMST_meta51")),  
                                              DT::DTOutput(ns("SummaryMST_meta52")),  
                                              DT::DTOutput(ns("SummaryMST_meta53")),  
                                              DT::DTOutput(ns("SummaryMST_meta54")),  
                                     ),
                                     tabPanel("MST table",
                                              DT::DTOutput(ns("MST_Meta_analysis5"))  
                                     ),
                                     tabPanel("MST diagram",
                                              networkD3::forceNetworkOutput(ns("MST_diagram_Meta_analysis5"),width = "100%", height = "600px")
                                     )
                         )
        ),
        
        )
      )
    )
}
    
#' MST_Meta_analysis Server Functions
#'
#' @noRd 
mod_MST_Meta_analysis_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
# 2 data 
    filedata_MST_expression21 <- reactive({
      req(input$MST_expresion21)
      fileInput <- load_file(input$MST_expresion21$name, input$MST_expresion21$datapath)
      fileInput <- as.data.frame(fileInput)
      return(list(fileInput = fileInput))
    })
    
    filedata_MST_expression22 <- reactive({
      req(input$MST_expresion22)
      fileInput <- load_file(input$MST_expresion22$name, input$MST_expresion22$datapath)
      fileInput <- as.data.frame(fileInput)
      return(list(fileInput = fileInput))
    })
    
    filedata_MST_genes2 <- reactive({
      req(input$MST_Genes2)
      fileInput <- load_file(input$MST_Genes2$name, input$MST_Genes2$datapath)
      fileInput <- as.data.frame(fileInput)
      return(list(fileInput = fileInput))
    })
    
    data_info_MST_two_META_21 <- reactive({
      req(filedata_MST_expression21()$fileInput)
      req(filedata_MST_genes2()$fileInput)
      Nrows <- nrow(filedata_MST_expression21()$fileInput)
      Ncols <- ncol(filedata_MST_expression21()$fileInput)-2
      Controls <- table(filedata_MST_expression21()$fileInput[,2])[1]
      disease <- table(filedata_MST_expression21()$fileInput[,2])[2]
      genes <- length(filedata_MST_genes2()$fileInput[,1])
      SummaryData <- data.frame(list(N = c(Nrows, Ncols, Controls, disease, genes)))
      rownames(SummaryData) <- c("Samples", "Genes", "Controls", "Diseases", "Genes of Interest")
      list(SummaryData = SummaryData)
    })
    
    data_info_MST_two_META_22 <- reactive({
      req(filedata_MST_expression22()$fileInput)
      req(filedata_MST_genes2()$fileInput)
      Nrows <- nrow(filedata_MST_expression22()$fileInput)
      Ncols <- ncol(filedata_MST_expression22()$fileInput)-2
      Controls <- table(filedata_MST_expression22()$fileInput[,2])[1]
      disease <- table(filedata_MST_expression22()$fileInput[,2])[2]
      genes <- length(filedata_MST_genes2()$fileInput[,1])
      SummaryData <- data.frame(list(N = c(Nrows, Ncols, Controls, disease, genes)))
      rownames(SummaryData) <- c("Samples", "Genes", "Controls", "Diseases", "Genes of Interest")
      list(SummaryData = SummaryData)
    })
    
    
    
    
    
####### 2 data ###  
    
####### 3 data ###   
    filedata_MST_expression31 <- reactive({
      req(input$MST_expresion31)
      fileInput <- load_file(input$MST_expresion31$name, input$MST_expresion31$datapath)
      fileInput <- as.data.frame(fileInput)
      return(list(fileInput = fileInput))
    })
    
    filedata_MST_expression32 <- reactive({
      req(input$MST_expresion32)
      fileInput <- load_file(input$MST_expresion32$name, input$MST_expresion32$datapath)
      fileInput <- as.data.frame(fileInput)
      return(list(fileInput = fileInput))
    })
    
    filedata_MST_expression33 <- reactive({
      req(input$MST_expresion33)
      fileInput <- load_file(input$MST_expresion33$name, input$MST_expresion33$datapath)
      fileInput <- as.data.frame(fileInput)
      return(list(fileInput = fileInput))
    })
    
    filedata_MST_genes3 <- reactive({
      req(input$MST_Genes3)
      fileInput <- load_file(input$MST_Genes3$name, input$MST_Genes3$datapath)
      fileInput <- as.data.frame(fileInput)
      return(list(fileInput = fileInput))
    })
    
    data_info_MST_two_META_3 <- reactive({
      req(filedata_MST_expression31()$fileInput)
      req(filedata_MST_genes3()$fileInput)
      Nrows <- nrow(filedata_MST_expression31()$fileInput)
      Ncols <- ncol(filedata_MST_expression31()$fileInput)-2
      Controls <- table(filedata_MST_expression31()$fileInput[,2])[1]
      disease <- table(filedata_MST_expression31()$fileInput[,2])[2]
      genes <- length(filedata_MST_genes3()$fileInput[,1])
      SummaryData <- data.frame(list(N = c(Nrows, Ncols, Controls, disease, genes)))
      rownames(SummaryData) <- c("Samples", "Genes", "Controls", "Diseases", "Genes of Interest")
      list(SummaryData = SummaryData)
    })
    
    data_info_MST_two_META_31 <- reactive({
      req(filedata_MST_expression32()$fileInput)
      req(filedata_MST_genes3()$fileInput)
      Nrows <- nrow(filedata_MST_expression32()$fileInput)
      Ncols <- ncol(filedata_MST_expression32()$fileInput)-2
      Controls <- table(filedata_MST_expression32()$fileInput[,2])[1]
      disease <- table(filedata_MST_expression32()$fileInput[,2])[2]
      genes <- length(filedata_MST_genes3()$fileInput[,1])
      SummaryData <- data.frame(list(N = c(Nrows, Ncols, Controls, disease, genes)))
      rownames(SummaryData) <- c("Samples", "Genes", "Controls", "Diseases", "Genes of Interest")
      list(SummaryData = SummaryData)
    })
    
    data_info_MST_two_META_32 <- reactive({
      req(filedata_MST_expression33()$fileInput)
      req(filedata_MST_genes3()$fileInput)
      Nrows <- nrow(filedata_MST_expression33()$fileInput)
      Ncols <- ncol(filedata_MST_expression33()$fileInput)-2
      Controls <- table(filedata_MST_expression33()$fileInput[,2])[1]
      disease <- table(filedata_MST_expression33()$fileInput[,2])[2]
      genes <- length(filedata_MST_genes3()$fileInput[,1])
      SummaryData <- data.frame(list(N = c(Nrows, Ncols, Controls, disease, genes)))
      rownames(SummaryData) <- c("Samples", "Genes", "Controls", "Diseases", "Genes of Interest")
      list(SummaryData = SummaryData)
    })
######  
    ####### 4 data ###   
    filedata_MST_expression41 <- reactive({
      req(input$MST_expresion41)
      fileInput <- load_file(input$MST_expresion41$name, input$MST_expresion41$datapath)
      fileInput <- as.data.frame(fileInput)
      return(list(fileInput = fileInput))
    })
    
    filedata_MST_expression42 <- reactive({
      req(input$MST_expresion42)
      fileInput <- load_file(input$MST_expresion42$name, input$MST_expresion42$datapath)
      fileInput <- as.data.frame(fileInput)
      return(list(fileInput = fileInput))
    })
    
    filedata_MST_expression43 <- reactive({
      req(input$MST_expresion43)
      fileInput <- load_file(input$MST_expresion43$name, input$MST_expresion43$datapath)
      fileInput <- as.data.frame(fileInput)
      return(list(fileInput = fileInput))
    })
    
    filedata_MST_expression44 <- reactive({
      req(input$MST_expresion44)
      fileInput <- load_file(input$MST_expresion44$name, input$MST_expresion44$datapath)
      fileInput <- as.data.frame(fileInput)
      return(list(fileInput = fileInput))
    })
    
    filedata_MST_genes4 <- reactive({
      req(input$MST_Genes4)
      fileInput <- load_file(input$MST_Genes4$name, input$MST_Genes4$datapath)
      fileInput <- as.data.frame(fileInput)
      return(list(fileInput = fileInput))
    })
    
    data_info_MST_two_META_4 <- reactive({
      req(filedata_MST_expression41()$fileInput)
      req(filedata_MST_genes4()$fileInput)
      Nrows <- nrow(filedata_MST_expression41()$fileInput)
      Ncols <- ncol(filedata_MST_expression41()$fileInput)-2
      Controls <- table(filedata_MST_expression41()$fileInput[,2])[1]
      disease <- table(filedata_MST_expression41()$fileInput[,2])[2]
      genes <- length(filedata_MST_genes4()$fileInput[,1])
      SummaryData <- data.frame(list(N = c(Nrows, Ncols, Controls, disease, genes)))
      rownames(SummaryData) <- c("Samples", "Genes", "Controls", "Diseases", "Genes of Interest")
      list(SummaryData = SummaryData)
    })
    
    data_info_MST_two_META_41 <- reactive({
      req(filedata_MST_expression42()$fileInput)
      req(filedata_MST_genes4()$fileInput)
      Nrows <- nrow(filedata_MST_expression42()$fileInput)
      Ncols <- ncol(filedata_MST_expression42()$fileInput)-2
      Controls <- table(filedata_MST_expression42()$fileInput[,2])[1]
      disease <- table(filedata_MST_expression42()$fileInput[,2])[2]
      genes <- length(filedata_MST_genes4()$fileInput[,1])
      SummaryData <- data.frame(list(N = c(Nrows, Ncols, Controls, disease, genes)))
      rownames(SummaryData) <- c("Samples", "Genes", "Controls", "Diseases", "Genes of Interest")
      list(SummaryData = SummaryData)
    })
    
    data_info_MST_two_META_42 <- reactive({
      req(filedata_MST_expression43()$fileInput)
      req(filedata_MST_genes4()$fileInput)
      Nrows <- nrow(filedata_MST_expression43()$fileInput)
      Ncols <- ncol(filedata_MST_expression43()$fileInput)-2
      Controls <- table(filedata_MST_expression43()$fileInput[,2])[1]
      disease <- table(filedata_MST_expression43()$fileInput[,2])[2]
      genes <- length(filedata_MST_genes4()$fileInput[,1])
      SummaryData <- data.frame(list(N = c(Nrows, Ncols, Controls, disease, genes)))
      rownames(SummaryData) <- c("Samples", "Genes", "Controls", "Diseases", "Genes of Interest")
      list(SummaryData = SummaryData)
    })
    
    data_info_MST_two_META_43 <- reactive({
      req(filedata_MST_expression44()$fileInput)
      req(filedata_MST_genes4()$fileInput)
      Nrows <- nrow(filedata_MST_expression44()$fileInput)
      Ncols <- ncol(filedata_MST_expression44()$fileInput)-2
      Controls <- table(filedata_MST_expression44()$fileInput[,2])[1]
      disease <- table(filedata_MST_expression44()$fileInput[,2])[2]
      genes <- length(filedata_MST_genes4()$fileInput[,1])
      SummaryData <- data.frame(list(N = c(Nrows, Ncols, Controls, disease, genes)))
      rownames(SummaryData) <- c("Samples", "Genes", "Controls", "Diseases", "Genes of Interest")
      list(SummaryData = SummaryData)
    })
    
    ###### 
    
    ####### 5 data ###   
    filedata_MST_expression51 <- reactive({
      req(input$MST_expresion51)
      fileInput <- load_file(input$MST_expresion51$name, input$MST_expresion51$datapath)
      fileInput <- as.data.frame(fileInput)
      return(list(fileInput = fileInput))
    })
    
    filedata_MST_expression52 <- reactive({
      req(input$MST_expresion52)
      fileInput <- load_file(input$MST_expresion52$name, input$MST_expresion52$datapath)
      fileInput <- as.data.frame(fileInput)
      return(list(fileInput = fileInput))
    })
    
    filedata_MST_expression53 <- reactive({
      req(input$MST_expresion53)
      fileInput <- load_file(input$MST_expresion53$name, input$MST_expresion53$datapath)
      fileInput <- as.data.frame(fileInput)
      return(list(fileInput = fileInput))
    })
    
    filedata_MST_expression54 <- reactive({
      req(input$MST_expresion54)
      fileInput <- load_file(input$MST_expresion54$name, input$MST_expresion54$datapath)
      fileInput <- as.data.frame(fileInput)
      return(list(fileInput = fileInput))
    })
    
    filedata_MST_expression55 <- reactive({
      req(input$MST_expresion55)
      fileInput <- load_file(input$MST_expresion55$name, input$MST_expresion55$datapath)
      fileInput <- as.data.frame(fileInput)
      return(list(fileInput = fileInput))
    })
    
    filedata_MST_genes5 <- reactive({
      req(input$MST_Genes5)
      fileInput <- load_file(input$MST_Genes5$name, input$MST_Genes5$datapath)
      fileInput <- as.data.frame(fileInput)
      return(list(fileInput = fileInput))
    })
    
    data_info_MST_two_META_5 <- reactive({
      req(filedata_MST_expression51()$fileInput)
      req(filedata_MST_genes5()$fileInput)
      Nrows <- nrow(filedata_MST_expression51()$fileInput)
      Ncols <- ncol(filedata_MST_expression51()$fileInput)-2
      Controls <- table(filedata_MST_expression51()$fileInput[,2])[1]
      disease <- table(filedata_MST_expression51()$fileInput[,2])[2]
      genes <- length(filedata_MST_genes5()$fileInput[,1])
      SummaryData <- data.frame(list(N = c(Nrows, Ncols, Controls, disease, genes)))
      rownames(SummaryData) <- c("Samples", "Genes", "Controls", "Diseases", "Genes of Interest")
      list(SummaryData = SummaryData)
    })
    
    data_info_MST_two_META_51 <- reactive({
      req(filedata_MST_expression52()$fileInput)
      req(filedata_MST_genes5()$fileInput)
      Nrows <- nrow(filedata_MST_expression52()$fileInput)
      Ncols <- ncol(filedata_MST_expression52()$fileInput)-2
      Controls <- table(filedata_MST_expression52()$fileInput[,2])[1]
      disease <- table(filedata_MST_expression52()$fileInput[,2])[2]
      genes <- length(filedata_MST_genes5()$fileInput[,1])
      SummaryData <- data.frame(list(N = c(Nrows, Ncols, Controls, disease, genes)))
      rownames(SummaryData) <- c("Samples", "Genes", "Controls", "Diseases", "Genes of Interest")
      list(SummaryData = SummaryData)
    })
    
    data_info_MST_two_META_52 <- reactive({
      req(filedata_MST_expression53()$fileInput)
      req(filedata_MST_genes5()$fileInput)
      Nrows <- nrow(filedata_MST_expression53()$fileInput)
      Ncols <- ncol(filedata_MST_expression53()$fileInput)-2
      Controls <- table(filedata_MST_expression53()$fileInput[,2])[1]
      disease <- table(filedata_MST_expression53()$fileInput[,2])[2]
      genes <- length(filedata_MST_genes5()$fileInput[,1])
      SummaryData <- data.frame(list(N = c(Nrows, Ncols, Controls, disease, genes)))
      rownames(SummaryData) <- c("Samples", "Genes", "Controls", "Diseases", "Genes of Interest")
      list(SummaryData = SummaryData)
    })
    
    data_info_MST_two_META_53 <- reactive({
      req(filedata_MST_expression54()$fileInput)
      req(filedata_MST_genes5()$fileInput)
      Nrows <- nrow(filedata_MST_expression54()$fileInput)
      Ncols <- ncol(filedata_MST_expression54()$fileInput)-2
      Controls <- table(filedata_MST_expression54()$fileInput[,2])[1]
      disease <- table(filedata_MST_expression54()$fileInput[,2])[2]
      genes <- length(filedata_MST_genes5()$fileInput[,1])
      SummaryData <- data.frame(list(N = c(Nrows, Ncols, Controls, disease, genes)))
      rownames(SummaryData) <- c("Samples", "Genes", "Controls", "Diseases", "Genes of Interest")
      list(SummaryData = SummaryData)
    })
    
    data_info_MST_two_META_54 <- reactive({
      req(filedata_MST_expression55()$fileInput)
      req(filedata_MST_genes5()$fileInput)
      Nrows <- nrow(filedata_MST_expression55()$fileInput)
      Ncols <- ncol(filedata_MST_expression55()$fileInput)-2
      Controls <- table(filedata_MST_expression55()$fileInput[,2])[1]
      disease <- table(filedata_MST_expression55()$fileInput[,2])[2]
      genes <- length(filedata_MST_genes5()$fileInput[,1])
      SummaryData <- data.frame(list(N = c(Nrows, Ncols, Controls, disease, genes)))
      rownames(SummaryData) <- c("Samples", "Genes", "Controls", "Diseases", "Genes of Interest")
      list(SummaryData = SummaryData)
    })
    ###### 
    
    MST2 <- eventReactive(input$button_MST2,{
      wnv21 <- as.data.frame(filedata_MST_expression21()$fileInput)
      wnv22 <- as.data.frame(filedata_MST_expression22()$fileInput)
      wnv23 <- as.data.frame(filedata_MST_genes2()$fileInput)
      outEval <- MST_tow_disease(data_expresion1 = wnv21, data_expresion2 = wnv22, data_gene = wnv23)
      return(list(outEval=outEval))
    })
    
    output$MST_Meta_analysis2 <- DT::renderDataTable({
      df <- as.data.frame(MST2())
      df[,3] = 2 - as.numeric(as.character(df[,3]))
      names(df) <- c("Gene1", "Gene2", "Weight")
      DT::datatable(df)
    })
    
    output$SummaryMST_meta2 <- DT::renderDataTable({
      df <- data_info_MST_two_META_21()$SummaryData
      DT::datatable(df)
    })
    
    output$SummaryMST_meta21 <- DT::renderDataTable({
      df <- data_info_MST_two_META_22()$SummaryData
      DT::datatable(df)
    })
    
    #download_Result.MST
    output$downloadData.MST2 <- downloadHandler(
      filename = function() {
        paste("Result_MST", ".csv", sep = "")
        
      },
      content = function(file) {
        write.csv(MST2(), file, row.names = FALSE)
      }
    )
    
    MST_digram2 <- eventReactive(input$button_MST2,{
      Data=as.data.frame(MST2())
      g <- igraph::graph.data.frame(Data, directed = FALSE)
      igraph::tkplot(g )
      diagram <- networkD3::simpleNetwork(Data=Data, linkDistance = 30, charge = -30, fontSize = 11,linkColour = "red",nodeColour ="blue"  )
      return(diagram)
    })
    
    output$MST_diagram_Meta_analysis2 <- networkD3::renderForceNetwork({
      MST_digram2()
    })
    
  ##########  
    MST3 <- eventReactive(input$button_MST3,{
      wnv31 <- as.data.frame(filedata_MST_expression31()$fileInput)
      wnv32 <- as.data.frame(filedata_MST_expression32()$fileInput)
      wnv33 <- as.data.frame(filedata_MST_expression33()$fileInput)
      wnv34 <- as.data.frame(filedata_MST_genes3()$fileInput)
       
      outEval <- MST_three_disease(data_expresion1 = wnv31, data_expresion2 = wnv32, 
                                   data_expresion3 = wnv33, data_gene = wnv34)
      return(list(outEval=outEval))
    })
    
    output$MST_Meta_analysis3 <- DT::renderDataTable({
      df <- as.data.frame(MST3())
      df[,3] = 3 - as.numeric(as.character(df[,3]))
      names(df) <- c("Gene1", "Gene2", "Weight")
      DT::datatable(df)
    })
    
    #download_Result.MST
    output$downloadData.MST3 <- downloadHandler(
      filename = function() {
        paste("Result_MST", ".csv", sep = "")
        
      },
      content = function(file) {
        write.csv(MST3(), file, row.names = FALSE)
      }
    )
    
    MST_digram3 <- eventReactive(input$button_MST3,{
      Data=as.data.frame(MST3())
      g <- igraph::graph.data.frame(Data, directed = FALSE)
      igraph::tkplot(g )
      diagram <- networkD3::simpleNetwork(Data=Data, linkDistance = 30, charge = -30, fontSize = 11,linkColour = "red",nodeColour ="blue"  )
      return(diagram)
    })
    
    output$MST_diagram_Meta_analysis3 <- networkD3::renderForceNetwork({
      MST_digram3()
    })
    
    output$SummaryMST_meta3 <- DT::renderDataTable({
      df <- data_info_MST_two_META_3()$SummaryData
      DT::datatable(df)
    })
    
    output$SummaryMST_meta31 <- DT::renderDataTable({
      df <- data_info_MST_two_META_31()$SummaryData
      DT::datatable(df)
    })
    
    output$SummaryMST_meta32 <- DT::renderDataTable({
      df <- data_info_MST_two_META_32()$SummaryData
      DT::datatable(df)
    })
    ##########
    
    MST4 <- eventReactive(input$button_MST4,{
      wnv41 <- as.data.frame(filedata_MST_expression41()$fileInput)
      wnv42 <- as.data.frame(filedata_MST_expression42()$fileInput)
      wnv43 <- as.data.frame(filedata_MST_expression43()$fileInput)
      wnv44 <- as.data.frame(filedata_MST_expression44()$fileInput)
      wnv45 <- as.data.frame(filedata_MST_genes4()$fileInput)
      
      outEval <- MST_four_disease(data_expresion1 = wnv41, data_expresion2 = wnv42, 
                                   data_expresion3 = wnv43,data_expresion4 = wnv44,
                                   data_gene = wnv45)
      return(list(outEval=outEval))
    })
    
    output$MST_Meta_analysis4 <- DT::renderDataTable({
      df <- as.data.frame(MST4())
      df[,3] = 4 - as.numeric(as.character(df[,3]))
      names(df) <- c("Gene1", "Gene2", "Weight")
      DT::datatable(df)
    })
    
    #download_Result.MST
    output$downloadData.MST4 <- downloadHandler(
      filename = function() {
        paste("Result_MST", ".csv", sep = "")
        
      },
      content = function(file) {
        write.csv(MST4(), file, row.names = FALSE)
      }
      )
    
    MST_digram4 <- eventReactive(input$button_MST4,{
      Data=as.data.frame(MST4())
      g <- igraph::graph.data.frame(Data, directed = FALSE)
      igraph::tkplot(g )
      diagram <- networkD3::simpleNetwork(Data=Data, linkDistance = 30, charge = -30, fontSize = 11,linkColour = "red",nodeColour ="blue"  )
      return(diagram)
    })
    
    output$MST_diagram_Meta_analysis4 <- networkD3::renderForceNetwork({
      MST_digram4()
    })
    
    output$SummaryMST_meta4 <- DT::renderDataTable({
      df <- data_info_MST_two_META_4()$SummaryData
      DT::datatable(df)
    })
    
    output$SummaryMST_meta41 <- DT::renderDataTable({
      df <- data_info_MST_two_META_41()$SummaryData
      DT::datatable(df)
    })
    
    output$SummaryMST_meta42 <- DT::renderDataTable({
      df <- data_info_MST_two_META_42()$SummaryData
      DT::datatable(df)
    })
    
    output$SummaryMST_meta43 <- DT::renderDataTable({
      df <- data_info_MST_two_META_43()$SummaryData
      DT::datatable(df)
    })
    
    ##########
    
    MST5 <- eventReactive(input$button_MST5,{
      wnv51 <- as.data.frame(filedata_MST_expression51()$fileInput)
      wnv52 <- as.data.frame(filedata_MST_expression52()$fileInput)
      wnv53 <- as.data.frame(filedata_MST_expression53()$fileInput)
      wnv54 <- as.data.frame(filedata_MST_expression54()$fileInput)
      wnv55 <- as.data.frame(filedata_MST_expression55()$fileInput)
      wnv56 <- as.data.frame(filedata_MST_genes5()$fileInput)
      
      outEval <- MST_five_disease(data_expresion1 = wnv51, data_expresion2 = wnv52, 
                                  data_expresion3 = wnv53,data_expresion4 = wnv54,data_expresion5 = wnv55,
                                  data_gene = wnv56)
      return(list(outEval=outEval))
    })
    
    output$MST_Meta_analysis5 <- DT::renderDataTable({
      df <- as.data.frame(MST5())
      df[,3] = 5 - as.numeric(as.character(df[,3]))
      names(df) <- c("Gene1", "Gene2", "Weight")
      DT::datatable(df)
    })
    
    #download_Result.MST
    output$downloadData.MST5 <- downloadHandler(
      filename = function() {
        paste("Result_MST", ".csv", sep = "")
        
      },
      content = function(file) {
        write.csv(MST5(), file, row.names = FALSE)
      }
    )
    
    MST_digram5 <- eventReactive(input$button_MST5,{
      Data=as.data.frame(MST5())
      g <- igraph::graph.data.frame(Data, directed = FALSE)
      igraph::tkplot(g )
      diagram <- networkD3::simpleNetwork(Data=Data, linkDistance = 30, charge = -30, fontSize = 11,linkColour = "red",nodeColour ="blue"  )
      return(diagram)
    })
    
    output$MST_diagram_Meta_analysis5 <- networkD3::renderForceNetwork({
      MST_digram5()
    })
    
    output$SummaryMST_meta5 <- DT::renderDataTable({
      df <- data_info_MST_two_META_5()$SummaryData
      DT::datatable(df)
    })
    
    output$SummaryMST_meta51 <- DT::renderDataTable({
      df <- data_info_MST_two_META_51()$SummaryData
      DT::datatable(df)
    })
    
    output$SummaryMST_meta52 <- DT::renderDataTable({
      df <- data_info_MST_two_META_52()$SummaryData
      DT::datatable(df)
    })
    
    output$SummaryMST_meta53 <- DT::renderDataTable({
      df <- data_info_MST_two_META_53()$SummaryData
      DT::datatable(df)
    })
    
    output$SummaryMST_meta54 <- DT::renderDataTable({
      df <- data_info_MST_two_META_54()$SummaryData
      DT::datatable(df)
    })
    
    
  })
}
    
## To be copied in the UI
# mod_MST_Meta_analysis_ui("MST_Meta_analysis_ui_1")
    
## To be copied in the server
# mod_MST_Meta_analysis_server("MST_Meta_analysis_ui_1")
