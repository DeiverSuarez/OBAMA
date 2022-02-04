#' Meta_analysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Meta_analysis_ui <- function(id){
  options(shiny.maxRequestSize=30*1024^8)
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(width = 4, 
      fluidRow(column(6,style=list("padding-right: 28px;"),
                      radioButtons(ns("Cases"), "Number of datasets:", choices = list("Two"=2,"Three"=3,"Four"=4,"Five"=5))),
      
      column(6,style=list("padding-right: 28px;"),
                       selectInput(ns("PMs111"), label = "PM",
                                   choices = list("Median"=2, "Mean"=1, "Quantile"=3))),
      
      column(6,style=list("padding-left: 28px;"),
                      conditionalPanel(condition ="input.PMs111==3",ns=ns,
                                       column(6,style=list("padding-right: 28px;"),
                                              textInput(ns("ValueqPM11"),label = h5("Quantile value (q)"),value = 75)))),
      fluidRow(column(4,textInput(ns("NFro_11"),label = h5("Number of frontiers"),value = "3")))
      ),
      
      conditionalPanel(condition = "input.Cases==2", ns=ns,
                       ######enfermedad2
                       fileInput(ns("fileBcsv112"),accept = c('text/csv','text/comma-separated-values,text/plain','.csv'),label = h5("Gene Expression Data 1")),
                       ######enfermedad2
                       fileInput(ns("fileBcsv122"),accept = c('text/csv','text/comma-separated-values,text/plain','.csv'),label = h5("Gene Expression Data 2")),
                       
                       actionButton(ns("button112"),"Run",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                       downloadButton(ns("downloadData.2metanalisis"), "Save My Result")
      ),
      conditionalPanel(condition = "input.Cases==3", ns=ns,
                       fileInput(ns("fileBcsv113"),accept = c('text/csv','text/comma-separated-values,text/plain','.csv'),label = h5("Gene Expression Data 1")),
                       ######enfermedad2
                       fileInput(ns("fileBcsv123"),accept = c('text/csv','text/comma-separated-values,text/plain','.csv'),label = h5("Gene Expression Data 2")),
                       ######enfermedad3
                       fileInput(ns("fileBcsv133"),accept = c('text/csv','text/comma-separated-values,text/plain','.csv'),label = h5("Gene Expression Data 3")),
                       
                       actionButton(ns("button113"),"Run",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                       downloadButton(ns("downloadData.3metanalisis"), "Save My Result")
      ),
      conditionalPanel(condition = "input.Cases==4", ns=ns,
                       fileInput(ns("fileBcsv114"),accept = c('text/csv','text/comma-separated-values,text/plain','.csv'),label = h5("Gene Expression Data 1")),
                       ######enfermedad2
                       fileInput(ns("fileBcsv124"),accept = c('text/csv','text/comma-separated-values,text/plain','.csv'),label = h5("Gene Expression Data 2")),
                       ######enfermedad3
                       fileInput(ns("fileBcsv134"),accept = c('text/csv','text/comma-separated-values,text/plain','.csv'),label = h5("Gene Expression Data 3")),
                       ######enfermedad4
                       fileInput(ns("fileBcsv144"),accept = c('text/csv','text/comma-separated-values,text/plain','.csv'),label = h5("Gene Expression Data 4")),
                       
                       actionButton(ns("button114"),"Run",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                       downloadButton(ns("downloadData.4metanalisis"), "Save My Result")
      ),
      
      conditionalPanel(condition = "input.Cases==5", ns=ns,
                       fileInput(ns("fileBcsv115"),accept = c('text/csv','text/comma-separated-values,text/plain','.csv'),label = h5("Gene Expression Data 1")),
                       ######enfermedad2
                       fileInput(ns("fileBcsv125"),accept = c('text/csv','text/comma-separated-values,text/plain','.csv'),label = h5("Gene Expression Data 2")),
                       ######enfermedad3
                       fileInput(ns("fileBcsv135"),accept = c('text/csv','text/comma-separated-values,text/plain','.csv'),label = h5("Gene Expression Data 3")),
                       ######enfermedad4
                       fileInput(ns("fileBcsv145"),accept = c('text/csv','text/comma-separated-values,text/plain','.csv'),label = h5("Gene Expression Data 4")),
                       ######enfermedad5
                       fileInput(ns("fileBcsv155"),accept = c('text/csv','text/comma-separated-values,text/plain','.csv'),label = h5("Gene Expression Data 5")),
                       
                       actionButton(ns("button115"),"Run",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                       downloadButton(ns("downloadData.5metanalisis"), "Save My Result")
      ),
      
      
      
      ),
      
      
                       mainPanel(
                         conditionalPanel(condition = "input.Cases== 2 || input.Cases== 3", ns=ns,
                         tabsetPanel(
                           tabPanel("SummaryData",
                                    conditionalPanel(condition = " input.Cases== 2 ", ns=ns,
                                                     DT::DTOutput(ns("infotablem11")),
                                                     DT::DTOutput(ns("infotablem12"))
                                    
                                    ),
                                    conditionalPanel(condition = " input.Cases== 3 ", ns=ns,
                                                     DT::DTOutput(ns("infotablem31")),
                                                     DT::DTOutput(ns("infotablem32")),
                                                     DT::DTOutput(ns("infotablem33"))
                                                     
                                    )
                                    ),
                           tabPanel("Frontiers",
                                    conditionalPanel(condition = "input.Cases== 2", ns=ns,
                                                     DT::dataTableOutput(ns("mtable12"))),
                                    conditionalPanel(condition = "input.Cases== 3", ns=ns,
                                                     DT::dataTableOutput(ns("mtable13")))),
                           
                           tabPanel("Plot",
                                    conditionalPanel(condition = "input.Cases== 2", ns=ns,
                                                     plotOutput(ns("mplot12"))),
                                    
                                    conditionalPanel(condition = "input.Cases== 3", ns=ns,
                                                     plotly::plotlyOutput(ns("mplot13")))
                            )
                           )
                         ),
                         conditionalPanel(condition = "input.Cases== 4 || input.Cases== 5", ns=ns,
                                          tabsetPanel(
                                              tabPanel("SummaryData",
                                                       conditionalPanel(condition = " input.Cases== 4 ", ns=ns,
                                                                        DT::DTOutput(ns("infotablem41")),
                                                                        DT::DTOutput(ns("infotablem42")),
                                                                        DT::DTOutput(ns("infotablem43")),
                                                                        DT::DTOutput(ns("infotablem44"))
                                                                        ),
                                                       conditionalPanel(condition = " input.Cases== 5 ", ns=ns,
                                                                        DT::DTOutput(ns("infotablem51")),
                                                                        DT::DTOutput(ns("infotablem52")),
                                                                        DT::DTOutput(ns("infotablem53")),
                                                                        DT::DTOutput(ns("infotablem54")),
                                                                        DT::DTOutput(ns("infotablem55"))
                                                       )
                                              ),
                                              tabPanel("Frontiers",
                                                       conditionalPanel(condition = "input.Cases== 4", ns=ns,
                                                                        DT::dataTableOutput(ns("mtable14"))),
                                                       conditionalPanel(condition = "input.Cases== 5", ns=ns,
                                                                        DT::dataTableOutput(ns("mtable15"))))
                                             )
                                            
                                          )
                         )
      )
    
  )
 
}
    
#' Meta_analysis Server Functions
#'
#' @noRd 
mod_Meta_analysis_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    ####metanalisis 2 datas 
    
    filedata_m1 <- reactive({
      req(input$fileBcsv112)
      fileInput <- load_file(input$fileBcsv112$name, input$fileBcsv112$datapath)
      fileInput <- as.data.frame(fileInput)
      return(list(fileInput = fileInput))
    })
    
    data_info_m1 <- reactive({
      req(filedata_m1()$fileInput)
      Nrows <- nrow(filedata_m1()$fileInput)
      Ncols <- ncol(filedata_m1()$fileInput)
      SummaryData <- data.frame(list(N = c(Nrows, Ncols)))
      rownames(SummaryData) <- c("Nrows", "Ncols")
      list(SummaryData = SummaryData)
    })
    
    output$infotablem11 <- DT::renderDataTable({
      df <- data_info_m1()$SummaryData
      DT::datatable(df)
    })
    
    #######
    
    filedata_m11 <- reactive({
      req(input$fileBcsv122)
      fileInput <- load_file(input$fileBcsv122$name, input$fileBcsv122$datapath)
      fileInput <- as.data.frame(fileInput)
      return(list(fileInput = fileInput))
    })
    
    data_info_m11 <- reactive({
      req(filedata_m11()$fileInput)
      Nrows <- nrow(filedata_m11()$fileInput)
      Ncols <- ncol(filedata_m11()$fileInput)
      SummaryData <- data.frame(list(N = c(Nrows, Ncols)))
      rownames(SummaryData) <- c("Nrows", "Ncols")
      list(SummaryData = SummaryData)
    })
    
    output$infotablem12 <- DT::renderDataTable({
      df <- data_info_m11()$SummaryData
      DT::datatable(df)
    })
    #####
    ####metanalisis 3 datas
    filedata_m3 <- reactive({
      req(input$fileBcsv113)
      fileInput <- load_file(input$fileBcsv113$name, input$fileBcsv113$datapath)
      fileInput <- as.data.frame(fileInput)
      return(list(fileInput = fileInput))
    })
    
    data_info_m3 <- reactive({
      req(filedata_m3()$fileInput)
      Nrows <- nrow(filedata_m3()$fileInput)
      Ncols <- ncol(filedata_m3()$fileInput)
      SummaryData <- data.frame(list(N = c(Nrows, Ncols)))
      rownames(SummaryData) <- c("Nrows", "Ncols")
      list(SummaryData = SummaryData)
    })
    
    output$infotablem31 <- DT::renderDataTable({
      df <- data_info_m3()$SummaryData
      DT::datatable(df)
    })
    #####
    
    filedata_m32 <- reactive({
      req(input$fileBcsv123)
      fileInput <- load_file(input$fileBcsv123$name, input$fileBcsv123$datapath)
      fileInput <- as.data.frame(fileInput)
      return(list(fileInput = fileInput))
    })
    
    data_info_m32 <- reactive({
      req(filedata_m32()$fileInput)
      Nrows <- nrow(filedata_m32()$fileInput)
      Ncols <- ncol(filedata_m32()$fileInput)
      SummaryData <- data.frame(list(N = c(Nrows, Ncols)))
      rownames(SummaryData) <- c("Nrows", "Ncols")
      list(SummaryData = SummaryData)
    })
    
    output$infotablem32 <- DT::renderDataTable({
      df <- data_info_m32()$SummaryData
      DT::datatable(df)
    })
    #####
    
    filedata_m33 <- reactive({
      req(input$fileBcsv133)
      fileInput <- load_file(input$fileBcsv133$name, input$fileBcsv133$datapath)
      fileInput <- as.data.frame(fileInput)
      return(list(fileInput = fileInput))
    })
    
    data_info_m33 <- reactive({
      req(filedata_m33()$fileInput)
      Nrows <- nrow(filedata_m33()$fileInput)
      Ncols <- ncol(filedata_m33()$fileInput)
      SummaryData <- data.frame(list(N = c(Nrows, Ncols)))
      rownames(SummaryData) <- c("Nrows", "Ncols")
      list(SummaryData = SummaryData)
    })
    
    output$infotablem33 <- DT::renderDataTable({
      df <- data_info_m33()$SummaryData
      DT::datatable(df)
    })
    #####
    ####metanalisis 4 datas
    
    filedata_m41 <- reactive({
      req(input$fileBcsv114)
      fileInput <- load_file(input$fileBcsv114$name, input$fileBcsv114$datapath)
      fileInput <- as.data.frame(fileInput)
      return(list(fileInput = fileInput))
    })
    
    data_info_m41 <- reactive({
      req(filedata_m41()$fileInput)
      Nrows <- nrow(filedata_m41()$fileInput)
      Ncols <- ncol(filedata_m41()$fileInput)
      SummaryData <- data.frame(list(N = c(Nrows, Ncols)))
      rownames(SummaryData) <- c("Nrows", "Ncols")
      list(SummaryData = SummaryData)
    })
    
    output$infotablem41 <- DT::renderDataTable({
      df <- data_info_m41()$SummaryData
      DT::datatable(df)
    })
    #####
    
    filedata_m42 <- reactive({
      req(input$fileBcsv124)
      fileInput <- load_file(input$fileBcsv124$name, input$fileBcsv124$datapath)
      fileInput <- as.data.frame(fileInput)
      return(list(fileInput = fileInput))
    })
    
    data_info_m42 <- reactive({
      req(filedata_m42()$fileInput)
      Nrows <- nrow(filedata_m42()$fileInput)
      Ncols <- ncol(filedata_m42()$fileInput)
      SummaryData <- data.frame(list(N = c(Nrows, Ncols)))
      rownames(SummaryData) <- c("Nrows", "Ncols")
      list(SummaryData = SummaryData)
    })
    
    output$infotablem42 <- DT::renderDataTable({
      df <- data_info_m42()$SummaryData
      DT::datatable(df)
    })
    #####
    
    filedata_m43 <- reactive({
      req(input$fileBcsv134)
      fileInput <- load_file(input$fileBcsv134$name, input$fileBcsv134$datapath)
      fileInput <- as.data.frame(fileInput)
      return(list(fileInput = fileInput))
    })
    
    data_info_m43 <- reactive({
      req(filedata_m43()$fileInput)
      Nrows <- nrow(filedata_m43()$fileInput)
      Ncols <- ncol(filedata_m43()$fileInput)
      SummaryData <- data.frame(list(N = c(Nrows, Ncols)))
      rownames(SummaryData) <- c("Nrows", "Ncols")
      list(SummaryData = SummaryData)
    })
    
    output$infotablem43 <- DT::renderDataTable({
      df <- data_info_m43()$SummaryData
      DT::datatable(df)
    })
    #####
    
    filedata_m44 <- reactive({
      req(input$fileBcsv144)
      fileInput <- load_file(input$fileBcsv144$name, input$fileBcsv144$datapath)
      fileInput <- as.data.frame(fileInput)
      return(list(fileInput = fileInput))
    })
    
    data_info_m44 <- reactive({
      req(filedata_m44()$fileInput)
      Nrows <- nrow(filedata_m44()$fileInput)
      Ncols <- ncol(filedata_m44()$fileInput)
      SummaryData <- data.frame(list(N = c(Nrows, Ncols)))
      rownames(SummaryData) <- c("Nrows", "Ncols")
      list(SummaryData = SummaryData)
    })
    
    output$infotablem44 <- DT::renderDataTable({
      df <- data_info_m44()$SummaryData
      DT::datatable(df)
    })
    #####
    #####
    ####metanalisis 5 datas
    
    filedata_m51 <- reactive({
      req(input$fileBcsv115)
      fileInput <- load_file(input$fileBcsv115$name, input$fileBcsv115$datapath)
      fileInput <- as.data.frame(fileInput)
      return(list(fileInput = fileInput))
    })
    
    data_info_m51 <- reactive({
      req(filedata_m51()$fileInput)
      Nrows <- nrow(filedata_m51()$fileInput)
      Ncols <- ncol(filedata_m51()$fileInput)
      SummaryData <- data.frame(list(N = c(Nrows, Ncols)))
      rownames(SummaryData) <- c("Nrows", "Ncols")
      list(SummaryData = SummaryData)
    })
    
    output$infotablem51 <- DT::renderDataTable({
      df <- data_info_m51()$SummaryData
      DT::datatable(df)
    })
    #####
    
    filedata_m52 <- reactive({
      req(input$fileBcsv125)
      fileInput <- load_file(input$fileBcsv125$name, input$fileBcsv125$datapath)
      fileInput <- as.data.frame(fileInput)
      return(list(fileInput = fileInput))
    })
    
    data_info_m52 <- reactive({
      req(filedata_m52()$fileInput)
      Nrows <- nrow(filedata_m52()$fileInput)
      Ncols <- ncol(filedata_m52()$fileInput)
      SummaryData <- data.frame(list(N = c(Nrows, Ncols)))
      rownames(SummaryData) <- c("Nrows", "Ncols")
      list(SummaryData = SummaryData)
    })
    
    output$infotablem52 <- DT::renderDataTable({
      df <- data_info_m52()$SummaryData
      DT::datatable(df)
    })
    #####

    filedata_m53 <- reactive({
      req(input$fileBcsv135)
      fileInput <- load_file(input$fileBcsv135$name, input$fileBcsv135$datapath)
      fileInput <- as.data.frame(fileInput)
      return(list(fileInput = fileInput))
    })
    
    data_info_m53 <- reactive({
      req(filedata_m53()$fileInput)
      Nrows <- nrow(filedata_m53()$fileInput)
      Ncols <- ncol(filedata_m53()$fileInput)
      SummaryData <- data.frame(list(N = c(Nrows, Ncols)))
      rownames(SummaryData) <- c("Nrows", "Ncols")
      list(SummaryData = SummaryData)
    })
    
    output$infotablem53 <- DT::renderDataTable({
      df <- data_info_m53()$SummaryData
      DT::datatable(df)
    })
    #####
    
    filedata_m54 <- reactive({
      req(input$fileBcsv145)
      fileInput <- load_file(input$fileBcsv145$name, input$fileBcsv145$datapath)
      fileInput <- as.data.frame(fileInput)
      return(list(fileInput = fileInput))
    })
    
    data_info_m54 <- reactive({
      req(filedata_m54()$fileInput)
      Nrows <- nrow(filedata_m54()$fileInput)
      Ncols <- ncol(filedata_m54()$fileInput)
      SummaryData <- data.frame(list(N = c(Nrows, Ncols)))
      rownames(SummaryData) <- c("Nrows", "Ncols")
      list(SummaryData = SummaryData)
    })
    
    output$infotablem54 <- DT::renderDataTable({
      df <- data_info_m54()$SummaryData
      DT::datatable(df)
    })
    #####
    
    filedata_m55 <- reactive({
      req(input$fileBcsv155)
      fileInput <- load_file(input$fileBcsv155$name, input$fileBcsv155$datapath)
      fileInput <- as.data.frame(fileInput)
      return(list(fileInput = fileInput))
    })
    
    data_info_m55 <- reactive({
      req(filedata_m55()$fileInput)
      Nrows <- nrow(filedata_m55()$fileInput)
      Ncols <- ncol(filedata_m55()$fileInput)
      SummaryData <- data.frame(list(N = c(Nrows, Ncols)))
      rownames(SummaryData) <- c("Nrows", "Ncols")
      list(SummaryData = SummaryData)
    })
    
    output$infotablem55 <- DT::renderDataTable({
      df <- data_info_m55()$SummaryData
      DT::datatable(df)
    })
    ####################
    ####################
    ####################
    
    Stady2_mdf <- eventReactive(input$button112,{
      if(input$Cases == 2) {
          data1 <- filedata_m1()$fileInput
          data2 <- filedata_m11()$fileInput
          NF=as.numeric(input$NFro_11)
          measurePM_m = input$PMs111
          q=input$ValueqPM11
          outEval <- mco_two_meta_analysis(data1 = data1, data2 = data2, NF=NF, measurePM_m = measurePM_m, q = q) 
          finalEval <- outEval$final2
          dataPlot <- data_build(f1 = outEval$F1, X = outEval$X, Y = outEval$Y)
          return(list(final = finalEval, dataPlot=dataPlot))
      }
    })
    #####
    
    Stady3_mdf <- eventReactive(input$button113,{
      if(input$Cases == 3) {
        data1 <- filedata_m3()$fileInput
        data2 <- filedata_m32()$fileInput
        data3 <- filedata_m33()$fileInput
        NF=as.numeric(input$NFro_11)
        measurePM_m = input$PMs111
        q=input$ValueqPM11
        
        outEval <- mco_three_meta_analysis(data1 = data1, data2 = data2, data3 = data3, NF=NF, measurePM_m = measurePM_m, q = q) 
        finalEval <- outEval$final2
        dataPlot <- data_build2(f1 = outEval$F1, X = outEval$X, Y = outEval$Y, Z = outEval$Z)
        return(list(final = finalEval, dataPlot=dataPlot))
      }
    })
    
    #######
    Stady4_mdf <- eventReactive(input$button114,{
      if(input$Cases == 4) {
        data1 <- filedata_m41()$fileInput
        data2 <- filedata_m42()$fileInput
        data3 <- filedata_m43()$fileInput
        data4 <- filedata_m44()$fileInput
        NF=as.numeric(input$NFro_11)
        measurePM_m = input$PMs111
        q=input$ValueqPM11
        
        outEval <- mco_four_meta_analysis(data1 = data1, data2 = data2, data3 = data3, data4 = data4,
                                          NF=NF, measurePM_m = measurePM_m, q = q) 
        finalEval <- outEval$final2
        return(list(final = finalEval))
      }
    })
    #######
    Stady5_mdf <- eventReactive(input$button115,{
      if(input$Cases == 5) {
        data1 <- filedata_m51()$fileInput
        data2 <- filedata_m52()$fileInput
        data3 <- filedata_m53()$fileInput
        data4 <- filedata_m54()$fileInput
        data5 <- filedata_m55()$fileInput
        NF=as.numeric(input$NFro_11)
        measurePM_m = input$PMs111
        q=input$ValueqPM11
        
        outEval <- mco_five_meta_analysis(data1 = data1, data2 = data2, data3 = data3, data4 = data4, data5 = data5,
                                          NF=NF, measurePM_m = measurePM_m, q = q) 
        finalEval <- outEval$final2
        return(list(final = finalEval))
      }
    })
    
    #######
    
    
    plot1_m <- reactive({
      if(input$Cases == 2){
        df <- Stady2_mdf()$dataPlot
        p <- ggplot2::ggplot(df, ggplot2::aes(X, Y, group = as.factor(etiq) ,color=as.factor(etiq))) +
          ggplot2::geom_point() +
          ggplot2::labs(x = "PM1:Median", y = "PM2:Median") +
          ggplot2::geom_line(ggplot2::aes(linetype=as.factor(etiq)),show.legend = FALSE ) +
          ggplot2::theme(legend.position = "none") + ggplot2::guides(fill=FALSE, color=FALSE) +
          ggplot2::theme_bw()
        return(p)
      }
    })
    
    output$mtable12  <- DT::renderDataTable({
      if(input$Cases == 2){
        df <- as.data.frame(Stady2_mdf()$final)
        DT::datatable(df)
      }
    })
    
    output$mplot12  <- renderPlot({
      if(input$Cases == 2){
        plot1_m()
      }
    })
    
    output$downloadData.2metanalisis <- downloadHandler(
      filename = function() {
        paste("Result_2metanalisis", ".csv", sep = "")
        
      },
      content = function(file) {
        write.csv(Stady2_mdf()$final, file, row.names = FALSE)
      }
    )
    
    ######## 
    
    plot3_m <- reactive({
      if(input$Cases == 3){
        df <- Stady3_mdf()$dataPlot
        
        p <- plotly::plot_ly(df,x = ~X, y = ~Y, z = ~Z, type="scatter3d", mode="markers", color =~etiq,
                             colors = c("gray", "purple", "red", "green",  "cyan","blue", "coral","deepskyblue", "orange",  "yellow", "pink")
        ) %>%
          plotly::layout(
            title = "MCO",
            scene = list(
              xaxis = list(title = "PM1:median"),
              yaxis = list(title = "PM2:median"),
              zaxis = list(title = "PM3:median")
            )
          )
        return(p)
      }
    })
    
    output$mtable13  <- DT::renderDataTable({
      if(input$Cases == 3){
        df <- as.data.frame(Stady3_mdf()$final)
        DT::datatable(df)
      }
    })
    
    output$mplot13 <- plotly::renderPlotly({  
      plot3_m()
    })
    
    output$downloadData.3metanalisis <- downloadHandler(
      filename = function() {
        paste("Result_3metanalisis", ".csv", sep = "")
        
      },
      content = function(file) {
        write.csv(Stady3_mdf()$final, file, row.names = FALSE)
      }
    )
    
    ######
    output$mtable14  <- DT::renderDataTable({
      if(input$Cases == 4){
        df <- as.data.frame(Stady4_mdf()$final)
        DT::datatable(df)
      }
    })
    
    output$downloadData.4metanalisis <- downloadHandler(
      filename = function() {
        paste("Result_4metanalisis", ".csv", sep = "")
        
      },
      content = function(file) {
        write.csv(Stady4_mdf()$final, file, row.names = FALSE)
      }
    )
    
    ######
    output$mtable15  <- DT::renderDataTable({
      if(input$Cases == 5){
        df <- as.data.frame(Stady5_mdf()$final)
        DT::datatable(df)
      }
    })
    
    
    output$downloadData.5metanalisis <- downloadHandler(
      filename = function() {
        paste("Result_5metanalisis", ".csv", sep = "")
        
      },
      content = function(file) {
        write.csv(Stady5_mdf()$final, file, row.names = FALSE)
      }
    )
    
    
  })
}
    
## To be copied in the UI
# mod_Meta_analysis_ui("Meta_analysis_ui_1")
    
## To be copied in the server
# mod_Meta_analysis_server("Meta_analysis_ui_1")
