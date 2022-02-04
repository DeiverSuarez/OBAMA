#' Individual_by_Sex UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Individual_by_Sex_ui <- function(id){
  options(shiny.maxRequestSize=30*1024^8)
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(width = 4,
                   radioButtons(ns("performance_metrics_sex"), "Number of performance metrics:",  
                                choices = list("Two metrics"=1,"Three metrics"=2)
                                ),
                   
        ################################Two metricsL##################################################################################
                   
                   conditionalPanel(condition = "input.performance_metrics_sex==1", ns=ns,
                                    
                                    fluidRow(column(6,style=list("padding-right: 5px;"),
                                                    selectInput(ns("PMs_sex1"), label = "PM1",
                                                                choices = list("Mean"=1,"Median"=2,"Quantile"=3))),
                                             column(6,style=list("padding-left: 28px;"),
                                                    selectInput(ns("PMs_sex2"), label = "PM2",
                                                                choices = list("Median"=2,"Mean"=1,"Quantile"=3)))),
                                    
                                    fluidRow(column(6,style=list("padding-right: 5px;"),
                                                    conditionalPanel(condition ="input.PMs_sex1==3", ns=ns,
                                                                     column(6.1, style=list("padding-right: 5px;"),
                                                                            textInput(ns("ValueqPM_sex1"),label = h5("Quantile value (q)"),
                                                                                      value = 75)) )),
                                             column(6,style=list("padding-left: 28px;"),
                                                    conditionalPanel(condition ="input.PMs_sex2==3", ns=ns,
                                                                     column(6.1, style=list("padding-right: 5px;"),
                                                                            textInput(ns("ValueqPM_sex2"),label = h5("Quantile value (q)"),
                                                                                      value = 75)) ))),
                                    
                                    
                                    fluidRow(column(4,textInput(ns("NFro_sex1"),label = h5("Number of frontiers"),value = "10")))               
                                    
                              ),
        
        ################################Three metrics##################################################################################
        
        conditionalPanel(condition = "input.performance_metrics_sex==2", ns=ns,
                         
                         fluidRow(column(4,style=list("padding-right: 5px;"),
                                         selectInput(ns("PMs_sex21"), label = "PM1",
                                                     choices = list("Mean"=1,"Median"=2,"Quantile"=3))),
                                  column(4,style=list("padding-right: 5px;"),
                                         selectInput(ns("PMs_sex22"), label = "PM2",
                                                     choices = list("Median"=2,"Mean"=1,"Quantile"=3))),
                                  column(4,style=list("padding-right: 5px;"),
                                         selectInput(ns("PMs_sex23"), label = "PM3",
                                                     choices = list("Quantile"=3,"Mean"=1,"Median"=2)))
                         ),
                         
                         fluidRow(column(4,
                                         conditionalPanel(condition ="input.PMs_sex21==3", ns=ns,
                                                          column(6.1, style=list("padding-right: 5px;"),
                                                                 textInput(ns("ValueqPM_sex21"),label = h5("Quantile value (q)"),
                                                                           value = 75)) )),
                                  column(4,
                                         conditionalPanel(condition ="input.PMs_sex22==3",ns=ns,
                                                          column(6.1, style=list("padding-right: 5px;"),
                                                                 textInput(ns("ValueqPM_sex22"),label = h5("Quantile value (q)"),
                                                                           value = 75)) )),
                                  column(4,
                                         conditionalPanel(condition ="input.PMs_sex23==3", ns=ns,
                                                          column(6.1, style=list("padding-right: 5px;"),
                                                                 textInput(ns("ValueqPM_sex23"),label = h5("Quantile value (q)"),
                                                                           value = 75)) ))),
                         fluidRow(column(4,textInput(ns("NFro_sex2"),label = h5("Frontiers"),value = "5")))
                       ),
        
        
        hr(),
        
        
        
        
        ################################Data para una enfermedad1 Two metrics#####################################################################################
        conditionalPanel(condition = "input.performance_metrics_sex==1", ns=ns,
                         
                         ######enfermedad2
                         fileInput(ns("fileBcsv_sex1"),accept = c('text/csv','text/comma-separated-values,text/plain','.csv'),
                                   label = h5("Gene Expression Data")),
                         
                         fileInput(ns("filesex1"),accept = c('text/csv','text/comma-separated-values,text/plain','.csv'),
                                   label = h5("Sex information")),
                         
                         actionButton(ns("button_sex1"),"Run",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                         
                         
                         hr(),
                         downloadButton(ns("downloadData.female"), "Save My Female Data"),
                         downloadButton(ns("downloadData.male"), "Save My Male Data"),
                         
                         downloadButton(ns("downloadData.control"), "Save My Control Data"),
                         downloadButton(ns("downloadData.desease"), "Save My Disease Data"),
                         
                         hr(),
                         downloadButton(ns("downloadData.sex_female"), "Save My Female Results "),
                         downloadButton(ns("downloadData.sex_male"), "Save My Male Results "),
                         downloadButton(ns("downloadData.sex_control"), "Save My Control Results "),
                         downloadButton(ns("downloadData.sex_disease"), "Save My Disease Results ")
                         ),
        ################################Data para una enfermedad1 Three metrics#####################################################################################
        
        conditionalPanel(condition = "input.performance_metrics_sex==2",ns=ns,
                         
                         ######enfermedad2
                         fileInput(ns("fileBcsv_sex2"),accept = c('text/csv','text/comma-separated-values,text/plain','.csv'),
                                   label = h5("Gene Expression Data")),
                         
                         fileInput(ns("filesex2"),accept = c('text/csv','text/comma-separated-values,text/plain','.csv'),
                                   label = h5("Sex information")),
                         
                         actionButton(ns("button_sex_2"),"Ok",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                         
                         downloadButton(ns("downloadData.sex2"), "Save My Results")
                        )
                   
           ),
      conditionalPanel(condition = "input.performance_metrics_sex==1 || input.performance_metrics_sex==2", ns=ns,
                       mainPanel(
                         tabsetPanel(type = "tabs",
                                     tabPanel("SummaryData",
                                              conditionalPanel(condition = "input.performance_metrics_sex==1",ns=ns,
                                                               DT::DTOutput(ns("infotable_sex1"))
                                                               ),
                                              conditionalPanel(condition = "input.performance_metrics_sex==2",ns=ns,
                                                               DT::DTOutput(ns("infotable_sex2"))
                                                               )
                                     ),
                                     tabPanel("Frontiers",
                                              conditionalPanel(condition = "input.performance_metrics_sex==1",ns=ns,
                                                               DT::DTOutput(ns("table_sex1")), 
                                                               DT::DTOutput(ns("table_sex2")),
                                                               DT::DTOutput(ns("table_sex3")),
                                                               DT::DTOutput(ns("table_sex4"))
                                                              
                                              ),
                                              conditionalPanel(condition = "input.performance_metrics_sex==2",ns=ns,
                                                               #DT::dataTableOutput(ns("table_sex2")),
                                                               #                  DT::dataTableOutput("table22"),
                                                               #                  DT::dataTableOutput("table23"),
                                                               #                  DT::dataTableOutput("table24"),
                                                               #                  DT::dataTableOutput("table25")
                                              )
                                     ),
                                     tabPanel("Plot",conditionalPanel(condition = "input.performance_metrics_sex==1", ns=ns,
                                                                      fluidRow(column(width = 6,plotOutput(ns("plot_sex1"))),
                                                                               column(width = 6,plotOutput(ns("plot_sex2"))),
                                                                               column(width = 6,plotOutput(ns("plot_sex3"))),
                                                                               column(width = 6,plotOutput(ns("plot_sex4")))
                                                                               
                                                                      )
                                     ),
                                     conditionalPanel(condition = "input.performance_metrics_sex==2", ns=ns,
                                                      fluidRow(column(width = 5,plotly::plotlyOutput(ns("plot_sex21"))),
                                                               #                           column(width = 5,plotlyOutput("plot22")),
                                                               #                           column(width = 5,plotlyOutput("plot23")),
                                                               #                            column(width = 5,plotlyOutput("plot24")),
                                                               #                            column(width = 5,plotlyOutput("plot25"))
                                                              )
                                                     )
                                     
                                              ),
                                     tabPanel("VENN", conditionalPanel(condition = "input.performance_metrics_sex==1", ns=ns,
                                                                       plotOutput(ns("plot_venn_sex")), DT::DTOutput(ns("info_venn_sex")))
                                     )
                                     
                                    )
                             )
                       
    )
      
      
    )
    
    
    
    )
}
    
#' Individual_by_Sex Server Functions
#'
#' @noRd 
mod_Individual_by_Sex_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    #### Data 1 to 2 PMs   
    filedata_sex1 <- reactive({
      req(input$fileBcsv_sex1)
      fileInput <- load_file(input$fileBcsv_sex1$name, input$fileBcsv_sex1$datapath)
      fileInput <- as.data.frame(fileInput)
      return(list(fileInput = fileInput))
    })
    
    
    #### Data1 SEX to 2 PMs   
    data_sex1 <- reactive({
      req(input$filesex1)
      fileInput <- load_file(input$filesex1$name, input$filesex1$datapath)
      fileInput <- as.data.frame(fileInput)
      return(list(fileInput = fileInput))
    })
    #
    
    
    data_info_sex1 <- reactive({
      req(filedata_sex1()$fileInput)
      req(data_sex1()$fileInput)
      Nrows <- nrow(filedata_sex1()$fileInput)
      Ncols <- ncol(filedata_sex1()$fileInput)-2
      Controls <- table(filedata_sex1()$fileInput[,2])[1]
      disease <- table(filedata_sex1()$fileInput[,2])[2]
       if (input$PMs_sex1==1){performance_metrics1="Mean"}
       if (input$PMs_sex1==2){performance_metrics1="Median"}
       if (input$PMs_sex1==3){performance_metrics1="Quantile"}
       if (input$PMs_sex2==1){performance_metrics2="Mean"}
       if (input$PMs_sex2==2){performance_metrics2="Median"}
       if (input$PMs_sex2==3){performance_metrics2="Quantile"}
      frontiers <- input$NFro_sex1
      female <- table(data_sex1()$fileInput[,2])[1]
      male <- table(data_sex1()$fileInput[,2])[2]
      
      
      SummaryData <- data.frame(list(N = c(Nrows, Ncols, Controls, disease, performance_metrics1, performance_metrics2, frontiers, female, male)))
      rownames(SummaryData) <- c("Samples", "Genes","Controls", "Disease", "Performance metrics 1", "Performance_metrics 2", "Frontiers", "Female", "Male" )
      list(SummaryData = SummaryData)
    })
    
    
    particion_data <- reactive({
      Data_Exp <- as.data.frame(filedata_sex1()$fileInput)
      Data_SEX <- as.data.frame(data_sex1()$fileInput)
      data_partition(Data_Exp=Data_Exp,Data_SEX=Data_SEX)
       
      
      })
    
    ###
    df_female <- eventReactive("button_sex1",{
      if(input$performance_metrics_sex == 1) {
        
        wnv <- as.data.frame(particion_data()$geo.final99039_Female)
        NF=as.numeric(input$NFro_sex1)
        measurePM1 = input$PMs_sex1
        measurePM2 = input$PMs_sex2
        q1=input$alueqPM_sex1
        q2=input$ValueqPM_sex2
        
        outEval <- mco_one_diseases(data = wnv, NF = NF, measurePM1 = measurePM1, 
                                    measurePM2 = measurePM2,
                                    q1 = q1, 
                                    q2 = q2)
        finalEval <- outEval$final2
        dataPlot <- data_build(f1 = outEval$f1, X = outEval$X, Y = outEval$Y)
        return(list(final = finalEval, dataPlot = dataPlot))
        
      }
      
    })
    
    output$table_sex1<- DT::renderDataTable({
      if(input$performance_metrics_sex == 1){
        df <- as.data.frame(df_female()$final)
        DT::datatable(df,caption = 'Female Results')
      }
    })
    
    #download_Result.female
    output$downloadData.sex_female <- downloadHandler(
      filename = function() {
        paste("Result_female", ".csv", sep = "")
        
      },
      content = function(file) {
        write.csv(df_female()$final, file, row.names = FALSE)
      }
    )
    
  #################################################  
    df_male <- eventReactive("button_sex1",{
      if(input$performance_metrics_sex == 1) {
        
        wnv <- as.data.frame(particion_data()$geo.final99039_Male)
        NF=as.numeric(input$NFro_sex1)
        measurePM1 = input$PMs_sex1
        measurePM2 = input$PMs_sex2
        q1=input$alueqPM_sex1
        q2=input$ValueqPM_sex2
        
        outEval <- mco_one_diseases(data = wnv, NF = NF, measurePM1 = measurePM1, 
                                    measurePM2 = measurePM2,
                                    q1 = q1, 
                                    q2 = q2)
        finalEval <- outEval$final2
        dataPlot <- data_build(f1 = outEval$f1, X = outEval$X, Y = outEval$Y)
        return(list(final = finalEval, dataPlot = dataPlot))
        
      }
      
    })
    
    output$table_sex2<- DT::renderDataTable({
      if(input$performance_metrics_sex == 1){
        df <- as.data.frame(df_male()$final)
        DT::datatable(df,caption = 'Male Results')
      }
    })
    
    #download_Result.male
    output$downloadData.sex_male <- downloadHandler(
      filename = function() {
        paste("Result_male", ".csv", sep = "")
        
      },
      content = function(file) {
        write.csv(df_male()$final, file, row.names = FALSE)
      }
    )

    ################################################# 
    df_control <- eventReactive("button_sex1",{
      if(input$performance_metrics_sex == 1) {
        
        wnv <- as.data.frame(particion_data()$geo.final99039_Control)
        NF=as.numeric(input$NFro_sex1)
        measurePM1 = input$PMs_sex1
        measurePM2 = input$PMs_sex2
        q1=input$alueqPM_sex1
        q2=input$ValueqPM_sex2
        
        outEval <- mco_one_diseases(data = wnv, NF = NF, measurePM1 = measurePM1, 
                                    measurePM2 = measurePM2,
                                    q1 = q1, 
                                    q2 = q2)
        finalEval <- outEval$final2
        dataPlot <- data_build(f1 = outEval$f1, X = outEval$X, Y = outEval$Y)
        return(list(final = finalEval, dataPlot = dataPlot))
        
      }
      
    })
    
    output$table_sex3<- DT::renderDataTable({
      if(input$performance_metrics_sex == 1){
        df <- as.data.frame(df_control()$final)
        DT::datatable(df,caption = 'Control Results')
      }
    })
    
    #download_Result.control
    output$downloadData.sex_control <- downloadHandler(
      filename = function() {
        paste("Result_control", ".csv", sep = "")
        
      },
      content = function(file) {
        write.csv(df_control()$final, file, row.names = FALSE)
      }
    )
    ################################################# 
    
    df_disease <- eventReactive("button_sex1",{
      if(input$performance_metrics_sex == 1) {
        
        wnv <- as.data.frame(particion_data()$geo.final99039_Disease)
        NF=as.numeric(input$NFro_sex1)
        measurePM1 = input$PMs_sex1
        measurePM2 = input$PMs_sex2
        q1=input$alueqPM_sex1
        q2=input$ValueqPM_sex2
        
        outEval <- mco_one_diseases(data = wnv, NF = NF, measurePM1 = measurePM1, 
                                    measurePM2 = measurePM2,
                                    q1 = q1, 
                                    q2 = q2)
        finalEval <- outEval$final2
        dataPlot <- data_build(f1 = outEval$f1, X = outEval$X, Y = outEval$Y)
        return(list(final = finalEval, dataPlot = dataPlot))
        
      }
      
    })
    
    output$table_sex4<- DT::renderDataTable({
      if(input$performance_metrics_sex == 1){
        df <- as.data.frame(df_disease()$final)
        DT::datatable(df, caption = 'Disease Results')
      }
    })
    ######### VENN
    grap_venn_sex <- reactive({
     
      genes = merge(df_female()$final, df_male()$final, by = "Gene", all = TRUE) 
      genes2 = merge(genes,df_control()$final, by = "Gene", all = TRUE  )
      genes3 = merge(genes2,df_disease()$final, by = "Gene", all = TRUE  )
      
      colnames(genes3)= c("Gene", "Female", "Male", "Control", "Condition")
      genes3$Female[!is.na(genes3$Female)] <- 1
      genes3$Male[!is.na(genes3$Male)] <- 1
      genes3$Control[!is.na(genes3$Control)] <- 1
      genes3$Condition[!is.na(genes3$Condition)] <- 1
      genes3[is.na(genes3)] <- 0
      
      genes3$Count=rowSums(genes3[2:5])
      
      venn4 = as.data.frame(genes3[,c("Control","Female", "Male", "Condition")])
      
      grap <- Venn(Data=venn4,c=1,t=4)$Venn
      return(list(grap=grap,genes3=genes3))
    })
    
    
    output$plot_venn_sex <- renderPlot({
      grap_venn_sex()$grap
    })
    
    output$info_venn_sex <- DT::renderDataTable({
      df <- as.data.frame(grap_venn_sex()$genes3)
      DT::datatable(df)
    })
    
    
    #download_Result.disease
    output$downloadData.sex_disease <- downloadHandler(
      filename = function() {
        paste("Result_disease", ".csv", sep = "")
        
      },
      content = function(file) {
        write.csv(df_disease()$final, file, row.names = FALSE)
      }
    )
    
    #################################################  
    
    
    
    #downloadData.female
    output$downloadData.female <- downloadHandler(
      filename = function() {
        paste("Data_female", ".csv", sep = "")
        
      },
      content = function(file) {
        write.csv(particion_data()$geo.final99039_Female, file, row.names = FALSE)
      }
    )

    
    
    
    ###
   
    
    #downloadData.male
    output$downloadData.male <- downloadHandler(
      filename = function() {
        paste("Data_male", ".csv", sep = "")
        
      },
      content = function(file) {
        write.csv(particion_data()$geo.final99039_Male, file, row.names = FALSE)
      }
    )
    
    
    #downloadData.control
    output$downloadData.control <- downloadHandler(
      filename = function() {
        paste("Data_control", ".csv", sep = "")
        
      },
      content = function(file) {
        write.csv(particion_data()$geo.final99039_Control, file, row.names = FALSE)
      }
    )
    
    #downloadData.desease
    output$downloadData.desease <- downloadHandler(
      filename = function() {
        paste("Data_disease", ".csv", sep = "")
        
      },
      content = function(file) {
        write.csv(particion_data()$geo.final99039_Disease, file, row.names = FALSE)
      }
    )
    
 output$infotable_sex1 <- DT::renderDataTable({
      df <- data_info_sex1()$SummaryData
      DT::datatable(df)
    })
 
 plot1_sex <- reactive({
   if(input$performance_metrics_sex == 1){
     df <- df_female()$dataPlot
     p <- ggplot2::ggplot(df, ggplot2::aes(X, Y, group = as.factor(etiq) ,color=as.factor(etiq))) +
       ggplot2::geom_point() +
       ggplot2::labs(x = "PM1:Median", y = "PM2:Mean") +
       ggplot2::geom_line(ggplot2::aes(linetype=as.factor(etiq)),show.legend = FALSE ) +
       ggplot2::theme(legend.position = "none") + ggplot2::guides(fill=FALSE, color=FALSE) +
       ggplot2::theme_bw()
     return(p)
   }
 })
 
 output$plot_sex1 <- renderPlot({
   plot1_sex()
 })
 
 
 plot2_sex <- reactive({
   if(input$performance_metrics_sex == 1){
     df <- df_male()$dataPlot
     p <- ggplot2::ggplot(df, ggplot2::aes(X, Y, group = as.factor(etiq) ,color=as.factor(etiq))) +
       ggplot2::geom_point() +
       ggplot2::labs(x = "PM1:Median", y = "PM2:Mean") +
       ggplot2::geom_line(ggplot2::aes(linetype=as.factor(etiq)),show.legend = FALSE ) +
       ggplot2::theme(legend.position = "none") + ggplot2::guides(fill=FALSE, color=FALSE) +
       ggplot2::theme_bw()
     return(p)
   }
 })
 
 output$plot_sex2 <- renderPlot({
   plot2_sex()
 })
 
 plot3_sex <- reactive({
   if(input$performance_metrics_sex == 1){
     df <- df_control()$dataPlot
     p <- ggplot2::ggplot(df, ggplot2::aes(X, Y, group = as.factor(etiq) ,color=as.factor(etiq))) +
       ggplot2::geom_point() +
       ggplot2::labs(x = "PM1:Median", y = "PM2:Mean") +
       ggplot2::geom_line(ggplot2::aes(linetype=as.factor(etiq)),show.legend = FALSE ) +
       ggplot2::theme(legend.position = "none") + ggplot2::guides(fill=FALSE, color=FALSE) +
       ggplot2::theme_bw()
     return(p)
   }
 })
 
 output$plot_sex3 <- renderPlot({
   plot3_sex()
 })
 
 plot4_sex <- reactive({
   if(input$performance_metrics_sex == 1){
     df <- df_disease()$dataPlot
     p <- ggplot2::ggplot(df, ggplot2::aes(X, Y, group = as.factor(etiq) ,color=as.factor(etiq))) +
       ggplot2::geom_point() +
       ggplot2::labs(x = "PM1:Median", y = "PM2:Mean") +
       ggplot2::geom_line(ggplot2::aes(linetype=as.factor(etiq)),show.legend = FALSE ) +
       ggplot2::theme(legend.position = "none") + ggplot2::guides(fill=FALSE, color=FALSE) +
       ggplot2::theme_bw()
     return(p)
   }
 })
 
 output$plot_sex4 <- renderPlot({
   plot4_sex()
 })
 
 
 
  })
}
    
## To be copied in the UI
# mod_Individual_by_Sex_ui("Individual_by_Sex_ui_1")
    
## To be copied in the server
# mod_Individual_by_Sex_server("Individual_by_Sex_ui_1")
