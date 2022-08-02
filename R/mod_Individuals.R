#' Individuals UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' 
#' @importFrom utils write.csv
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Individuals_ui <- function(id){
  options(shiny.maxRequestSize=30*1024^8)
  ns <- NS(id)
  tagList(
    sidebarLayout(
      
      sidebarPanel(
        width = 4,
        radioButtons(
          ns("performance_metrics"), 
          "Number of performance metrics:",  
          choices = list("Two metrics"=1,
                         "Three metrics"=2
                         )
                     ),
                   
                   
         ################################Two metricsL##########################     
                   
        conditionalPanel(
          condition = "input.performance_metrics==1",
          ns=ns,
          
          fluidRow(
            column(
              6,
              style=list("padding-right: 5px;"),
              selectInput(
                ns("PMs1"),
                label = "PM1",
                choices = list("Median"=2,
                               "Mean"=1,
                               "Quantile"=3
                               )
                          )
                   ),
            
            column(
              6,
              style=list("padding-left: 28px;"),
              selectInput(
                ns("PMs2"),
                label = "PM2",
                choices = list("Mean"=1,
                               "Median"=2,
                               "Quantile"=3
                               )
                          )
                   )
                   ),
          
          fluidRow(
            column(
              6,
              style=list("padding-right: 5px;"),
              conditionalPanel(condition ="input.PMs1==3", 
                               ns=ns,
                               column(
                                 6.1,
                                 style=list("padding-right: 5px;"),
                                 textInput(
                                   ns("ValueqPM1"),
                                   label = h5("Quantile value (q)"),
                                   value = 75)
                                 ) 
                               )
                   ),
            
            column(
              6,
              style=list("padding-left: 28px;"),
              conditionalPanel(condition ="input.PMs2==3", 
                               ns=ns,
                               column(
                                 6.1, 
                                 style=list("padding-right: 5px;"),
                                 textInput(
                                   ns("ValueqPM2"),
                                   label = h5("Quantile value (q)"),
                                   value = 75))
                               )
                  )
            ),
          
          fluidRow(
            column(
              4,
              radioButtons(ns("Nudata1"), 
                           label = "Number of datasets",
                           choices = list("One"= 1,
                                          "Two" = 2,
                                          "Three" = 3,
                                          "Four" = 4,
                                          "Five" = 5)
                           )
              )
            ),
          
          fluidRow(
            column(
              4,
              textInput(
                ns("NFro1"),
                label = h5("Number of frontiers"),
                value = "10")
              ),
            )  
          ),
                   
                   ################################Three metrics###############
                   
        conditionalPanel(
          condition = "input.performance_metrics==2", 
          ns=ns,
          fluidRow(
            column(
              4,
              style=list("padding-right: 5px;"),
              selectInput(
                ns("PMs31"),
                label = "PM1",
                choices = list("Mean"=1,
                               "Median"=2,
                               "Quantile"=3
                               )
                          )
                  ),
            
            column(
              4,
              style=list("padding-right: 5px;"),
              selectInput(
                ns("PMs32"),
                label = "PM2",
                choices = list("Mean"=1,
                               "Median"=2,
                               "Quantile"=3)
                               )
                   ),
            
            column(
              4,
              style=list("padding-right: 5px;"),
              selectInput(
                ns("PMs33"),
                label = "PM3",
                choices = list("Mean"=1,
                               "Median"=2,
                               "Quantile"=3
                               )
                )
              )
            ),
          fluidRow(
            column(
              4,
              conditionalPanel(
                condition ="input.PMs31==3", 
                ns=ns,
                column(
                  6, 
                  style=list("padding-right: 5px;"),
                  textInput(ns("ValueqPM31"),
                            label = h5("Quantile value (q)"),
                            value = 75)
                  )
                )
              ),
            
            column(
              4,
              conditionalPanel(
                condition ="input.PMs32==3",
                ns=ns,
                column(
                  6, 
                  style=list("padding-right: 5px;"),
                  textInput(ns("ValueqPM32"),
                            label = h5("Quantile value (q)"),
                            value = 75)
                  )
                )
              ),
                                             
            column(
              4,  
              conditionalPanel(
                condition ="input.PMs33==3", 
                ns=ns,
                column(
                  6,
                  style=list("padding-right: 5px;"),
                  textInput(
                    ns("ValueqPM33"),
                    label = h5("Quantile value (q)"),
                    value = 75)
                  )
                )
              )
            ),
         
           fluidRow(
            column(
              4,
              textInput(
                ns("NFro21"),
                label = h5("Number of frontiers"),
                value = "5"
                )
              )
            )
          ),
        hr(),
        
        ##################Data para una enfermedad1 Two metrics################
        
        ######enfermedad1
        conditionalPanel(
          condition = "input.performance_metrics== 1",
          ns=ns,
          conditionalPanel(
            condition = "input.Nudata1== 1 ", 
            ns=ns,
            fileInput(
              ns("fileBcsv"),
              accept = c('text/csv',
              'text/comma-separated-values,
              text/plain',
              '.csv'
              ),
              label = h5("Gene Expression Data 1")),
            actionButton(
              ns("button1"),
              "Run",
              style="color: #fff; background-color: #337ab7;
              border-color: #2e6da4"),
            downloadButton(
              ns("downloadData.one"),
              "Save My Results")
            ),
          
          ####enfermedad2
          conditionalPanel(
            condition = "input.Nudata1== 2",
            ns=ns,
            fileInput(
              ns("fileBcsv2_1"),
              accept = c('text/csv','
                         text/comma-separated-values,
                         text/plain',
                         '.csv'
                         ),
              label = h5("Gene Expression Data 1")),
            downloadButton(
              ns("downloadData21.two"),
              "Save My Results"
              ),
            hr(),
            
            fileInput(
              ns("fileBcsv22"),
              accept = c('text/csv',
                         'text/comma-separated-values,text/plain',
                         '.csv'),
              label = h5("Gene Expression Data 2")
              ),
            downloadButton(
              ns("downloadData22.two"), 
              "Save My Results"),
            hr(),
            
            actionButton(
              ns("button2"),
              "Run",
              style="color: #fff; background-color: #337ab7;
              border-color: #2e6da4"),
            ),
          
          ####enfermedad3
          conditionalPanel(
            condition = "input.Nudata1== 3",
            ns=ns,
            fileInput(
              ns("fileBcsv31"),
              accept = c('text/csv',
                         'text/comma-separated-values,text/plain',
                         '.csv'
                         ),
              label = h5("Gene Expression Data 1")
              ),
            
            downloadButton(
              ns("downloadData31.three"), 
              "Save My Results"),
            hr(),
            
            fileInput(
              ns("fileBcsv32"),
              accept = c('text/csv',
                         'text/comma-separated-values,
                         text/plain',
                         '.csv'
                         ),
              label = h5("Gene Expression Data 2")
              ),
                                                   
              downloadButton(
                ns("downloadData32.three"),
                "Save My Results"),      
               hr(),
            
            fileInput(
              ns("fileBcsv33"),
              accept = c('text/csv',
                         'text/comma-separated-values,
                         text/plain',
                         '.csv'
                         ),
              label = h5("Gene Expression Data 3")
              ),
                                                    
             downloadButton(
               ns("downloadData33.three"),
               "Save My Results"),
            hr(),
                                                     
            actionButton(
              ns("button3"),
              "Run",
              style="color: #fff; background-color: #337ab7;
              border-color: #2e6da4"),
            ),
          
          
          ####enfermedad4
          conditionalPanel(
            condition = "input.Nudata1== 4", 
            ns=ns,
            fileInput(
              ns("fileBcsv41"),
              accept = c('text/csv',
                         'text/comma-separated-values,
                         text/plain',
                         '.csv')
              ,
              label = h5("Gene Expression Data 1")),
            
            downloadButton(
              ns("downloadData41.four"), 
              "Save My Results"),
            hr(),
                                                     
            fileInput(
              ns("fileBcsv42"),
              accept = c('text/csv',
                         'text/comma-separated-values,
                         text/plain',
                         '.csv'),
              label = h5("Gene Expression Data 2")),
            
            downloadButton(
              ns("downloadData42.four"), "Save My Results"),
            hr(),
                                                    
            fileInput(
              ns("fileBcsv43"),
              accept = c('text/csv',
                         'text/comma-separated-values,
                         text/plain',
                         '.csv'),
              label = h5("Gene Expression Data 3")),
            
            downloadButton(
              ns("downloadData43.four"),
              "Save My Results"),
            hr(),
            
            fileInput(
              ns("fileBcsv44"),
              accept = c('text/csv',
                         'text/comma-separated-values,
                         text/plain',
                         '.csv'
                         ),
              label = h5("Gene Expression Data 4")
              ),
                                                     
            downloadButton(
              ns("downloadData44.four"), 
              "Save My Results"),
            hr(),
                                                     
            actionButton(
              ns("button4"),
              "Run",
              style="color: #fff; background-color: #337ab7; 
              border-color: #2e6da4"),
            ),
                                    
          ####enfermedad5
          conditionalPanel(
            condition = "input.Nudata1== 5", 
            ns=ns,
                                                     
            fileInput(
              ns("fileBcsv51"),
              accept = c('text/csv',
                         'text/comma-separated-values,
                         text/plain',
                         '.csv'
                         ),
              label = h5("Gene Expression Data 1")
              ),
                                                    
             downloadButton(
               ns("downloadData51.five"), 
               "Save My Results"),
            hr(),
                                                     
            fileInput(
              ns("fileBcsv52"),
              accept = c('text/csv',
                         'text/comma-separated-values,
                         text/plain',
                         '.csv'
                         ),
              label = h5("Gene Expression Data 2")),
                                                     
            downloadButton(
              ns("downloadData52.five"), 
              "Save My Results"),
            hr(),
            
            fileInput(
              ns("fileBcsv53"),
              accept = c('text/csv',
                         'text/comma-separated-values,
                         text/plain',
                         '.csv'
                         ),
              label = h5("Gene Expression Data 3")
              ),
                                                     
            downloadButton(
              ns("downloadData53.five"), 
              "Save My Results"),
            hr(),
            
            fileInput(
              ns("fileBcsv54"),
              accept = c('text/csv',
                         'text/comma-separated-values,text/plain',
                         '.csv'
                         ),
              label = h5("Gene Expression Data 4")),
                                                     
            downloadButton(
              ns("downloadData54.five"),
              "Save My Results"),
            hr(),
                                                  
            fileInput(
              ns("fileBcsv55"),
              accept = c('text/csv',
                         'text/comma-separated-values,
                         text/plain',
                         '.csv'
                         ),
              label = h5("Gene Expression Data 5")
              ),
                                                     
            downloadButton(ns("downloadData55.five"), 
                           "Save My Results"),
            hr(),
            
            actionButton(
              ns("button5"),
              "Run",
              style="color: #fff; background-color: #337ab7;
              border-color: #2e6da4"
              ),
            ),
          ),
        ########################################
        
        
        ###############Data para una enfermedad1 Three metrics###############
      
                
        conditionalPanel(
          condition = "input.performance_metrics==2",
          ns=ns,
          
          ######enfermedad2
          fileInput(
            ns("fileBcsv21"),
            accept = c('text/csv',
                       'text/comma-separated-values,text/plain',
                       '.csv'
                       ),
            label = h5("Gene Expression Data 1")),
          actionButton(
            ns("button21"),
            "Run",style="color: #fff; background-color: #337ab7; 
            border-color: #2e6da4"),
          
          downloadButton(ns("downloadData.2one"), "Save My Results"),
                   #
                   #                  
          ######enfermedad2
          #                        
          # fileInput(
          #   ns("fileBcsv22"),
          #   accept = c('text/csv',
          #   'text/comma-separated-values,
          #   text/plain',
          #   '.csv'
          #   ),
        #   label = h5("Study 2")),
        #  
        # actionButton(
        #   ns("button22"),
        #   "Ok",
        #   style="color: #fff; 
        #   background-color: #337ab7; border-color: #2e6da4"),
                 
        # downloadButton(
        #   ns("downloadData.2two"), 
        #   "Save My Data"),
        # 
        # 
        # ######enfermedad3
        #   fileInput(
        #   ns("fileBcsv23"),
        #   accept = c('text/csv',
        #              'text/comma-separated-values,
        #              text/plain',
        #              '.csv'
        #              ),
        #   label = h5("Study 3")),
        # actionButton(
        #   ns("button23"),
        #   "Ok",
        #   style="color: #fff; background-color: #337ab7; 
        # #   border-color: #2e6da4"),
        # downloadButton(ns("downloadData.2three"), 
        #                "Save My Data"),
        # ######enfermedad4
        # fileInput(ns("fileBcsv24"),
        #           accept = c('text/csv',
        #                      'text/comma-separated-values,
        #                      text/plain',
        #                      '.csv'
        #                      ),
        #            #                          
        #           label = h5("Study 4")
        #           ),
        # actionButton(ns("button24"),
        #              "Ok",
        #              style="color: #fff; background-color: #337ab7; 
        #              border-color: #2e6da4"),
        # downloadButton(ns("downloadData.2four"), "Save My Data"),
        # ######enfermedad5
        # fileInput(ns("fileBcsv25"),
        #           accept = c('text/csv',
        #                      'text/comma-separated-values,
        #                      text/plain',
        #                      '.csv'
        #                      ),
        #           label = h5("Study 5")),
        # actionButton(ns("button25"),
        #              "Ok",style="color: #fff; background-color: #337ab7; 
        #              border-color: #2e6da4"),
        # downloadButton(ns("downloadData.2five"), 
        #                "Save My Data"
        #                ),
        #            #
                   ),
                   
                   ########################################
                   ########################################
      ),
      ################### mainPanel    
      
      conditionalPanel(
        condition = "input.performance_metrics==1 ||
        input.performance_metrics==2", ns=ns,
      mainPanel(
        tabsetPanel(
          type = "tabs",
          tabPanel("SummaryData",
                   conditionalPanel(
                     condition = "input.performance_metrics==1",ns=ns,
                     conditionalPanel(condition = "input.Nudata1== 1 ", 
                                      ns=ns,
                                      DT::DTOutput(ns("infotable")
                                                   )
                                      ),
                     conditionalPanel(
                       condition = "input.Nudata1== 2 ", 
                       ns=ns,
                       DT::DTOutput(ns("infotable2_1")),
                       DT::DTOutput(ns("infotable2_2"))
                                       ),
                     conditionalPanel(
                       condition = "input.Nudata1== 3 ",
                       ns=ns,
                       DT::DTOutput(ns("infotable31")),
                       DT::DTOutput(ns("infotable32")),
                       DT::DTOutput(ns("infotable33"))
                       ),
                     conditionalPanel(
                       condition = "input.Nudata1== 4 ", 
                       ns=ns,
                       DT::DTOutput(ns("infotable41")),
                       DT::DTOutput(ns("infotable42")),
                       DT::DTOutput(ns("infotable43")),
                       DT::DTOutput(ns("infotable44"))
                       ),
                     conditionalPanel(
                       condition = "input.Nudata1== 5 ", 
                       ns=ns,
                       DT::DTOutput(ns("infotable51")),
                       DT::DTOutput(ns("infotable52")),
                       DT::DTOutput(ns("infotable53")),
                       DT::DTOutput(ns("infotable54")),
                       DT::DTOutput(ns("infotable55"))
                       ),
                     ),
                   conditionalPanel(
                     condition = "input.performance_metrics==2",
                     ns=ns,
                     DT::DTOutput(ns("infotable21"))
                     )
                   ),
          tabPanel(
            "Frontiers",
            conditionalPanel(
              condition = "input.performance_metrics==1"
              ,ns=ns,
              conditionalPanel(
                condition = "input.Nudata1==1 ",
                ns=ns,
                DT::DTOutput(ns("table1"))
                ),
              conditionalPanel(
                condition = "input.Nudata1==2 ", 
                ns=ns,
                DT::DTOutput(ns("table2_1")),
                DT::DTOutput(ns("table2_2"))
                ),
              conditionalPanel(
                condition = "input.Nudata1==3 ",
                ns=ns,
                DT::DTOutput(ns("table31")),
                DT::DTOutput(ns("table32")),
                DT::DTOutput(ns("table33")),
                ),
              conditionalPanel
              (condition = "input.Nudata1==4 ", 
                ns=ns,
                DT::DTOutput(ns("table41")),
                DT::DTOutput(ns("table42")),
                DT::DTOutput(ns("table43")),
                DT::DTOutput(ns("table44")),
                ),
              conditionalPanel(
                condition = "input.Nudata1==5 ", 
                ns=ns,
                DT::DTOutput(ns("table51")),
                DT::DTOutput(ns("table52")),
                DT::DTOutput(ns("table53")),
                DT::DTOutput(ns("table54")),
                DT::DTOutput(ns("table55")),
                )
              # DT::dataTableOutput("table2"),
              # DT::dataTableOutput("table3"),
              # DT::dataTableOutput("table4"),
              # DT::dataTableOutput("table5")
              ),
            conditionalPanel(
              condition = "input.performance_metrics==2",
              ns=ns,
              DT::dataTableOutput(ns("table21")),
                             #                  DT::dataTableOutput("table22"),
                             #                  DT::dataTableOutput("table23"),
                             #                  DT::dataTableOutput("table24"),
                             #                  DT::dataTableOutput("table25")
              )
            ),
          tabPanel(
            "Plot", 
            conditionalPanel(
              condition = "input.performance_metrics==1", 
              ns=ns,
              conditionalPanel(
                condition = "input.Nudata1 == 1 ", 
                ns=ns,                
                plotOutput(ns("plot11"))
                ),
              conditionalPanel(
                condition = "input.Nudata1 == 2 ", 
                ns=ns,                
                column(width = 6,plotOutput(ns("plot_2_1")),
                       plotOutput(ns("plot_2_2")))
                ),
              conditionalPanel(
                condition = "input.Nudata1 == 3 ", 
                ns=ns,              
                plotOutput(ns("plot_3_1")),
                plotOutput(ns("plot_3_2")),
                plotOutput(ns("plot_3_3"))
                ),
              conditionalPanel(
                condition = "input.Nudata1 == 4 ", 
                ns=ns,                
                plotOutput(ns("plot_4_1")),
                plotOutput(ns("plot_4_2")),
                plotOutput(ns("plot_4_3")),
                plotOutput(ns("plot_4_4"))
                ),
              conditionalPanel(
                condition = "input.Nudata1 == 5 ",
                ns=ns,                
                plotOutput(ns("plot_5_1")),
                plotOutput(ns("plot_5_2")),
                plotOutput(ns("plot_5_3")),
                plotOutput(ns("plot_5_4")),
                plotOutput(ns("plot_5_5"))
                )
              # column(width = 6,plotOutput("plot2")),
              # column(width = 6,plotOutput("plot3")),
              # column(width = 6,plotOutput("plot4")),
              # column(width = 6,plotOutput("plot5"))
              ),
            conditionalPanel(
              condition = "input.performance_metrics==2",
              ns=ns,
              fluidRow(column(width = 5,
                              plotly::plotlyOutput(ns("plot21"))
                              ),
                    #                           column(width = 5,plotlyOutput("plot22")),
                    #                           column(width = 5,plotlyOutput("plot23")),
                    #                            column(width = 5,plotlyOutput("plot24")),
                    #                            column(width = 5,plotlyOutput("plot25"))
                    )
              )
            ),
          tabPanel(
            "VENN",
            conditionalPanel(
              condition = "input.Nudata1== 2 ", 
              ns=ns,
              plotOutput(ns("plot_venn2")), 
              DT::DTOutput(ns("info_venn2"))
              ),
            conditionalPanel(
              condition = "input.Nudata1== 3 ",
              ns=ns,
              plotOutput(ns("plot_venn3")), 
              DT::DTOutput(ns("info_venn3"))
              ),
            conditionalPanel(
              condition = "input.Nudata1== 4 ",
              ns=ns,
              plotOutput(ns("plot_venn4")),
              DT::DTOutput(ns("info_venn4"))
              ),
            conditionalPanel(
              condition = "input.Nudata1== 5 ",
              ns=ns,
              plotOutput(ns("plot_venn5")), 
              DT::DTOutput(ns("info_venn5"))
              )
            )
          )
        )
      )
      )
  )
  }

#' Individuals Server Function
#'
#' @noRd 
mod_Individuals_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    
#### 1 Data  
    
    filedata <- reactive({
      req(input$fileBcsv)
      fileInput <- load_file(input$fileBcsv$name, input$fileBcsv$datapath)
      fileInput <- as.data.frame(fileInput)
      return(list(fileInput = fileInput))
    })
    
    data_info <- reactive({
      req(filedata()$fileInput)
      Nrows <- nrow(filedata()$fileInput)
      Ncols <- ncol(filedata()$fileInput)
      SummaryData <- data.frame(list(N = c(Nrows, Ncols)))
      rownames(SummaryData) <- c("Nrows", "Ncols")
      list(SummaryData = SummaryData)
    })
    
    output$infotable <- DT::renderDataTable({
      df <- data_info()$SummaryData
      DT::datatable(df)
    })
    ########################################################
    
    #### 2 data
    filedata2_1 <- reactive({
      req(input$fileBcsv2_1)
      fileInput <- load_file(input$fileBcsv2_1$name, input$fileBcsv2_1$datapath)
      fileInput <- as.data.frame(fileInput)
      return(list(fileInput = fileInput))
    })
    
    data_info2_1 <- reactive({
      req(filedata2_1()$fileInput)
      Nrows <- nrow(filedata2_1()$fileInput)
      Ncols <- ncol(filedata2_1()$fileInput)
      SummaryData <- data.frame(list(N = c(Nrows, Ncols)))
      rownames(SummaryData) <- c("Nrows", "Ncols")
      list(SummaryData = SummaryData)
    })
    
    output$infotable2_1 <- DT::renderDataTable({
      df <- data_info2_1()$SummaryData
      DT::datatable(df)
    })
    
    ###
    filedata22 <- reactive({
      req(input$fileBcsv22)
      fileInput <- load_file(input$fileBcsv22$name, input$fileBcsv22$datapath)
      fileInput <- as.data.frame(fileInput)
      return(list(fileInput = fileInput))
    })
    
    data_info22 <- reactive({
      req(filedata22()$fileInput)
      Nrows <- nrow(filedata22()$fileInput)
      Ncols <- ncol(filedata22()$fileInput)
      SummaryData <- data.frame(list(N = c(Nrows, Ncols)))
      rownames(SummaryData) <- c("Nrows", "Ncols")
      list(SummaryData = SummaryData)
    })
    
    output$infotable2_2 <- DT::renderDataTable({
      df <- data_info22()$SummaryData
      DT::datatable(df)
    })
    
    #### 3 data  
    
    filedata31 <- reactive({
      req(input$fileBcsv31)
      fileInput <- load_file(input$fileBcsv31$name, input$fileBcsv31$datapath)
      fileInput <- as.data.frame(fileInput)
      return(list(fileInput = fileInput))
    })
    
    data_info31 <- reactive({
      req(filedata31()$fileInput)
      Nrows <- nrow(filedata31()$fileInput)
      Ncols <- ncol(filedata31()$fileInput)
      SummaryData <- data.frame(list(N = c(Nrows, Ncols)))
      rownames(SummaryData) <- c("Nrows", "Ncols")
      list(SummaryData = SummaryData)
    })
    
    output$infotable31 <- DT::renderDataTable({
      df <- data_info31()$SummaryData
      DT::datatable(df)
    })
  ##
    filedata32 <- reactive({
      req(input$fileBcsv32)
      fileInput <- load_file(input$fileBcsv32$name, input$fileBcsv32$datapath)
      fileInput <- as.data.frame(fileInput)
      return(list(fileInput = fileInput))
    })
    
    data_info32 <- reactive({
      req(filedata32()$fileInput)
      Nrows <- nrow(filedata32()$fileInput)
      Ncols <- ncol(filedata32()$fileInput)
      SummaryData <- data.frame(list(N = c(Nrows, Ncols)))
      rownames(SummaryData) <- c("Nrows", "Ncols")
      list(SummaryData = SummaryData)
    })
    
    output$infotable32 <- DT::renderDataTable({
      df <- data_info32()$SummaryData
      DT::datatable(df)
    })
  ##
    filedata33 <- reactive({
      req(input$fileBcsv33)
      fileInput <- load_file(input$fileBcsv33$name, input$fileBcsv33$datapath)
      fileInput <- as.data.frame(fileInput)
      return(list(fileInput = fileInput))
    })
    
    data_info33 <- reactive({
      req(filedata32()$fileInput)
      Nrows <- nrow(filedata33()$fileInput)
      Ncols <- ncol(filedata33()$fileInput)
      SummaryData <- data.frame(list(N = c(Nrows, Ncols)))
      rownames(SummaryData) <- c("Nrows", "Ncols")
      list(SummaryData = SummaryData)
    })
    
    output$infotable33 <- DT::renderDataTable({
      df <- data_info33()$SummaryData
      DT::datatable(df)
    })
    
  ########
    #### 4 data  
    
    filedata41 <- reactive({
      req(input$fileBcsv41)
      fileInput <- load_file(input$fileBcsv41$name, input$fileBcsv41$datapath)
      fileInput <- as.data.frame(fileInput)
      return(list(fileInput = fileInput))
    })
    
    data_info41 <- reactive({
      req(filedata41()$fileInput)
      Nrows <- nrow(filedata41()$fileInput)
      Ncols <- ncol(filedata41()$fileInput)
      SummaryData <- data.frame(list(N = c(Nrows, Ncols)))
      rownames(SummaryData) <- c("Nrows", "Ncols")
      list(SummaryData = SummaryData)
    })
    
    output$infotable41 <- DT::renderDataTable({
      df <- data_info41()$SummaryData
      DT::datatable(df)
    })
    ##
    filedata42 <- reactive({
      req(input$fileBcsv42)
      fileInput <- load_file(input$fileBcsv42$name, input$fileBcsv42$datapath)
      fileInput <- as.data.frame(fileInput)
      return(list(fileInput = fileInput))
    })
    
    data_info42 <- reactive({
      req(filedata42()$fileInput)
      Nrows <- nrow(filedata42()$fileInput)
      Ncols <- ncol(filedata42()$fileInput)
      SummaryData <- data.frame(list(N = c(Nrows, Ncols)))
      rownames(SummaryData) <- c("Nrows", "Ncols")
      list(SummaryData = SummaryData)
    })
    
    output$infotable42 <- DT::renderDataTable({
      df <- data_info42()$SummaryData
      DT::datatable(df)
    })
    ##
    filedata43 <- reactive({
      req(input$fileBcsv43)
      fileInput <- load_file(input$fileBcsv43$name, input$fileBcsv43$datapath)
      fileInput <- as.data.frame(fileInput)
      return(list(fileInput = fileInput))
    })
    
    data_info43 <- reactive({
      req(filedata43()$fileInput)
      Nrows <- nrow(filedata43()$fileInput)
      Ncols <- ncol(filedata43()$fileInput)
      SummaryData <- data.frame(list(N = c(Nrows, Ncols)))
      rownames(SummaryData) <- c("Nrows", "Ncols")
      list(SummaryData = SummaryData)
    })
    
    output$infotable43 <- DT::renderDataTable({
      df <- data_info43()$SummaryData
      DT::datatable(df)
    })
    
    ##
    filedata44 <- reactive({
      req(input$fileBcsv44)
      fileInput <- load_file(input$fileBcsv44$name, input$fileBcsv44$datapath)
      fileInput <- as.data.frame(fileInput)
      return(list(fileInput = fileInput))
    })
    
    data_info44 <- reactive({
      req(filedata44()$fileInput)
      Nrows <- nrow(filedata44()$fileInput)
      Ncols <- ncol(filedata44()$fileInput)
      SummaryData <- data.frame(list(N = c(Nrows, Ncols)))
      rownames(SummaryData) <- c("Nrows", "Ncols")
      list(SummaryData = SummaryData)
    })
    
    output$infotable44 <- DT::renderDataTable({
      df <- data_info44()$SummaryData
      DT::datatable(df)
    })
    
    ########
    #### 5 data  
    
    filedata51 <- reactive({
      req(input$fileBcsv51)
      fileInput <- load_file(input$fileBcsv51$name, input$fileBcsv51$datapath)
      fileInput <- as.data.frame(fileInput)
      return(list(fileInput = fileInput))
    })
    
    data_info51 <- reactive({
      req(filedata51()$fileInput)
      Nrows <- nrow(filedata51()$fileInput)
      Ncols <- ncol(filedata51()$fileInput)
      SummaryData <- data.frame(list(N = c(Nrows, Ncols)))
      rownames(SummaryData) <- c("Nrows", "Ncols")
      list(SummaryData = SummaryData)
    })
    
    output$infotable51 <- DT::renderDataTable({
      df <- data_info51()$SummaryData
      DT::datatable(df)
    })
    ##
    
    filedata52 <- reactive({
      req(input$fileBcsv52)
      fileInput <- load_file(input$fileBcsv52$name, input$fileBcsv52$datapath)
      fileInput <- as.data.frame(fileInput)
      return(list(fileInput = fileInput))
    })
    
    data_info52 <- reactive({
      req(filedata52()$fileInput)
      Nrows <- nrow(filedata52()$fileInput)
      Ncols <- ncol(filedata52()$fileInput)
      SummaryData <- data.frame(list(N = c(Nrows, Ncols)))
      rownames(SummaryData) <- c("Nrows", "Ncols")
      list(SummaryData = SummaryData)
    })
    
    output$infotable52 <- DT::renderDataTable({
      df <- data_info52()$SummaryData
      DT::datatable(df)
    })
    ##
    
    filedata53 <- reactive({
      req(input$fileBcsv53)
      fileInput <- load_file(input$fileBcsv53$name, input$fileBcsv53$datapath)
      fileInput <- as.data.frame(fileInput)
      return(list(fileInput = fileInput))
    })
    
    data_info53 <- reactive({
      req(filedata53()$fileInput)
      Nrows <- nrow(filedata53()$fileInput)
      Ncols <- ncol(filedata53()$fileInput)
      SummaryData <- data.frame(list(N = c(Nrows, Ncols)))
      rownames(SummaryData) <- c("Nrows", "Ncols")
      list(SummaryData = SummaryData)
    })
    
    output$infotable53 <- DT::renderDataTable({
      df <- data_info53()$SummaryData
      DT::datatable(df)
    })
    
    ##
    
    filedata54 <- reactive({
      req(input$fileBcsv54)
      fileInput <- load_file(input$fileBcsv54$name, input$fileBcsv54$datapath)
      fileInput <- as.data.frame(fileInput)
      return(list(fileInput = fileInput))
    })
    
    data_info54 <- reactive({
      req(filedata54()$fileInput)
      Nrows <- nrow(filedata54()$fileInput)
      Ncols <- ncol(filedata54()$fileInput)
      SummaryData <- data.frame(list(N = c(Nrows, Ncols)))
      rownames(SummaryData) <- c("Nrows", "Ncols")
      list(SummaryData = SummaryData)
    })
    
    output$infotable54 <- DT::renderDataTable({
      df <- data_info54()$SummaryData
      DT::datatable(df)
    })
    ##
    
    filedata55 <- reactive({
      req(input$fileBcsv55)
      fileInput <- load_file(input$fileBcsv55$name, input$fileBcsv55$datapath)
      fileInput <- as.data.frame(fileInput)
      return(list(fileInput = fileInput))
    })
    
    data_info55 <- reactive({
      req(filedata55()$fileInput)
      Nrows <- nrow(filedata55()$fileInput)
      Ncols <- ncol(filedata55()$fileInput)
      SummaryData <- data.frame(list(N = c(Nrows, Ncols)))
      rownames(SummaryData) <- c("Nrows", "Ncols")
      list(SummaryData = SummaryData)
    })
    
    output$infotable55 <- DT::renderDataTable({
      df <- data_info55()$SummaryData
      DT::datatable(df)
    })
    
    
    
    
    
    
  #### Data 2 to 3 PM
    filedata21 <- reactive({
    req(input$fileBcsv21)
    fileInput <- load_file(input$fileBcsv21$name, input$fileBcsv21$datapath)
    fileInput <- as.data.frame(fileInput)
    return(list(fileInput = fileInput))
  })

  data_info21 <- reactive({
    req(filedata21()$fileInput)
    Nrows <- nrow(filedata21()$fileInput)
    Ncols <- ncol(filedata21()$fileInput)
    SummaryData <- data.frame(list(N = c(Nrows, Ncols)))
    rownames(SummaryData) <- c("Nrows", "Ncols")
    list(SummaryData = SummaryData)
  })

  output$infotable21 <- DT::renderDataTable({
    df <- data_info21()$SummaryData
    DT::datatable(df)
  })
  
  #################
  Stady1df <- eventReactive(input$button1,{
      if(input$performance_metrics == 1) {
        if(input$Nudata1 == 1) {
        
        wnv <- as.data.frame(filedata()$fileInput)
        NF=as.numeric(input$NFro1)
        measurePM1 = input$PMs1
        measurePM2 = input$PMs2
        q1=input$ValueqPM1
        q2=input$ValueqPM2
        
        outEval <- mco_one_diseases(data = wnv, NF = NF, measurePM1 = measurePM1, 
                                    measurePM2 = measurePM2,
                                    q1 = q1, 
                                    q2 = q2)
        finalEval <- outEval$final2
        dataPlot <- data_build(f1 = outEval$f1, X = outEval$X, Y = outEval$Y)
        return(list(final = finalEval, dataPlot = dataPlot))
        }
      }
  })
    
######    
    
    Stady2_1df <- eventReactive(input$button2,{
      if(input$performance_metrics == 1) {
        if(input$Nudata1 == 2) {
          
          wnv <- as.data.frame(filedata2_1()$fileInput)
          NF=as.numeric(input$NFro1)
          measurePM1 = input$PMs1
          measurePM2 = input$PMs2
          q1=input$ValueqPM1
          q2=input$ValueqPM2
          
          outEval <- mco_one_diseases(data = wnv, NF = NF, measurePM1 = measurePM1, 
                                      measurePM2 = measurePM2,
                                      q1 = q1, 
                                      q2 = q2)
          finalEval <- outEval$final2
          dataPlot <- data_build(f1 = outEval$f1, X = outEval$X, Y = outEval$Y)
          return(list(final = finalEval, dataPlot = dataPlot))
        }
      }
    })
    
    Stady2_2df <- eventReactive(input$button2,{
      if(input$performance_metrics == 1) {
        if(input$Nudata1 == 2) {
          
          wnv <- as.data.frame(filedata22()$fileInput)
          NF=as.numeric(input$NFro1)
          measurePM1 = input$PMs1
          measurePM2 = input$PMs2
          q1=input$ValueqPM1
          q2=input$ValueqPM2
          
          outEval <- mco_one_diseases(data = wnv, 
                                      NF = NF, 
                                      measurePM1 = measurePM1, 
                                      measurePM2 = measurePM2,
                                      q1 = q1, 
                                      q2 = q2)
          finalEval <- outEval$final2
          dataPlot <- data_build(f1 = outEval$f1, X = outEval$X, Y = outEval$Y)
          return(list(final = finalEval, dataPlot = dataPlot))
        }
      }
    })
  ########  
    Stady31df <- eventReactive(input$button3,{
      if(input$performance_metrics == 1) {
        if(input$Nudata1 == 3) {
          
          wnv <- as.data.frame(filedata31()$fileInput)
          NF=as.numeric(input$NFro1)
          measurePM1 = input$PMs1
          measurePM2 = input$PMs2
          q1=input$ValueqPM1
          q2=input$ValueqPM2
          
          outEval <- mco_one_diseases(data = wnv,
                                      NF = NF, 
                                      measurePM1 = measurePM1, 
                                      measurePM2 = measurePM2,
                                      q1 = q1, 
                                      q2 = q2)
          finalEval <- outEval$final2
          dataPlot <- data_build(f1 = outEval$f1, X = outEval$X, Y = outEval$Y)
          return(list(final = finalEval, dataPlot = dataPlot))
        }
      }
    })
    
    ##
    
    Stady32df <- eventReactive(input$button3,{
      if(input$performance_metrics == 1) {
        if(input$Nudata1 == 3) {
          
          wnv <- as.data.frame(filedata32()$fileInput)
          NF=as.numeric(input$NFro1)
          measurePM1 = input$PMs1
          measurePM2 = input$PMs2
          q1=input$ValueqPM1
          q2=input$ValueqPM2
          
          outEval <- mco_one_diseases(data = wnv, 
                                      NF = NF, measurePM1 = measurePM1, 
                                      measurePM2 = measurePM2,
                                      q1 = q1, 
                                      q2 = q2)
          finalEval <- outEval$final2
          dataPlot <- data_build(f1 = outEval$f1,
                                 X = outEval$X, 
                                 Y = outEval$Y)
          return(list(final = finalEval, dataPlot = dataPlot))
        }
      }
    })
    
    ##
    
    Stady33df <- eventReactive(input$button3,{
      if(input$performance_metrics == 1) {
        if(input$Nudata1 == 3) {
          
          wnv <- as.data.frame(filedata33()$fileInput)
          NF=as.numeric(input$NFro1)
          measurePM1 = input$PMs1
          measurePM2 = input$PMs2
          q1=input$ValueqPM1
          q2=input$ValueqPM2
          
          outEval <- mco_one_diseases(data = wnv,
                                      NF = NF, 
                                      measurePM1 = measurePM1, 
                                      measurePM2 = measurePM2,
                                      q1 = q1, 
                                      q2 = q2)
          finalEval <- outEval$final2
          dataPlot <- data_build(f1 = outEval$f1, X = outEval$X, Y = outEval$Y)
          return(list(final = finalEval, dataPlot = dataPlot))
        }
      }
    })
    
    #######
    ###
    Stady41df <- eventReactive(input$button4,{
      if(input$performance_metrics == 1) {
        if(input$Nudata1 == 4) {
          
          wnv <- as.data.frame(filedata41()$fileInput)
          NF=as.numeric(input$NFro1)
          measurePM1 = input$PMs1
          measurePM2 = input$PMs2
          q1=input$ValueqPM1
          q2=input$ValueqPM2
          
          outEval <- mco_one_diseases(data = wnv, 
                                      NF = NF, 
                                      measurePM1 = measurePM1, 
                                      measurePM2 = measurePM2,
                                      q1 = q1, 
                                      q2 = q2)
          finalEval <- outEval$final2
          dataPlot <- data_build(f1 = outEval$f1, X = outEval$X, Y = outEval$Y)
          return(list(final = finalEval, dataPlot = dataPlot))
        }
      }
    })
    
    Stady42df <- eventReactive(input$button4,{
      if(input$performance_metrics == 1) {
        if(input$Nudata1 == 4) {
          
          wnv <- as.data.frame(filedata42()$fileInput)
          NF=as.numeric(input$NFro1)
          measurePM1 = input$PMs1
          measurePM2 = input$PMs2
          q1=input$ValueqPM1
          q2=input$ValueqPM2
          
          outEval <- mco_one_diseases(data = wnv, 
                                      NF = NF, 
                                      measurePM1 = measurePM1, 
                                      measurePM2 = measurePM2,
                                      q1 = q1, 
                                      q2 = q2)
          finalEval <- outEval$final2
          dataPlot <- data_build(f1 = outEval$f1, X = outEval$X, Y = outEval$Y)
          return(list(final = finalEval, dataPlot = dataPlot))
        }
      }
    })
    
    Stady43df <- eventReactive(input$button4,{
      if(input$performance_metrics == 1) {
        if(input$Nudata1 == 4) {
          
          wnv <- as.data.frame(filedata43()$fileInput)
          NF=as.numeric(input$NFro1)
          measurePM1 = input$PMs1
          measurePM2 = input$PMs2
          q1=input$ValueqPM1
          q2=input$ValueqPM2
          
          outEval <- mco_one_diseases(data = wnv, 
                                      NF = NF, 
                                      measurePM1 = measurePM1, 
                                      measurePM2 = measurePM2,
                                      q1 = q1, 
                                      q2 = q2)
          finalEval <- outEval$final2
          dataPlot <- data_build(f1 = outEval$f1, X = outEval$X, Y = outEval$Y)
          return(list(final = finalEval, dataPlot = dataPlot))
        }
      }
    })
    
    Stady44df <- eventReactive(input$button4,{
      if(input$performance_metrics == 1) {
        if(input$Nudata1 == 4) {
          
          wnv <- as.data.frame(filedata44()$fileInput)
          NF=as.numeric(input$NFro1)
          measurePM1 = input$PMs1
          measurePM2 = input$PMs2
          q1=input$ValueqPM1
          q2=input$ValueqPM2
          
          outEval <- mco_one_diseases(data = wnv, 
                                      NF = NF, 
                                      measurePM1 = measurePM1, 
                                      measurePM2 = measurePM2,
                                      q1 = q1, 
                                      q2 = q2)
          finalEval <- outEval$final2
          dataPlot <- data_build(f1 = outEval$f1, X = outEval$X, Y = outEval$Y)
          return(list(final = finalEval, dataPlot = dataPlot))
        }
      }
    })
    
    #######
    ###
    Stady51df <- eventReactive(input$button5,{
      if(input$performance_metrics == 1) {
        if(input$Nudata1 == 5) {
          
          wnv <- as.data.frame(filedata51()$fileInput)
          NF=as.numeric(input$NFro1)
          measurePM1 = input$PMs1
          measurePM2 = input$PMs2
          q1=input$ValueqPM1
          q2=input$ValueqPM2
          
          outEval <- mco_one_diseases(data = wnv, 
                                      NF = NF, 
                                      measurePM1 = measurePM1, 
                                      measurePM2 = measurePM2,
                                      q1 = q1, 
                                      q2 = q2)
          finalEval <- outEval$final2
          dataPlot <- data_build(f1 = outEval$f1, X = outEval$X, Y = outEval$Y)
          return(list(final = finalEval, dataPlot = dataPlot))
        }
      }
    })
    
    Stady52df <- eventReactive(input$button5,{
      if(input$performance_metrics == 1) {
        if(input$Nudata1 == 5) {
          
          wnv <- as.data.frame(filedata52()$fileInput)
          NF=as.numeric(input$NFro1)
          measurePM1 = input$PMs1
          measurePM2 = input$PMs2
          q1=input$ValueqPM1
          q2=input$ValueqPM2
          
          outEval <- mco_one_diseases(data = wnv, 
                                      NF = NF, 
                                      measurePM1 = measurePM1, 
                                      measurePM2 = measurePM2,
                                      q1 = q1, 
                                      q2 = q2)
          finalEval <- outEval$final2
          dataPlot <- data_build(f1 = outEval$f1, X = outEval$X, Y = outEval$Y)
          return(list(final = finalEval, dataPlot = dataPlot))
        }
      }
    })
    
    Stady53df <- eventReactive(input$button5,{
      if(input$performance_metrics == 1) {
        if(input$Nudata1 == 5) {
          
          wnv <- as.data.frame(filedata53()$fileInput)
          NF=as.numeric(input$NFro1)
          measurePM1 = input$PMs1
          measurePM2 = input$PMs2
          q1=input$ValueqPM1
          q2=input$ValueqPM2
          
          outEval <- mco_one_diseases(data = wnv,
                                      NF = NF, 
                                      measurePM1 = measurePM1, 
                                      measurePM2 = measurePM2,
                                      q1 = q1, 
                                      q2 = q2)
          finalEval <- outEval$final2
          dataPlot <- data_build(f1 = outEval$f1, X = outEval$X, Y = outEval$Y)
          return(list(final = finalEval, dataPlot = dataPlot))
        }
      }
    })
    
    Stady54df <- eventReactive(input$button5,{
      if(input$performance_metrics == 1) {
        if(input$Nudata1 == 5) {
          
          wnv <- as.data.frame(filedata54()$fileInput)
          NF=as.numeric(input$NFro1)
          measurePM1 = input$PMs1
          measurePM2 = input$PMs2
          q1=input$ValueqPM1
          q2=input$ValueqPM2
          
          outEval <- mco_one_diseases(data = wnv,
                                      NF = NF, 
                                      measurePM1 = measurePM1, 
                                      measurePM2 = measurePM2,
                                      q1 = q1, 
                                      q2 = q2)
          finalEval <- outEval$final2
          dataPlot <- data_build(f1 = outEval$f1, X = outEval$X, Y = outEval$Y)
          return(list(final = finalEval, dataPlot = dataPlot))
        }
      }
    })
    
    Stady55df <- eventReactive(input$button5,{
      if(input$performance_metrics == 1) {
        if(input$Nudata1 == 5) {
          
          wnv <- as.data.frame(filedata55()$fileInput)
          NF=as.numeric(input$NFro1)
          measurePM1 = input$PMs1
          measurePM2 = input$PMs2
          q1=input$ValueqPM1
          q2=input$ValueqPM2
          
          outEval <- mco_one_diseases(data = wnv,
                                      NF = NF, 
                                      measurePM1 = measurePM1, 
                                      measurePM2 = measurePM2,
                                      q1 = q1, 
                                      q2 = q2)
          finalEval <- outEval$final2
          dataPlot <- data_build(f1 = outEval$f1, X = outEval$X, Y = outEval$Y)
          return(list(final = finalEval, dataPlot = dataPlot))
        }
      }
    })
    
    
    
    ######### Venn 2 datas
    
    grap_venn2 <- reactive({
      genes = merge(Stady2_1df()$final,
                    Stady2_2df()$final, by = "Gene", all = TRUE) 
      genes$Frontier.x[!is.na(genes$Frontier.x)] <- 1
      genes$Frontier.y[!is.na(genes$Frontier.y)] <- 1
      genes[is.na(genes)] <- 0
      c=2
      t=3
      genes$count=rowSums(genes[c:t])
      grap <- Venn(Data=genes,c=c,t=t)$Venn
      return(list(grap=grap,genes=genes))
    })
    
    
    output$plot_venn2 <- renderPlot({
      grap_venn2()$grap
    })
    
    output$info_venn2 <- DT::renderDataTable({
      df <- as.data.frame(grap_venn2()$genes)
      DT::datatable(df)
    })
    ######### Venn 3 datas
    
    grap_venn3 <- reactive({
      c = 2
      t = 4
      genes = merge(Stady31df()$final, 
                    Stady32df()$final, by = "Gene", all = TRUE) 
      genes2 = merge(genes,Stady33df()$final, by = "Gene", all = TRUE  )
      
      genes2$Frontier.x[!is.na(genes2$Frontier.x)] <- 1
      genes2$Frontier.y[!is.na(genes2$Frontier.y)] <- 1
      genes2$Frontier[!is.na(genes2$Frontier)] <- 1
      genes2[is.na(genes2)] <- 0
      
      genes2$Count=rowSums(genes2[c:t])
      
      colnames(genes2)= c("Gene", "Study1", "Study2", "Study3", "Count")
      
      venn3= as.data.frame(genes2)
      #venn(venn3, ilab=TRUE, zcolor = "style", ilcs = 1.2, sncs = 1.2)
     
      grap <- Venn(Data=venn3,c=c,t=t)$Venn
      return(list(grap=grap,genes2=genes2))
    })
    
    
    output$plot_venn3 <- renderPlot({
      grap_venn3()$grap
    })
    
    output$info_venn3 <- DT::renderDataTable({
      df <- as.data.frame(grap_venn3()$genes2)
      DT::datatable(df)
    })
    
    ######### Venn 4 datas
    
    grap_venn4 <- reactive({
      c = 2
      t = 5
      genes = merge(Stady41df()$final, 
                    Stady42df()$final, by = "Gene", all = TRUE) 
      genes2 = merge(genes,Stady43df()$final, by = "Gene", all = TRUE  )
      genes3 = merge(genes2,Stady44df()$final, by = "Gene", all = TRUE  )
      
      colnames(genes3)= c("Gene", "Study1", "Study2", "Study3", "Study4")
      genes3$Study1[!is.na(genes3$Study1)] <- 1
      genes3$Study2[!is.na(genes3$Study2)] <- 1
      genes3$Study3[!is.na(genes3$Study3)] <- 1
      genes3$Study4[!is.na(genes3$Study4)] <- 1
      genes3[is.na(genes3)] <- 0
      
      genes3$Count=rowSums(genes3[c:t])
      
      venn4 = as.data.frame(genes3)
      
      grap <- Venn(Data=venn4,c=c,t=t)$Venn
      return(list(grap=grap,genes3=genes3))
    })
    
    
    output$plot_venn4 <- renderPlot({
      grap_venn4()$grap
    })
    
    output$info_venn4 <- DT::renderDataTable({
      df <- as.data.frame(grap_venn4()$genes3)
      DT::datatable(df)
    })
    
    ######### Venn 5 datas
    
    grap_venn5 <- reactive({
      c = 2
      t = 6
      genes = merge(Stady51df()$final, 
                    Stady52df()$final, by = "Gene", all = TRUE) 
      genes2 = merge(genes,Stady53df()$final, by = "Gene", all = TRUE  )
      genes3 = merge(genes2,Stady54df()$final, by = "Gene", all = TRUE  )
      genes4 = merge(genes3,Stady55df()$final, by = "Gene", all = TRUE  )
      
      colnames(genes4)= c("Gene", "Study1", "Study2", "Study3", "Study4", "Study5")
      
      genes4$Study1[!is.na(genes4$Study1)] <- 1
      genes4$Study2[!is.na(genes4$Study2)] <- 1
      genes4$Study3[!is.na(genes4$Study3)] <- 1
      genes4$Study4[!is.na(genes4$Study4)] <- 1
      genes4$Study5[!is.na(genes4$Study5)] <- 1
      genes4[is.na(genes4)] <- 0
      
      genes4$Count=rowSums(genes4[c:t])
      venn5= as.data.frame(genes4)
      
      
      grap <- Venn(Data=venn5,c=c,t=t)$Venn
      return(list(grap=grap,genes4=genes4))
    })
    
    
    output$plot_venn5 <- renderPlot({
      grap_venn5()$grap
    })
    
    output$info_venn5 <- DT::renderDataTable({
      df <- as.data.frame(grap_venn5()$genes4)
      DT::datatable(df)
    })
    
    
    
    
 #############################   
    
      
      Stady1df2 <- eventReactive(input$button21,{
      
      if(input$performance_metrics == 2) {
        
        wnv <- as.data.frame(filedata21()$fileInput)
        NF=as.numeric(input$NFro21)
        measurePMs31 = input$PMs31
        measurePMs32 = input$PMs32
        measurePMs33 = input$PMs33
        q1=input$ValueqPM31
        q2=input$ValueqPM32
        q3=input$ValueqPM33
        
        outEval <- mco_one_diseases_3PM(data = wnv, 
                                        NF=NF,
                                        measurePMs31 = measurePMs31,
                                        measurePMs32 = measurePMs32, 
                                        measurePMs33 = measurePMs33, 
                                        q1 = q1,
                                        q2 = q2, 
                                        q3 = q3)
        finalEval <- outEval$final2
        dataPlot <- data_build2(f1 = outEval$f1, 
                                X = outEval$X,
                                Y = outEval$Y,
                                Z = outEval$Z)
        return(list(final = finalEval,dataPlot = dataPlot))
      }
      
    })
    
    ########
    
    output$table1 <- DT::renderDataTable({
      if(input$performance_metrics == 1){
        df <- as.data.frame(Stady1df()$final)
        DT::datatable(df)
      }
    })
    ##########
    
    output$table2_1 <- DT::renderDataTable({
      if(input$performance_metrics == 1){
        df <- as.data.frame(Stady2_1df()$final)
        DT::datatable(df)
      }
    })
    
    output$table2_2 <- DT::renderDataTable({
      if(input$performance_metrics == 1){
        df <- as.data.frame(Stady2_2df()$final)
        DT::datatable(df)
      }
    })
    #################
    
    output$table31 <- DT::renderDataTable({
      if(input$performance_metrics == 1){
        df <- as.data.frame(Stady31df()$final)
        DT::datatable(df)
      }
    })
    
    output$table32 <- DT::renderDataTable({
      if(input$performance_metrics == 1){
        df <- as.data.frame(Stady32df()$final)
        DT::datatable(df)
      }
    })
    
    output$table33 <- DT::renderDataTable({
      if(input$performance_metrics == 1){
        df <- as.data.frame(Stady33df()$final)
        DT::datatable(df)
      }
    })
    
    #################
    
    output$table41 <- DT::renderDataTable({
      if(input$performance_metrics == 1){
        df <- as.data.frame(Stady41df()$final)
        DT::datatable(df)
      }
    })
    
    output$table42 <- DT::renderDataTable({
      if(input$performance_metrics == 1){
        df <- as.data.frame(Stady42df()$final)
        DT::datatable(df)
      }
    })
    
    output$table43 <- DT::renderDataTable({
      if(input$performance_metrics == 1){
        df <- as.data.frame(Stady43df()$final)
        DT::datatable(df)
      }
    })
    output$table44 <- DT::renderDataTable({
      if(input$performance_metrics == 1){
        df <- as.data.frame(Stady44df()$final)
        DT::datatable(df)
      }
    })
    
    #################
    
    output$table51 <- DT::renderDataTable({
      if(input$performance_metrics == 1){
        df <- as.data.frame(Stady51df()$final)
        DT::datatable(df)
      }
    })
    
    output$table52 <- DT::renderDataTable({
      if(input$performance_metrics == 1){
        df <- as.data.frame(Stady52df()$final)
        DT::datatable(df)
      }
    })
    
    output$table53 <- DT::renderDataTable({
      if(input$performance_metrics == 1){
        df <- as.data.frame(Stady53df()$final)
        DT::datatable(df)
      }
    })
    output$table54 <- DT::renderDataTable({
      if(input$performance_metrics == 1){
        df <- as.data.frame(Stady54df()$final)
        DT::datatable(df)
      }
    })
    output$table55 <- DT::renderDataTable({
      if(input$performance_metrics == 1){
        df <- as.data.frame(Stady55df()$final)
        DT::datatable(df)
      }
    })
    
    
    
 ########### 3 PM   
    output$table21 <- DT::renderDataTable({
      if(input$performance_metrics == 2){
        df <- as.data.frame(Stady1df2()$final)
        DT::datatable(df)
      }
    })
    
   ####### 
    
    plot1_obj <- reactive({
      if(input$performance_metrics == 1){
        df <- Stady1df()$dataPlot
        p <- ggplot2::ggplot(df, 
                             ggplot2::aes(X,
                                          Y, 
                                          group = as.factor(etiq),
                                          color=as.factor(etiq))) +
          ggplot2::geom_point() +
          ggplot2::labs(x = "PM1:Median", y = "PM2:Mean") +
          ggplot2::geom_line(ggplot2::aes(linetype=as.factor(etiq)),
                             show.legend = FALSE ) +
          ggplot2::theme(legend.position = "none") + 
          ggplot2::guides(fill=FALSE, color=FALSE) +
          ggplot2::theme_bw()
        return(p)
      }
      
    })
    output$plot11 <- renderPlot({
      plot1_obj()
    })
    ########
    plot21_obj <- reactive({
      if(input$performance_metrics == 1){
        df <- Stady2_1df()$dataPlot
        p <- ggplot2::ggplot(df, ggplot2::aes(X, 
                                              Y, 
                                              group = as.factor(etiq),
                                              color=as.factor(etiq))) +
          ggplot2::geom_point() +
          ggplot2::labs(x = "PM1:Median", y = "PM2:Mean") +
          ggplot2::geom_line(ggplot2::aes(linetype=as.factor(etiq)),show.legend = FALSE ) +
          ggplot2::theme(legend.position = "none") + 
          ggplot2::guides(fill=FALSE, color=FALSE) +
          ggplot2::theme_bw()
        return(p)
      }
    })
    
    output$plot_2_1 <- renderPlot({
      plot21_obj()
    })
    
    plot22_obj <- reactive({
      if(input$performance_metrics == 1){
        df <- Stady2_2df()$dataPlot
        p <- ggplot2::ggplot(df, ggplot2::aes(X, 
                                              Y,
                                              group = as.factor(etiq),
                                              color=as.factor(etiq))) +
          ggplot2::geom_point() +
          ggplot2::labs(x = "PM1:Median", y = "PM2:Mean") +
          ggplot2::geom_line(
            ggplot2::aes(linetype=as.factor(etiq)),show.legend = FALSE ) +
          ggplot2::theme(legend.position = "none") +
          ggplot2::guides(fill=FALSE, color=FALSE) +
          ggplot2::theme_bw()
        return(p)
      }
    })
    
    output$plot_2_2 <- renderPlot({
      plot22_obj()
    })
    #######
    plot31_obj <- reactive({
      if(input$performance_metrics == 1){
        df <- Stady31df()$dataPlot
        p <- ggplot2::ggplot(df, ggplot2::aes(X,
                                              Y, 
                                              group = as.factor(etiq),
                                              color=as.factor(etiq))) +
          ggplot2::geom_point() +
          ggplot2::labs(x = "PM1:Median", y = "PM2:Mean") +
          ggplot2::geom_line(
            ggplot2::aes(linetype=as.factor(etiq)),show.legend = FALSE ) +
          ggplot2::theme(legend.position = "none") +
          ggplot2::guides(fill=FALSE, color=FALSE) +
          ggplot2::theme_bw()
        return(p)
      }
    })
    
    output$plot_3_1 <- renderPlot({
      plot31_obj()
    })
    
    plot32_obj <- reactive({
      if(input$performance_metrics == 1){
        df <- Stady32df()$dataPlot
        p <- ggplot2::ggplot(df, ggplot2::aes(X, 
                                              Y, 
                                              group = as.factor(etiq),
                                              color=as.factor(etiq))) +
          ggplot2::geom_point() +
          ggplot2::labs(x = "PM1:Median", y = "PM2:Mean") +
          ggplot2::geom_line(
            ggplot2::aes(linetype=as.factor(etiq)),show.legend = FALSE ) +
          ggplot2::theme(legend.position = "none") + 
          ggplot2::guides(fill=FALSE, color=FALSE) +
          ggplot2::theme_bw()
        return(p)
      }
    })
    
    output$plot_3_2 <- renderPlot({
      plot32_obj()
    })
    
    plot33_obj <- reactive({
      if(input$performance_metrics == 1){
        df <- Stady33df()$dataPlot
        p <- ggplot2::ggplot(df, ggplot2::aes(X,
                                              Y, 
                                              group = as.factor(etiq),
                                              color=as.factor(etiq))) +
          ggplot2::geom_point() +
          ggplot2::labs(x = "PM1:Median", y = "PM2:Mean") +
          ggplot2::geom_line(
            ggplot2::aes(linetype=as.factor(etiq)),show.legend = FALSE ) +
          ggplot2::theme(legend.position = "none") + 
          ggplot2::guides(fill=FALSE, color=FALSE) +
          ggplot2::theme_bw()
        return(p)
      }
    })
    
    output$plot_3_3 <- renderPlot({
      plot33_obj()
    })
    
    #######
    plot41_obj <- reactive({
      if(input$performance_metrics == 1){
        df <- Stady41df()$dataPlot
        p <- ggplot2::ggplot(df, ggplot2::aes(X,
                                              Y, 
                                              group = as.factor(etiq),
                                              color=as.factor(etiq))) +
          ggplot2::geom_point() +
          ggplot2::labs(x = "PM1:Median", y = "PM2:Mean") +
          ggplot2::geom_line(
            ggplot2::aes(linetype=as.factor(etiq)),show.legend = FALSE ) +
          ggplot2::theme(legend.position = "none") + 
          ggplot2::guides(fill=FALSE, color=FALSE) +
          ggplot2::theme_bw()
        return(p)
      }
    })
    
    output$plot_4_1 <- renderPlot({
      plot41_obj()
    })
    
    plot42_obj <- reactive({
      if(input$performance_metrics == 1){
        df <- Stady42df()$dataPlot
        p <- ggplot2::ggplot(df, ggplot2::aes(X,
                                              Y,
                                              group = as.factor(etiq),
                                              color=as.factor(etiq))) +
          ggplot2::geom_point() +
          ggplot2::labs(x = "PM1:Median", y = "PM2:Mean") +
          ggplot2::geom_line(
            ggplot2::aes(linetype=as.factor(etiq)),show.legend = FALSE ) +
          ggplot2::theme(legend.position = "none") +
          ggplot2::guides(fill=FALSE, color=FALSE) +
          ggplot2::theme_bw()
        return(p)
      }
    })
    
    output$plot_4_2 <- renderPlot({
      plot42_obj()
    })
    
    plot43_obj <- reactive({
      if(input$performance_metrics == 1){
        df <- Stady43df()$dataPlot
        p <- ggplot2::ggplot(df, ggplot2::aes(X,
                                              Y,
                                              group = as.factor(etiq),
                                              color=as.factor(etiq))) +
          ggplot2::geom_point() +
          ggplot2::labs(x = "PM1:Median", y = "PM2:Mean") +
          ggplot2::geom_line(
            ggplot2::aes(linetype=as.factor(etiq)),show.legend = FALSE ) +
          ggplot2::theme(legend.position = "none") + 
          ggplot2::guides(fill=FALSE, color=FALSE) +
          ggplot2::theme_bw()
        return(p)
      }
    })
    
    output$plot_4_3 <- renderPlot({
      plot43_obj()
    })
    
    plot44_obj <- reactive({
      if(input$performance_metrics == 1){
        df <- Stady44df()$dataPlot
        p <- ggplot2::ggplot(df, ggplot2::aes(X,
                                              Y,
                                              group = as.factor(etiq),
                                              color=as.factor(etiq))) +
          ggplot2::geom_point() +
          ggplot2::labs(x = "PM1:Median", y = "PM2:Mean") +
          ggplot2::geom_line(
            ggplot2::aes(linetype=as.factor(etiq)),show.legend = FALSE ) +
          ggplot2::theme(legend.position = "none") + 
          ggplot2::guides(fill=FALSE, color=FALSE) +
          ggplot2::theme_bw()
        return(p)
      }
    })
    
    output$plot_4_4 <- renderPlot({
      plot44_obj()
    })
    #######
    plot51_obj <- reactive({
      if(input$performance_metrics == 1){
        df <- Stady51df()$dataPlot
        p <- ggplot2::ggplot(df, ggplot2::aes(X,
                                              Y,
                                              group = as.factor(etiq),
                                              color=as.factor(etiq))) +
          ggplot2::geom_point() +
          ggplot2::labs(x = "PM1:Median", y = "PM2:Mean") +
          ggplot2::geom_line(
            ggplot2::aes(linetype=as.factor(etiq)),show.legend = FALSE ) +
          ggplot2::theme(legend.position = "none") + 
          ggplot2::guides(fill=FALSE, color=FALSE) +
          ggplot2::theme_bw()
        return(p)
      }
    })
    
    output$plot_5_1 <- renderPlot({
      plot51_obj()
    })
    
    plot52_obj <- reactive({
      if(input$performance_metrics == 1){
        df <- Stady52df()$dataPlot
        p <- ggplot2::ggplot(df, ggplot2::aes(X,
                                              Y,
                                              group = as.factor(etiq),
                                              color=as.factor(etiq))) +
          ggplot2::geom_point() +
          ggplot2::labs(x = "PM1:Median", y = "PM2:Mean") +
          ggplot2::geom_line(
            ggplot2::aes(linetype=as.factor(etiq)),show.legend = FALSE ) +
          ggplot2::theme(legend.position = "none") + 
          ggplot2::guides(fill=FALSE, color=FALSE) +
          ggplot2::theme_bw()
        return(p)
      }
    })
    
    output$plot_5_2 <- renderPlot({
      plot52_obj()
    })
    
    plot53_obj <- reactive({
      if(input$performance_metrics == 1){
        df <- Stady53df()$dataPlot
        p <- ggplot2::ggplot(df, ggplot2::aes(X,
                                              Y,
                                              group = as.factor(etiq),
                                              color=as.factor(etiq))) +
          ggplot2::geom_point() +
          ggplot2::labs(x = "PM1:Median", y = "PM2:Mean") +
          ggplot2::geom_line(
            ggplot2::aes(linetype=as.factor(etiq)),show.legend = FALSE ) +
          ggplot2::theme(legend.position = "none") + 
          ggplot2::guides(fill=FALSE, color=FALSE) +
          ggplot2::theme_bw()
        return(p)
      }
    })
    
    output$plot_5_3 <- renderPlot({
      plot53_obj()
    })
    
    plot54_obj <- reactive({
      if(input$performance_metrics == 1){
        df <- Stady54df()$dataPlot
        p <- ggplot2::ggplot(df, ggplot2::aes(X,
                                              Y,
                                              group = as.factor(etiq),
                                              color=as.factor(etiq))) +
          ggplot2::geom_point() +
          ggplot2::labs(x = "PM1:Median", y = "PM2:Mean") +
          ggplot2::geom_line(
            ggplot2::aes(linetype=as.factor(etiq)),show.legend = FALSE ) +
          ggplot2::theme(legend.position = "none") +
          ggplot2::guides(fill=FALSE, color=FALSE) +
          ggplot2::theme_bw()
        return(p)
      }
    })
    
    output$plot_5_4 <- renderPlot({
      plot54_obj()
    })
    
    plot55_obj <- reactive({
      if(input$performance_metrics == 1){
        df <- Stady55df()$dataPlot
        p <- ggplot2::ggplot(df, ggplot2::aes(X,
                                              Y,
                                              group = as.factor(etiq),
                                              color=as.factor(etiq))) +
          ggplot2::geom_point() +
          ggplot2::labs(x = "PM1:Median", y = "PM2:Mean") +
          ggplot2::geom_line(
            ggplot2::aes(linetype=as.factor(etiq)),show.legend = FALSE ) +
          ggplot2::theme(legend.position = "none") + 
          ggplot2::guides(fill=FALSE, color=FALSE) +
          ggplot2::theme_bw()
        return(p)
      }
    })
    
    output$plot_5_5 <- renderPlot({
      plot55_obj()
    })
    #######
   
    
    plot1_obj2 <- reactive({
      if(input$performance_metrics == 2){
        df <- Stady1df2()$dataPlot
        
        p <- plotly::plot_ly(df,
                             x = ~X, 
                             y = ~Y,
                             z = ~Z, 
                             type="scatter3d",
                             mode="markers", 
                             color =~etiq,
                           colors = c("gray", 
                                      "purple", 
                                      "red", 
                                      "green", 
                                      "cyan",
                                      "blue",
                                      "coral",
                                      "deepskyblue", 
                                      "orange", 
                                      "yellow", 
                                      "pink")
        ) %>%
          plotly::layout(
           title = "MCO",
            scene = list(
              xaxis = list(title = "PM1:median"),
              yaxis = list(title = "PM2:mean"),
              zaxis = list(title = "PM3:3thQuantile")
              )
            )
          return(p)
        }
    }) 
    
    output$plot21 <- plotly::renderPlotly({  
      plot1_obj2()
    })
    
    
    
####### Down load data   
######################
    ####### one data 
    output$downloadData.one <- downloadHandler(
      filename = function() {
        paste("Study1", ".csv", sep = "")
        
      },
      content = function(file) {
        write.csv(Stady1df()$final, file, row.names = FALSE)
      }
    )
    
 ######## two data 
    
    output$downloadData21.two <- downloadHandler(
      filename = function() {
        paste("Study1", ".csv", sep = "")
        
      },
      content = function(file) {
        write.csv(Stady2_1df()$final, file, row.names = FALSE)
      }
    )
 ### 
    output$downloadData22.two <- downloadHandler(
      filename = function() {
        paste("Study2", ".csv", sep = "")
        
      },
      content = function(file) {
        write.csv(Stady2_2df()$final, file, row.names = FALSE)
      }
    )
    
    ######## three data 
    
    output$downloadData31.three <- downloadHandler(
      filename = function() {
        paste("Study1", ".csv", sep = "")
        
      },
      content = function(file) {
        write.csv(Stady31df()$final, file, row.names = FALSE)
      }
    )
    ### 
    output$downloadData32.three <- downloadHandler(
      filename = function() {
        paste("Study2", ".csv", sep = "")
        
      },
      content = function(file) {
        write.csv(Stady32df()$final, file, row.names = FALSE)
      }
    )
    ### 
    output$downloadData33.three <- downloadHandler(
      filename = function() {
        paste("Study3", ".csv", sep = "")
        
      },
      content = function(file) {
        write.csv(Stady33df()$final, file, row.names = FALSE)
      }
    )
    
    ######## four data 
    
    output$downloadData41.four <- downloadHandler(
      filename = function() {
        paste("Study1", ".csv", sep = "")
        
      },
      content = function(file) {
        write.csv(Stady41df()$final, file, row.names = FALSE)
      }
    )
    ### 
    output$downloadData42.four <- downloadHandler(
      filename = function() {
        paste("Study2", ".csv", sep = "")
        
      },
      content = function(file) {
        write.csv(Stady42df()$final, file, row.names = FALSE)
      }
    )
    ### 
    output$downloadData43.four <- downloadHandler(
      filename = function() {
        paste("Study3", ".csv", sep = "")
        
      },
      content = function(file) {
        write.csv(Stady43df()$final, file, row.names = FALSE)
      }
    )
    ### 
    output$downloadData44.four <- downloadHandler(
      filename = function() {
        paste("Study4", ".csv", sep = "")
        
      },
      content = function(file) {
        write.csv(Stady44df()$final, file, row.names = FALSE)
      }
    )
    ######## five data 
    
    output$downloadData51.five <- downloadHandler(
      filename = function() {
        paste("Study1", ".csv", sep = "")
        
      },
      content = function(file) {
        write.csv(Stady51df()$final, file, row.names = FALSE)
      }
    )
    ### 
    output$downloadData52.five <- downloadHandler(
      filename = function() {
        paste("Study2", ".csv", sep = "")
        
      },
      content = function(file) {
        write.csv(Stady52df()$final, file, row.names = FALSE)
      }
    )
    ### 
    output$downloadData53.five <- downloadHandler(
      filename = function() {
        paste("Study3", ".csv", sep = "")
        
      },
      content = function(file) {
        write.csv(Stady53df()$final, file, row.names = FALSE)
      }
    )
    ### 
    output$downloadData54.five <- downloadHandler(
      filename = function() {
        paste("Study4", ".csv", sep = "")
        
      },
      content = function(file) {
        write.csv(Stady54df()$final, file, row.names = FALSE)
      }
    )
    ### 
    output$downloadData55.five <- downloadHandler(
      filename = function() {
        paste("Study4", ".csv", sep = "")
        
      },
      content = function(file) {
        write.csv(Stady55df()$final, file, row.names = FALSE)
      }
    )
    
    
    
    
    
  })
}

## To be copied in the UI
# mod_Individuals_ui("Individuals_ui_1")

## To be copied in the server
# mod_Individuals_server("Individuals_ui_1")

