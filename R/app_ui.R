#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  options(spinner.color="#337ab7", spinner.color.background="#ffffff", spinner.size = 2)
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic 
    fluidPage(
      #h1("OBAMA"),
      navbarPage("OBAMA",
        tabPanel("Home",icon = icon("home", lib = "glyphicon")),
        navbarMenu("MCO",
          tabPanel("Individual analysis",
                   mod_Individuals_ui("Individuals_ui_1")
          ),
          tabPanel("Individual analysis by sex",
                   mod_Individual_by_Sex_ui("Individual_by_Sex_ui_1")
          ),
          tabPanel("Meta-analysis",
          mod_Meta_analysis_ui("Meta_analysis_ui_1")
          )
        ),
        navbarMenu("MST",
                   tabPanel("MST one condition",
                            mod_MST_one_condition_ui("MST_one_condition_ui_1")
                            
                   ),
                   tabPanel("MST meta-analysis",
                            mod_MST_Meta_analysis_ui("MST_Meta_analysis_ui_1")
                            
                   )
                   
        )
        
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'OBAMA'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

