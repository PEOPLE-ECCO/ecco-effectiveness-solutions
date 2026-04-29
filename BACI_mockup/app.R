#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib)

#library(ggplot2)



# ====================================================
# UI
# ====================================================


ui <- fluidPage(
  

  # 
  # selectInput("method", label = "Select matching method", choices = c("optimal", "nearest", "full", "quick"), selected="nearest", multiple=FALSE),
  titlePanel("PEOPLE-ECCO BACI solution - mock-UI"),
  
  tabsetPanel(
    tabPanel("Instructions", 
             
             h3("Instructions"),
             p("General overview of solution, links, etc.")
             
    ),
    
    tabPanel("Units of analysis",
  
             h3("Units of analysis"),  

             p("Some info about this tab.\n"),

             fileInput("uoa_vec", 
                       label= tags$span(
                         "Choose file:",
                         tags$i(
                           class = "glyphicon glyphicon-info-sign", 
                           style = "color:#0072B2;",
                           title = "Further information "
                         )),
                       accept = c(".shp",".geojson", "application/json")),

             selectInput("geom_type", label="Geometry type:", choices=c("points", "polygons")),
             
             selectInput("col_treatment", label="Select treatment attribute name:", choices=c("colname1", "colname2", "etc")),
             
             textOutput("Short overview of input vector file (nr geometries, attributes).")
    ),
    
    tabPanel("Matching covariates",
             
             h3("Matching covariates"),
             p("Some info about this tab.\n"),
             
             p("Matching covariates can be included in the vector file of units of analysis..."),
             selectInput("col_matchvars", 
                         label="Select matching variables in input vector:", 
                         choices=c("colname1", "colname2", "etc"),
                         multiple=TRUE),
             
             p("... or collated from vector/raster files. \n"),
             fileInput("matchLyr1", 
                       "Select matching covariate layer #1"),
             p("Depending whether layer is raster or vector, options should change."),
             selectInput("matchLyr1_fun", 
                         label="Summarizing function", 
                         choices=c("nearest", "bilinear", "mean", "sd", "count", "distance", "fraction", "custom function"),
                         selected="nearest"),
             textInput("matchLyr1_args", "Additional arguments:", ""),
             
             
             fileInput("matchLyr2", 
                       "Select matching covariate layer #2"),
             selectInput("matchLyr2_fun", 
                         label="Summarizing function", 
                         choices=c("nearest", "bilinear", "mean", "sd", "count", "distance", "fraction", "custom function"),
                         selected="nearest"),
             textInput("matchLyr2_args", "Additional arguments:", ""),
             p("Each time a matching covariate layer is selected, option to select another one should be provided.")
             
             ),
    

    
    
    tabPanel("Matching parameters",
             
             h3("Matching parameters"),
             p("Some info about this tab.\n"),
             
             div(
               style = "display: flex; align-items: center; gap: 10px;",
               tags$label("method: "),
               selectInput("method", label = NULL, 
                           choices = c("nearest", "optimal", "full", "quick", "genetic", "cem", "exact", "cardinality", "subclass"),
                           selected="nearest")
             ),
             
             div(
               style = "display: flex; align-items: center; gap: 10px;",
               tags$label("distance: "),
               selectInput("distance", label = NULL, 
                           choices = c("glm", "mahalanobis", "others to be added"),
                           selected="glm")
             ),
             
             div(
               style = "display: flex; align-items: center; gap: 10px;",
               tags$label("ratio: "),
               numericInput("ratio", label = NULL, 
                            0, min = 0)
             ),
             
             div(
               style = "display: flex; align-items: center; gap: 10px;",
               tags$label("replace: "),
               selectInput("replace", label = NULL, 
                           choices = c("FALSE", "TRUE"),
                           selected="FALSE")
             ),


             

             actionButton("click_matching", "Run matching", class = "btn-lg btn-success")
             ),
    

    tabPanel("Matching results",
             h3("Matching evaluation"),
             p("Could be a tab with plots etc. to evaluate matching analysis, ideally also option to write to file.\n")
             ),
    
    
    
    tabPanel("Impact evaluation",
             
             h3("Impact evaluation"),
             p("Tab info.\n"),
             
             p("This step can be stand-alone with pre-defined matching results(in which case the units of analysis need to be selected here), or use the output of the previous step .\n"),
             fileInput("uoa_vec", 
                       "Choose file",
                       accept = c(".shp",".geojson", "application/json")),
             
             p("These last inputs correspond to the outputs of the other PEOPLE-ECCO tools, or can be any other dataset of interest.\n"),
             fileInput("fn_before", 
                       'Choose impact variable for "before" period (optional)'),
             fileInput("fn_after", 
                       'Choose impact variable for "after" period (optional)'),
             fileInput("fn_effect", 
                       'Choose impact variable effect (optional)'),
             
             actionButton("click_baci", "Run impact evaluation", class = "btn-lg btn-success")
             ),
    
    tabPanel("Impact evaluation results",
             h3("Impact evaluation results"),
             p("Optionally a tab to inspect the outputs, or combine it with the previous tab.\n")
    )
    
  )

  # 

  

  
)






# ====================================================
# SERVER
# ====================================================

server <- function(input, output, session) {
  



  # string <- reactive(paste0("Hello ", input$name, "!"))
  # 
  # output$greeting <- renderText(string())
  # observeEvent(input$name, {
  #   message("Greeting performed")
  # })
  # 
}






# ====================================================
# Run
# ====================================================

# Run the application 
shinyApp(ui = ui, server = server)
