library(ggplot2)
library(tidyr)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyBS)
library(DT)
library(mlr)
load(file = "../results.RData")
# surrogate models evtl. entfernen aus der Datei

server = function(input, output) {
  
  
  #load(file = "./hyperpars.RData")
  learner.names = names(results)
  
  output$plot1 <- renderPlot({
    plotBMRSummary(bmrInput())
  })
  
  output$plot2 <- renderPlot({
    plotBMRRanksAsBarChart(bmrInput(), pos = "stack")
  })
  
  bmrInput <- reactive({
    bmr_surrogate[[which(learner.names == input$algo)]]
  })
  
  output$task = renderUI({
    selectInput('taski', 'Task', c("classification", "regression"), selected = "classification", multiple = FALSE)
  })
  
  output$algorithm = renderUI({
    selectInput('algo', 'Algorithm', learner.names, selected = learner.names[1], multiple = FALSE)
  })
  
  output$defaults = renderTable({
    results[[input$algo]]$default$default
  }, digits = 3)
  
  output$overallTunability = renderTable({
    mean(results[[input$algo]]$overallTunability)
  }, colnames = FALSE, digits = 3)
  
  output$plot3 <- renderPlot({
    plot(density(results[[input$algo]]$overallTunability), main = "Density of the Overall Tunability")
  })
  
  output$tunability = renderTable({
    data.frame(t(results[[input$algo]]$tunability))
  }, digits = 3)
  
  output$tuningSpaceNumerics = renderTable({
    results[[input$algo]]$tuningSpace$numerics
  }, rownames = TRUE, digits = 3)
  
  output$tuningSpaceFactors = renderTable({
    results[[input$algo]]$tuningSpace$factors
  })
  
  # defaults = reactive({ 
  #   results[[input$algo]]$default$default
  #   })
  
  # hyperparTable = reactive({
  #   hyperpars[hyperpars$run.id %in% unique(results[results$flow.name == input$lrn & results$data.name == input$ds,]$run.id), ]
  # })
  

  
  # hyperparValues = reactive({
  #   #tidyr::spread(hyperpars, hyperpar.name, hyperpar.value, fill = NA)
  #   df = reshape(hyperparTable(), idvar = "run.id", timevar = "hyperpar.name", direction = "wide")
  #   colnames(df) = gsub("hyperpar.value.", "", colnames(df))
  #   df
  # })
  
  # output$summary.vis.hist = renderUI({
  #   list(
  #     column(9,
  #       sliderInput("summary.vis.hist.nbins", "Number of bins", min = 1L, max = 100L,
  #         value = 30L, step = 1L, width = "95%")
  #     ),
  #     column(3,
  #       radioButtons("summary.vis.dens", "Show density?", choices = c("Yes", "No"),
  #         selected = "Yes", inline = TRUE)
  #     )
  #   )
  # })

}

# shinyUI(fluidPage(
#   
#   titlePanel("Tabsets"),
#   
#   sidebarLayout(
#     
#     sidebarPanel(
#       # Inputs excluded for brevity
#     ),
#     
#     mainPanel(
#       tabsetPanel(
#         tabPanel("Plot", plotOutput("plot")), 
#         tabPanel("Summary", verbatimTextOutput("summary")), 
#         tabPanel("Table", tableOutput("table"))
#       )
#     )
#   )
# ))




ui = fluidPage(
  titlePanel("Summary of the benchmark results"),
  
  sidebarLayout(
    sidebarPanel(
      #uiOutput("task"),
      uiOutput("algorithm")
    ),
    
    tabsetPanel(
      tabPanel("Surrogate models comparison", 
        plotOutput("plot1"), plotOutput("plot2")),
      tabPanel("Defaults and Tunability", 
        fluidRow(
          column(12, "Defaults", tableOutput("defaults"))), 
        fluidRow(
          column(12, "Tunability", fluidRow(
            column(1, "Overall mean tunability", tableOutput("overallTunability")), 
            column(11, "Hyperparameters", tableOutput("tunability"))
          ))),
        plotOutput("plot3"), 
      
      fluidRow(column(12, "Tuning Space",
        column(12, "Numerics", align="left", tableOutput("tuningSpaceNumerics")),
        column(12, "Factors", align="left", tableOutput("tuningSpaceFactors"))
      ))
      )
    )
  )
)



shinyUI(fluidPage(
  fluidRow(
    column(12,
      "Fluid 12",
      fluidRow(
        column(6,
          "Fluid 6",
          fluidRow(
            column(6, 
              "Fluid 6"),
            column(6,
              "Fluid 6")
          )
        ),
        column(width = 6,
          "Fluid 6")
      )
    )
  )
))



shinyApp(ui = ui, server = server)


# 
# box(width = 12, title = "Variable Visualization", id = "summary.vis.box",
#   fluidRow(
#     column(12,
#       uiOutput("summary.vis.hist")),
#     column(12,
#       plotOutput("summary.vis")
#     )
#   )
# )