#' Analyze Images
#'
#'

analyze_images<-function(){
  library(shiny)
  library(shinyalert)
  library(shinyFiles)
  library(shinyjs)
  
  jscode <- "shinyjs.closeWindow = function() { window.close(); }"
  
  ui = fluidPage(
    useShinyjs(),
    h1("  "),
    fluidRow(
      column(4, offset=2,actionButton("buttonId", "Run Image Analysis",class = "btn-success",style='height:130px;width:375px;font-size:120%',icon=icon("wand-magic-sparkles")))),
    fluidRow(
      column(4, offset=2,actionButton("close", "Close window",class = "btn-danger",style='height:130px;width:375px;font-size:120%',icon=icon("check"))))
  )
  
  
  server = function(input, output, session) {
    observeEvent(input$buttonId, {
      message("running analyze_img.R")
      source("analyze_img.R")
    })
    output$table <- renderDataTable(Cell.Count)
    observeEvent(input$close, {
      js$closeWindow()
      stopApp()
    })
  }
  runGadget(ui, server, viewer = dialogViewer("cellcount Image Analysis Interface",
                                              width = 400, height = 100))
  
}
