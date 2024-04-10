#' Analyze Images GUI
#'
#' Brings in data entered from the "input_data_GUI" function and automatically runs a script to analyze images
#' pertaining to cells from fluorescent microscopy images. This uses a Shiny user-interface, where three buttons
#' are available to interact with, as well as an interactive table below. The "Run Image Analysis" button runs 
#' the analysis script, the "Close Window" button closes the Shiny interface, and the "Refresh" button allows 
#' users to view the analyzed data once the analysis has completed.
#' 
#' @return Runs an automatic image analysis
#' @export

analyze_images_GUI<-function(){
  library(shiny)
  library(shinyalert)
  library(shinyFiles)
  library(shinyjs)
  library(lubridate)
  library(DT)
  
  jscode <- "shinyjs.closeWindow = function() { window.close(); }"
  
  ui = fluidPage(
    useShinyjs(),
    h1("  "),
    h1("  "),
    actionButton("buttonId", "Run Image Analysis",class = "btn-success",style='height:75px;width:252px;font-size:140%',icon=icon("wand-magic-sparkles"),style="display:center-align"),
    actionButton("close", "Close window",class = "btn-danger",style='height:75px;width:252px;font-size:140%',icon=icon("check"),style="display:center-align"),
    actionButton("BRefresh","Refresh",class = "btn-success",style='height:75px;width:252px;font-size:140%',icon=icon("arrows-rotate"),style="display:center-align"),
    h1("  "),
    DT::dataTableOutput('data')
  )
  
  
  server = function(input, output, session) {
    rv <- reactiveVal(Cell.Count)
    
    output$data <- DT::renderDataTable ({
      DT::datatable(rv(), editable = TRUE)
    })
    observeEvent(input$buttonId, {
      message("running analyze_img.R")
      source("cellcount_files_HABF/analyze_img.R")
    })
    onclick("BRefresh",{
      proxy=dataTableProxy("data")
      replaceData(proxy,Cell.Count)
    })
    observeEvent(input$data_cell_edit, {
      info <- input$data_cell_edit
      newdf <- rv()
      newdf[info$row, info$col] <- info$value
      rv(newdf)
      Cell.Count <<- rv()
    })
    observeEvent(input$close, {
      js$closeWindow()
      stopApp()
    })
  }
  runGadget(ui, server, viewer = dialogViewer("cellcount Image Analysis Interface",
                                              width = 800, height = 700))
  
}
