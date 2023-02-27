#' Input Data
#'
#' This function provides a Shiny user-interface to easily input data needed for fluorescent microscopy image analysis
#' outlined in *cellcount*. This Shiny app contains several tabs for data input, directory selection, etc. Users must close
#' the app by selecting the 'Completion' tab followed by 'Submit data entries' for all entered data to be exported into 
#' the local R environment.
#' 
#' @return Data input user-interface for *cellcount* image analysis
#' @export

input_data<-function(){
  library(shiny)
  library(shinyalert)
  library(shinyFiles)
  library(shinyjs)
  
  jscode <- "shinyjs.closeWindow = function() { window.close(); }"
  
  image_names1<<-textConnection('image_names2','wr',local=FALSE)
  thresh_adj1<<-textConnection('thresh_adj2','wr',local=FALSE)
  FOV1<<-textConnection('FOV2','wr',local=FALSE)
  image_num1<<-textConnection('image_num2','wr',local=FALSE)
  filter_vol1<<-textConnection('filter_vol2','wr',local=FALSE)
  total_vol1<<-textConnection('total_vol2','wr',local=FALSE)
  contrast_adj1<<-textConnection('contrast_adj2','wr',local=FALSE)
  filter_area1<<-textConnection('filter_area2','wr',local=FALSE)
  csv_file1<<-textConnection('csv_file2','wr',local=FALSE)
  tolerance1<<-textConnection('tolerance2','wr',local=FALSE)
  ext1<<-textConnection('ext2','wr',local=FALSE)
  areathresh1<<-textConnection('areathresh2','wr',local=FALSE)
  rec_width1<<-textConnection('rec_width2','wr',local=FALSE)
  rec_height1<<-textConnection('rec_height2','wr',local=FALSE)
  
  ui1 = fixedPage(
    useShinyjs(),
    extendShinyjs(text = jscode, functions = c("closeWindow")),
    div(navlistPanel(
      "Cellcount - Image Analysis UI",
      " ",
      " ",
      tabPanel(h4("Directories"),
               h3("   "),
               fixedRow(
                 column(2,offset=2,shinyDirButton('path1','CSV save directory','Please select a directory to save CSV files',FALSE,class = "btn-success",style='height:35px;width:350px;font-size:120%',icon=icon("folder-open")))
               ),
               fixedRow(
                 column(2,offset=2,shinyDirButton('path2','Image save directory','Please select a directory to save Image files',FALSE,class = "btn-success",style='height:35px;width:350px;font-size:120%',icon=icon("folder-open")))
               ),
               fixedRow(
                 column(2,offset=2,shinyDirButton('path3','Image analysis directory','Please select a directory containing image files',FALSE,class = "btn-success",style='height:35px;width:350px;font-size:120%',icon=icon("folder-open")))
               )
      ),
      tabPanel(h4("Analysis Type"),
               h3("   "),
               fixedRow(
                 column(8,offset=2,selectInput("selectData",
                                               h5(strong("Please select one of the following options regarding imaging analysis type:")),
                                               choices=list("Single-cell","Filamentous","Colonial"),selected="Single-cell"))
               )
      ),
      tabPanel(h4("Data inputs"),
               h3("   "),
               fixedRow(
                 column(2,offset=2,actionButton("run4", "Species name", class = "btn-success",style='height:35px;width:350px;font-size:120%',icon=icon("clipboard")))
               ),
               fixedRow(
                 column(2,offset=2,actionButton("run6", 'Threshold adjustment', class = "btn-success",style='height:35px;width:350px;font-size:120%',icon=icon("gear")))
               ),
               fixedRow(
                 column(2,offset=2,actionButton("run7", 'Field of View', class = "btn-success",style='height:35px;width:350px;font-size:120%',icon=icon("magnifying-glass")))
               ),
               fixedRow(
                 column(2,offset=2,actionButton("run8", 'Number of images', class = "btn-success",style='height:35px;width:350px;font-size:120%',icon=icon("image")))
               ),
               fixedRow(
                 column(2,offset=2,actionButton("run9", 'Volume filtered', class = "btn-success",style='height:35px;width:350px;font-size:120%',icon=icon("vials")))
               ),
               fixedRow(
                 column(2,offset=2,actionButton("run10", 'Total volume', class = "btn-success",style='height:35px;width:350px;font-size:120%',icon=icon("flask")))
               ),
               fixedRow(
                 column(2,offset=2,actionButton("run11", 'Contrast adjustment', class = "btn-success",style='height:35px;width:350px;font-size:120%',icon=icon("gears")))
               ),
               fixedRow(
                 column(2,offset=2,actionButton("run12", 'Filtration area', class = "btn-success",style='height:35px;width:350px;font-size:120%',icon=icon("layer-group")))
               ),
               fixedRow(
                 column(2,offset=2,actionButton("run17", 'Rectangular window width', class = "btn-success",style='height:35px;width:350px;font-size:120%',icon=icon("arrows-left-right")))
               ),
               fixedRow(
                 column(2,offset=2,actionButton("run18", 'Rectangular window height', class = "btn-success",style='height:35px;width:350px;font-size:120%',icon=icon("arrows-up-down")))
               ),  
               fixedRow(
                 column(2,offset=2,actionButton("run14", 'Object tolerance', class = "btn-success",style='height:35px;width:350px;font-size:120%',icon=icon("circle-exclamation")))
               ),               
               fixedRow(
                 column(2,offset=2,actionButton("run15", 'EXT input', class = "btn-success",style='height:35px;width:350px;font-size:120%',icon=icon("globe")))
               ),
               fixedRow(
                 column(2,offset=2,actionButton("run16", 'Cell area threshold', class = "btn-success",style='height:35px;width:350px;font-size:120%',icon=icon("crop-simple")))
               ),
               fixedRow(
                 column(2,offset=2,actionButton("run13", 'CSV file name', class = "btn-success",style='height:35px;width:350px;font-size:120%',icon=icon("pen")))
               )
      ),
      tabPanel(h4("Completion"),
               h3("   "),
               h3("   "),
               fixedRow(
                 column(2,offset=2,actionButton("close", "Submit data entries",class = "btn-danger",style='height:35px;width:350px;font-size:120%',icon=icon("check")))
               )
      ),
      "  ",
      "  "
    ),
    "  ",
    "  ",
    "  ",
    fixedRow(
      column(4, "Developed by NOAA NCCOS")
    )
    )
  )
  server1 = function(input, output, session) {
    volumes=getVolumes()()
    observe({
      shinyDirChoose(input,'path1',roots=volumes,session=session,filetypes=c('','txt'))
      dirname_path1<- shiny::reactive({shinyFiles::parseDirPath(volumes,input$path1)})
      shiny::observe({
        if(!is.null(dirname_path1)){
          print(dirname_path1())
          output$path1<-shiny::renderText(dirname_path1())
          csv_file<<-paste0(normalizePath(dirname_path1(),winslash = "/"),"/")
        }
      })
    })
    observe({
      shinyDirChoose(input,'path2',roots=volumes,session=session,filetypes=c('','txt'))
      dirname_path2<- shiny::reactive({shinyFiles::parseDirPath(volumes,input$path2)})
      shiny::observe({
        if(!is.null(dirname_path2)){
          print(dirname_path2())
          output$path2<-shiny::renderText(dirname_path2())
          image_file<<-paste0(normalizePath(dirname_path2(),winslash = "/"),"/")
        }
      })
    })
    observe({
      shinyDirChoose(input,'path3',roots=volumes,session=session,filetypes=c('','txt'))
      dirname_path3<- shiny::reactive({shinyFiles::parseDirPath(volumes,input$path3)})
      shiny::observe({
        if(!is.null(dirname_path3)){
          print(dirname_path3())
          output$path3<-shiny::renderText(dirname_path3())
          image_analysis<<-paste0(normalizePath(dirname_path3(),winslash = "/"))
        }
      })
    })
    observeEvent(input$selectData, {
      if(input$selectData == 'Single-cell'){
        analysis_type<<-("single.cell")
      }
      else if(input$selectData == "Filamentous"){
        analysis_type<<-("filamentous")
      }
      else if(input$selectData == "Colonial"){
        analysis_type<<-("colonial")
      }
    })
    observeEvent(input$run4, {
      shinyalert::shinyalert('Enter species information for analyzed image classification',
                             type='input',callbackR=image_name, showCancelButton = TRUE,
                             inputPlaceholder = 'Example: Microcystis_ or Dolichospermum_',
                             size="m")
    })
    observeEvent(input$run6, {
      shinyalert::shinyalert('Enter the threshold adjustment value - separation of background against cells',
                             type='input',callbackR=threshadj, showCancelButton = TRUE,
                             inputPlaceholder = 'Enter a value from 0 to 1 - average value is around 0.3',
                             size="m")
    })
    observeEvent(input$run7, {
      shinyalert::shinyalert('Enter field of view information - units are in mm^2',
                             type='input',callbackR=FOV_data, showCancelButton = TRUE,
                             inputPlaceholder = 'Enter data here',
                             size="m")
    })
    observeEvent(input$run8, {
      shinyalert::shinyalert('Enter number of images analyzed',
                             type='input',callbackR=num_image, showCancelButton = TRUE,
                             inputPlaceholder = 'Enter data here',
                             size="m")
    })
    observeEvent(input$run9, {
      shinyalert::shinyalert('Enter the volume of sample filtered - units are in mL',
                             type='input',callbackR=vol_filter, showCancelButton = TRUE,
                             inputPlaceholder = 'Enter data here',
                             size="m")
    })
    observeEvent(input$run10, {
      shinyalert::shinyalert('Enter the total volume of the sample - units are in mL',
                             type='input',callbackR=vol_total, showCancelButton = TRUE,
                             inputPlaceholder = 'Enter data here',
                             size="m")
    })
    observeEvent(input$run11, {
      shinyalert::shinyalert('Enter the contrast adjustment value - values are non-decimal values',
                             type='input',callbackR=contrastadj, showCancelButton = TRUE,
                             inputPlaceholder = 'Example: 4',
                             size="m")
    })
    observeEvent(input$run12, {
      shinyalert::shinyalert('Enter the filtration area (measure width of filter tower to calculate) - units are in mm^2',
                             type='input',callbackR=area_filter, showCancelButton = TRUE,
                             inputPlaceholder = 'Enter data here',
                             size="m")
    })
    observeEvent(input$run13, {
      shinyalert::shinyalert('Enter a file name for the CSV data file',
                             type='input',callbackR=csvfile, showCancelButton = TRUE,
                             inputPlaceholder = 'Example: Microcystis_Rep1.csv',
                             size="m")
    })
    observeEvent(input$run14, {
      shinyalert::shinyalert('Enter a object tolerance value - categorized as the minimum height of the object (i.e., cell)',
                             type='input',callbackR=tolerance, showCancelButton = TRUE,
                             inputPlaceholder = 'Example: 0.8',
                             size="m")
    })
    observeEvent(input$run15, {
      shinyalert::shinyalert('Enter a ext value - radius of the neighborhood in pixels for the detection of neighboring objects',
                             type='input',callbackR=ext, showCancelButton = TRUE,
                             inputPlaceholder = 'Example: 6',
                             size="m")
    })
    observeEvent(input$run16, {
      shinyalert::shinyalert('Enter a area threshold value - removes particles under a certain pixel area',
                             type='input',callbackR=areathresh, showCancelButton = TRUE,
                             inputPlaceholder = 'Example: 250',
                             size="m")
    })
    observeEvent(input$run17, {
      shinyalert::shinyalert('Enter a rectangular window width value',
                             type='input',callbackR=recwidth, showCancelButton = TRUE,
                             inputPlaceholder = 'Example: 17',
                             size="m")
    })
    observeEvent(input$run18, {
      shinyalert::shinyalert('Enter a rectangular window height value',
                             type='input',callbackR=recheight, showCancelButton = TRUE,
                             inputPlaceholder = 'Example: 17',
                             size="m")
    })
    image_name<-function(value4) {
      sink(image_names1)
      save_name<-cat(value4)
      sink()
      close(image_names1)
    }
    threshadj<-function(value6) {
      sink(thresh_adj1)
      save_thresh<-cat(value6)
      sink()
      close(thresh_adj1)
      thresh_adj2<<-as.numeric(thresh_adj2)
    }
    FOV_data<-function(value7) {
      sink(FOV1)
      save_FOV<-cat(value7)
      sink()
      close(FOV1)
      FOV2<<-as.numeric(FOV2)
    }
    num_image<-function(value8) {
      sink(image_num1)
      save_num<-cat(value8)
      sink()
      close(image_num1)
      image_num2<<-as.numeric(image_num2)
    }
    vol_filter<-function(value9) {
      sink(filter_vol1)
      save_volf<-cat(value9)
      sink()
      close(filter_vol1)
      filter_vol2<<-as.numeric(filter_vol2)
    }
    vol_total<-function(value10) {
      sink(total_vol1)
      save_volt<-cat(value10)
      sink()
      close(total_vol1)
      total_vol2<<-as.numeric(total_vol2)
    }
    contrastadj<-function(value11) {
      sink(contrast_adj1)
      save_cont<-cat(value11)
      sink()
      close(contrast_adj1)
      contrast_adj2<<-as.numeric(contrast_adj2)
    }
    area_filter<-function(value12) {
      sink(filter_area1)
      save_area<-cat(value12)
      sink()
      close(filter_area1)
      filter_area2<<-as.numeric(filter_area2)
    }
    csvfile<-function(value13) {
      sink(csv_file1)
      save_file<-cat(value13)
      sink()
      close(csv_file1)
    }
    tolerance<-function(value14) {
      sink(tolerance1)
      save_tolerance<-cat(value14)
      sink()
      close(tolerance1)
      tolerance2<<-as.numeric(tolerance2)
    }
    ext<-function(value15) {
      sink(ext1)
      save_ext<-cat(value15)
      sink()
      close(ext1)
      ext2<<-as.numeric(ext2)
    }
    areathresh<-function(value16) {
      sink(areathresh1)
      save_areathresh<-cat(value16)
      sink()
      close(areathresh1)
      areathresh2<<-as.numeric(areathresh2)
    }
    recwidth<-function(value17) {
      sink(rec_width1)
      save_recwidth<-cat(value17)
      sink()
      close(rec_width1)
      rec_width2<<-as.numeric(rec_width2)
    }
    recheight<-function(value18) {
      sink(rec_height1)
      save_recheight<-cat(value18)
      sink()
      close(rec_height1)
      rec_height2<<-as.numeric(rec_height2)
    }
    observeEvent(input$close, {
      js$closeWindow()
      stopApp()
    })
  }
  #shinyApp(ui = ui1, server = server1)
  runGadget(ui1, server1, viewer = dialogViewer("cellcount Image Analysis Interface",
                                                width = 800, height = 1500))
  
  Cell.Count <<- data.frame(Image_File_Name = character(0), Cell_Count = numeric(0))
  images <<- list.files(image_analysis, pattern = "tif", full.name = T)
  images_names <<- list.files(image_analysis, pattern = "tif", full.name = F)
  imgNames <<- paste0(image_names2, images_names)
  
}
