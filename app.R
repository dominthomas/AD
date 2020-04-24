library(shiny)
library(oro.nifti)
library(png)
library(shinycssloaders)
library(keras)
library(imager)
library(abind)

# @author dthomas
# @version 1.0

# Set maximum file upload size to ~ 750MB
options(shiny.maxRequestSize = 750 * 1024 ^ 2)

ui <- fluidPage(
  # App title
  titlePanel("AD Detection"),
  
  # Sidebad panel for inputs
  sidebarPanel(
    fileInput(
      inputId = "niftiFile",
      label = "Drag and drop NIFTI file here",
      multiple = FALSE,
      buttonLabel = "Browse...",
      placeholder = "No file selected"
    ),
    
    #Add slider inputs with arbitrary min/max values 
    sliderInput('slider_x', 'X orientation', min=1, max=10, value=5),
    sliderInput('slider_y', 'Y orientation', min=1, max=10, value=5),
    sliderInput('slider_z', 'Z orientation', min=1, max=10, value=5),
    tags$br(),
    h3(textOutput("diagnosis_prediction"))
  ),
  
  
  # Main panel for displaying outputs
  mainPanel(
    h5('Plane View'),
    tabsetPanel(type = "tabs", 
                tabPanel("Axial", withSpinner(plotOutput("Axial", height = "450px",   brush = "plot_brush")), type = 1), 
                tabPanel("Sagittal", withSpinner(plotOutput("Sagittal", height = "450px",   brush = "plot_brush")), type = 1), 
                tabPanel("Coronal", withSpinner(plotOutput("Coronal", height = "450px",   brush = "plot_brush")), type = 1)
    ),
    plotOutput("orthographic")
  )
)

server <- function(input, output, session) {
  
  model <- load_model_hdf5("~/AD/final_model.h5")
  # Reactive for niftiFiles.
  niftiImage <- reactive({
    
    # Return if no file has been selected.
    if (is.null(input$niftiFile)) {
      return ()
    }
    
    # Read nifti image from datapath of temporary file; 
    # created when user uploaded the nifti.
    readNIfTI(input$niftiFile$datapath, reorient = FALSE, verbose = TRUE)
    
  })
  
  observe({
    volume <- niftiImage()
    d <- dim(volume)
    
    # Control the value, min, max, and step.
    # Step size is 2 when input value is even; 1 when value is odd.
    updateSliderInput(session, "slider_x", value = as.integer(d[1]/2),max = d[1])
    updateSliderInput(session, "slider_y", value = as.integer(d[2]/2),max = d[2])
    updateSliderInput(session, "slider_z", value = as.integer(d[3]/2),max = d[3])
  })
  
  # Add Axial, Sagittal and Coronal displays
  output$Axial <- renderPlot({
    try(image(niftiImage(),  z = input$slider_z, plane="axial", plot.type = "single", col = gray(0:64/64)))
  })
  output$Sagittal <- renderPlot({
    try(image(niftiImage(),  z = input$slider_x, plane="sagittal", plot.type = "single", col = gray(0:64/64)))
  })  
  
  output$Coronal <- renderPlot({
    try(image(niftiImage(),  z = input$slider_y, plane="coronal", plot.type = "single", col = gray(0:64/64)))
  })
  
  output$orthographic<- renderPlot({
    try(orthographic(niftiImage(),
                 col.crosshairs="green",
                 xyz = c(input$slider_x,   input$slider_y, input$slider_z),
                 oma = rep(0, 4),
                 mar = rep(0.5, 4),
                 col = gray(0:64/64)
    ))
  })
  
  observe({
    t1 <- niftiImage()
    
    if(!is.null(t1))
    {
      if(dir.exists("current_nifti"))
      {
        unlink("current_nifti", recursive = TRUE)
      }
      dir.create("current_nifti")
      
      for(slice_num in 1:dim(t1)[2])
      {
        full_path <- paste("current_nifti", "/", slice_num, ".png", sep="")
        png(filename = full_path)
        image(t1, z = slice_num, plot.type="single", plane="coronal")
        dev.off()     
        
        if(image_has_pixels_over_zero(full_path))
        {
          next
        }
        else
        {
          file.remove(full_path)
        }
      }
    }
  })
  
  prediction <- reactive({
    t1 <- niftiImage()
    print("I'm here")
    if(dir.exists("current_nifti"))
    {
      setwd("current_nifti")
      files <- list.files(".")
      file_num_only <- c()
      
      for (file in files)
      {
        file_num_only <- c(file_num_only, str_extract(file, '(\\d*)'))
      }
      
      file_num_only <- sort(as.numeric(file_num_only))
      png <- paste(file_num_only[88], ".png", sep = "") %>%
        load.image()
      
      setwd("../")
      
      png <- png %>%
        autocrop(c(0, 0, 0)) %>%
        resize(w = 227, h = 227)
      
      png <- png[, , , 1]
      dim(png) <- c(227, 227, 1)
      l <- list()
      l <- append(l, list(png))
      l <- abind(l, along = 0)
      a <- predict(model, l)
      print(a)
      class(a)
      print(a[1])
      if(a == 0)
      {
        print("Hello")
      }
      return(predict(model, l))
    }
  })
  
  output$diagnosis_prediction <- renderText({
    try(
      if(!is.null(prediction()))
      {
        if(prediction() == 0)
        {
          return("Cognitively Normal")
        } 
        else 
        {
          return("Alzheimer's Demented")
        }
      }
    )
  })
  
  observe({
    if(!is.null(prediction()))
    {
      print(prediction())
    }
  })
  
  image_has_pixels_over_zero <- function(file_path)
  {
    img <- readPNG(file_path)
    return(any(img > 0))
  }
 
# End the shiny session when the app closes. 
#  session$onSessionEnded(function() { 
#    stopApp()
#    q("no") 
#  })
  
  onStop(function() {
    if(dir.exists("current_nifti"))
    {
      unlink("current_nifti", recursive = TRUE)
    }
  })
  
  # TODO remove - for debugging.
  observe({
    print(input$niftiFile$datapath)
    print(niftiImage())
  })
}


shinyApp(ui, server)