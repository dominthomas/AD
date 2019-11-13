library(shiny)
library(oro.nifti)


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
    sliderInput('slider_z', 'Z orientation', min=1, max=10, value=5)
  ),
  
  
  # Main panel for displaying outputs
  mainPanel(
    h5('Plane View'),
    tabsetPanel(type = "tabs", 
                tabPanel("Axial", plotOutput("Axial", height = "450px",   brush = "plot_brush")), 
                tabPanel("Sagittal", plotOutput("Sagittal", height = "450px",   brush = "plot_brush")), 
                tabPanel("Coronal", plotOutput("Coronal", height = "450px",   brush = "plot_brush"))
    ),
    plotOutput("orthographic")
  )
)

server <- function(input, output, session) {
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
 
  # End the shiny session when the app closes. 
  session$onSessionEnded(function() { 
    stopApp()
    q("no") 
  })
  
  # TODO remove - for debugging.
  observe({
    print(input$niftiFile$datapath)
    print(niftiImage())
  })
}


shinyApp(ui, server)