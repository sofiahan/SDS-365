library(shiny)
library(png)
library(jpeg)

# load the PCA image compression function
source("imagecompfunc.R")

# helper function for compressing b/w or color images
compress_color_image <- function(img, k) {
  
  # compress black and white images directly since they're stored as 2D matrices
  if (length(dim(img)) == 2) {
    return(compress_image(img,k)$approximation)
  }
  
  # compress each RGB channel separately for color images since they're stored as 3D arrays
  if (length(dim(img)) == 3) {
    
    # create empty array with the same dimensions as the image
    compressed <- array(0, dim = dim(img))
    
    # apply PCA compression separately to red, green, and blue channels
    for (channel in 1:3) {
      compressed[,,channel] <-
        compress_image(img[,,channel],k)$approximation
    }
    # preserve the transparency channel for PNG images, if present
    if(dim(img)[3] == 4) {
      compressed[,,4] <- img[,,4]
    }
    
    # restrict pixel values to the valid range [0,1]
    compressed[compressed < 0] <- 0
    compressed[compressed > 1] <- 1
    
    return(compressed)
  }
}

# user interface
ui <- fluidPage(
  
  # custom CSS styling for the application to match mockup design
  tags$head(
    tags$style(HTML("
    
    /* results page container */
    .results-page {
    width: 90%;
    margin: 0 auto;
    text-align: center;
    }
    
    /* results title */
    .results-title {
    font-size: 42px;
    margin-top: 10px;
    margin-bottom: 8px;
    }
    
    /* selected k text */
    .results-k {
    font-size: 22px;
    margin-bottom: 10px;
    }
    
    /* image section labels */
    .results-page h4 {
    font-size: 22px;
    margin-bottom: 5px;
    }
    
    /* buttons under results */
    .results-buttons {
    display: flex;
    justify-content: center;
    gap: 20px;
    margin-top: 15px;
    }
    
    /* style result buttons */
    .results-btn {
    background-color: #2b67a0;
    color: white;
    border: 2px solid #17324d;
    font-size: 20px;
    padding: 12px 24px;
    }
    
    .results-btn:hover {
    background-color: #245887;
    color:white;
    }
    
    /* uploaded image page container */
    .uploaded-page {
    width: 55%;
    margin: 0 auto;
    }
    
    /* blue uploaded file container */
    .uploaded-box {
    background-color: #dceaf7;
    border: 2px solid #17324d;
    border-radius: 40px;
    padding: 55px 70px 45px 70px;
    margin-bottom: 28px;
    text-align: center;
    }
    
    /* file information box */
    .file-info {
    background-color: #f5f5f5;
    border: 1px solid #bcbcbc;
    padding: 12px 16px;
    text-align: left;
    margin: 0 auto 20px auto;
    width: 80%;
    font-size: 18px;
    }
    
    /* remove button */
    .remove-btn {
    font-size: 18px;
    padding: 10px 28px;
    }
    
    /* row containing dropdown and compress buttons */
    .compression-controls {
    display: flex;
    gap: 30px;
    align-items: flex-end;
    }
    
    /* dropdown */
    .k-control {
    flex: 1;
    }
    
    /* make compress button large */
    .compress-btn {
    flex: 1;
    background-color: #2b67a0;
    color: white;
    border: 2px solid #17324d;
    font-size: 20px;
    padding: 14px 30px;
    }
    
    .compress-btn:hover {
    background-color: #245887;
    color:white;
    }
    
    /* center the page title */
    .main-title {
    text-align: center;
    font-size: 42px;
    margin-top: 35px;
    margin-bottom: 40px;
    }
    
    /* large upload container */
    .upload-box {
    width: 55%;
    height: 320px;
    margin: 0 auto;
    background-color: #dceaf7;
    border: 2px solid #17324d;
    border-radius: 40px;
    display: flex;
    align-items: center;
    justify-content: center;
    }
    
    /* make the upload control cleaner */
    .upload-box .form-group {
    margin: 0;
    text-align: center;
    }
    
    /* style the browse button */
    .upload-box .btn-file {
    background-color: #2b67a0;
    color: white;
    border: 2px solid #17324d;
    font-size: 22px;
    padding: 16px 40px;
    border-radius: 0;
    }
    
    .upload-box .btn-file:hover {
    background-color: #245887;
    color: white;
    }
    
    /* make the file name area less prominent */
    .upload-box .form-control {
    display: none;
    }
   "))
  ),
  # main app title
  h1("Image Compressor", class = "main-title"),
  # dynamically displays the appropriate page
  uiOutput("page")
)

# server logic
server <- function(input, output, session) {
  # stores information about the uploaded image
  uploaded_image <- reactiveVal(NULL)
  # store the compressed image after the image is compressed
  compressed_image <-reactiveVal(NULL)
  
  # when an image is uploaded, save its information
  observeEvent(input$image, {
    uploaded_image(input$image)
  })
  # compress image when the button is clicked
  observeEvent(input$compress, {
    
    # display a progress bar while compression is running
    withProgress(
      message = "Compressing image...", 
      value = 0,
      {
        incProgress(0.1)
        # compress the image using the selected value of k
        compressed <- compress_color_image(
          image_data(),
          as.numeric(input$k)
          )
        incProgress(0.9)
        # save the compressed image
        compressed_image(compressed)
      }
    )
  })
  # read the uploaded image
  image_data <- reactive({
    # require an uploaded image before continuing
    req(uploaded_image())
    file <- uploaded_image()
      
      # determine the file extension type
      extension <- tolower(tools::file_ext(file$name))
      
      # read the file using the appropriate package
      if (extension == "png") {
        img <- readPNG(file$datapath)
      } else if (extension %in% c("jpg", "jpeg")) {
        img <- readJPEG(file$datapath)
      }
      return(img)
  })
  # change the page depending on the current app state
  output$page <- renderUI({
    
    # landing page aka no image has been uploaded
    if (is.null(uploaded_image())) {
      
      div(
        class = "upload-box",
        fileInput(
          inputId = "image",
          label = NULL, 
          buttonLabel = "Upload Image",
          placeholder = "",
          accept = c(".png", ".jpg", ".jpeg")
        )
      )
      # results page aka image has been compressed
    } else if (!is.null(compressed_image())) {
      div(
        class = "results-page",
        # results heading
        h1(
          "Compression Results",
          class = "results-title"
        ),
        # display the selected number of principal components
        div(
          paste("k =", input$k),
          class = "results-k"
        ),
        # display original and compressed images side by side
        fluidRow(
          column(
            width = 6,
            h4("Original Image"),
            plotOutput("original")
          ),
          column(
            width = 6,
            h4("Compressed Image"),
            plotOutput("compressed")
          )
        ),
        # navigation buttons to previous pages
        div(
          class = "results-buttons",
          
          actionButton(
            inputId = "adjust",
            label = "Adjust Compression",
            class = "results-btn"
          ),
          
          actionButton(
            inputId = "remove",
            label = "Upload New Image",
            class = "results-btn"
          )
        )
      )
      # image uploaded page
    } else {
      file <- uploaded_image()
      
      # get uploaded image
      img <- image_data()
      
      # maximum allowable k is limited by the image dimensions
      dims <- dim(img)
      maxk <- min(dims[1], dims[2])
      
      div(
        class = "uploaded-page",
        # display uploaded file information
        div(
          class = "uploaded-box",
          div(
            class = "file-info",
            div(file$name),
            div(paste(round(file$size/1024,1),"KB"))
          ),
          actionButton(
            inputId = "remove",
            label = "Remove Image",
            class = "remove-btn"
          )
        ),
        div(
          class = "compression-controls",
          # choose the number of principal components
          div(
            class = "k-control",
            selectInput(inputId = "k",
                        label = "Select Level of Compression",
                        choices = 1:maxk
            )
          ),
          # run compression
          actionButton(
            inputId = "compress",
            label = "Compress",
            class = "compress-btn"
          )
        )
      )
    }
  })
  # display the original uploaded image
  output$original <- renderPlot({
    req(image_data())
    img <- image_data()
    
    # plot b/w images using a grayscale color palette
    if(length(dim(img)) == 2) {
      image(
        img, 
        axes = FALSE, 
        col = gray.colors(256)
      )
    } else {
      # plot RGB images as raster images
      plot(
        as.raster(img), 
        axes = FALSE
      )
    }
  })
  # display the compressed image
  output$compressed <- renderPlot({
    req(compressed_image())
    img <- compressed_image()
    # plot b/w compressed images
    if(length(dim(img)) == 2) {
      image(
        img, 
        axes = FALSE, 
        col = gray.colors(256)
      )
    } else {
      # plot color compressed images
      plot(
        as.raster(img),
        axes = FALSE
      )
    }
  })
  # return to the compression selection page
  observeEvent(input$adjust, {
    compressed_image(NULL)
  })
  # remove the current image and return to the landing page
  observeEvent(input$remove, {
    uploaded_image(NULL)
    compressed_image(NULL)
  })
}  

# run the app
shinyApp(ui = ui, server = server)
