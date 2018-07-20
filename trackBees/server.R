#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(png)
library(shiny)

# source files; commit folder, trackdem_Alvaro_source.R and load trackdem

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  
  # You can access the value of the widget with input$text, e.g.
  output$videos_directory <- renderPrint({ paste0("Directory selected: ",input$videos_directory) })
  
  # Next botton ----
  # Inititating reactive values, these will `reset` for each session
  # These are just for counting purposes so we can step through the questions/Users/krichilskyea/Documents/Alvaro/trackDem_Bees/trackBees_shiny/VideosToTryShiny
  values <- reactiveValues()
  #videoHandler <- reactiveValues()
  
  dirInputVideos <- reactive({
    input$videos_directory
  })
  
  videosInInputDir <- reactive({
    setdiff(list.files(input$videos_directory), list.dirs(input$videos_directory,recursive = FALSE, full.names = FALSE))
  })
  
  nVideos <- reactive({
    length(videosInInputDir())
  })
  
  #Produce thumbnails
  dirThumbnails<- reactive({
    paste0(dirInputVideos(),"/thumbnails")
  })
  

  values$count <- 0
  
  beginText <- eventReactive(input$beginButton,{
    #dirInputVideos <- input$videos_directory
    # dir() also gets folders; wanna exclude that
    #videosInInputDir <-setdiff(list.files(dirInputVideos), list.dirs(dirInputVideos,recursive = FALSE, full.names = FALSE))
    #nVideos <- length(videosInInputDir)
    
    #Produce thumbnails
    #dirThumbnails<-paste0(dirInputVideos(),"/thumbnails")
    dir.create(dirThumbnails())
    
    for(ivideo in videosInInputDir()){
      system(paste0("ffmpeg -y -i ",dirInputVideos(),"/",ivideo," -vf  \"thumbnail\" -frames:v 1 ", dirThumbnails(),"/",ivideo, ".png"))
    }
    return(paste0("Imported ",nVideos()," videos. ", "Go to Step 2"))
    
    # videoHandler$dirInputVideos <- input$videos_directory
    # videoHandler$videosInInputDir <- setdiff(list.files(videoHandler$dirInputVideos), list.dirs(videoHandler$dirInputVideos,recursive = FALSE, full.names = FALSE))
    # videoHandler$nVideos <- length(videoHandler$videosInInputDir)
  })
  
  
  
  output$beginText <- renderText({
    # The `if` statement below is to test if the botton has been clicked or not for the first time,
    # recall that the button works as a counter, everytime it is clicked it gets incremented by 1
    # The initial value is set to 0 so we just going to return the first question if it hasnt been clicked
    if(input$beginButton == 0){
      return("Videos were not loaded yet")
    }
    beginText()
  })
  # Reactive expression will only be executed when the button is clicked
  ntext <- eventReactive(input$goButton,{
    dirInputVideos <- input$videos_directory
    # dir() also gets folders; wanna exclude that
    videosInInputDir <-setdiff(list.files(dirInputVideos), list.dirs(dirInputVideos,recursive = FALSE, full.names = FALSE))
    nVideos <- length(videosInInputDir)
    # Check if the counter `values$count` are not equal to the length of your questions
    # if not then increment quesions by 1 and return that question
    # Note that initially the button hasn't been pressed yet so the `ntext()` will not be executed
    if(values$count != nVideos){
      values$count <- values$count + 1
      return(dir(dirThumbnails())[values$count])
      
    }
    else{
      values$count <- values$count + 1
      # otherwise just return the last quesion
      return(paste0("Finished setting the area to crop of ", values$count-1, " videos! Go to Step 3"))
    }
    
  })
  
  output$nText <- renderText({
    # The `if` statement below is to test if the botton has been clicked or not for the first time,
    # recall that the button works as a counter, everytime it is clicked it gets incremented by 1
    # The initial value is set to 0 so we just going to return the first question if it hasnt been clicked
    if(input$goButton == 0){
      return("Click Next when the videos were loaded")
    }
    
    ntext()
  })
  
  img <- reactive({
    if(values$count <= nVideos() & values$count !=0){
      readPNG(paste0(dirThumbnails(),"/",videosInInputDir()[values$count], ".png"), native = TRUE)}
  })
  # plot image ----
  
  prev_vals <- NULL
  structures <- reactiveValues(data = data.frame(box_id = numeric(), 
                                                 xmin = numeric(), 
                                                 ymin = numeric(), 
                                                 xmax = numeric(), 
                                                 xmax = numeric()))
  output$plot1 <- renderPlot({
    if(values$count <= nVideos() & values$count !=0){
      par(mar=c(1,1,1,1))
      img <- readPNG(paste0(dirThumbnails(),"/",videosInInputDir()[values$count], ".png"), native = TRUE)
      img_width <- dim(img)[2]
      img_height <- dim(img)[1]
      
      #plot(1:2, type='n')
      plot(1:2, type='n', xlab=" ", ylab=" ",
           xlim = c(0,img_width),
           ylim = c(0,img_height))
      #rasterImage(img,1,1,640,640)
      rasterImage(img,
                  xleft = 0, 
                  ybottom = 0,
                  xright =  img_width,
                  ytop =  img_height) 
      
      if (nrow(structures$data) > 0) {
        r <- structures$data
        # rect(r$xmin, dim(img)[1]-r$ymin, r$xmax, dim(img)[1]-r$ymax, border = "red")
      }
    }}, height = 640, width = 640)
  
  
  observe({
    if(values$count <= nVideos() & values$count !=0){
      
      e <- input$plot_brush
      if (!is.null(e)) {
        
        vals <- data.frame(xmin = round(e$xmin, 1), ymin = round(dim(img())[1]-e$ymin, 1), 
                           xmax = round(e$xmax, 1), ymax = round(dim(img())[1]-e$ymax, 1))
        
        if (identical(vals,prev_vals)) return() #We dont want to change anything if the values havent changed.
        structures$data <- rbind(structures$data,cbind(data.frame(box_id = nrow(structures$data)+1),vals))
        prev_vals <<- vals
      }}
  })
  output$info <- renderText({
    
    xy_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("x=", round(e$x, 1), " y=", round(dim(img)[1]-e$y, 1), "\n")
    }
    xy_range_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("xmin=", round(e$xmin, 1), 
             " xmax=", round(e$xmax, 1), 
             " ymin=", round(dim(img())[1]-e$ymax, 1), 
             " ymax=", round(dim(img())[1]-e$ymin, 1))
    }
    
    paste0(
      "Area to cut and analyze: ", xy_range_str(input$plot_brush)
    )
  })
  
  next_count <- reactive({values$count})
  
  output$cropText <- renderText({
    #depends on the crop Button
    input$cropButton
    isolate({
      if(input$cropButton!=0){
        xmin_tmp = isolate(round(input$plot_brush$xmin, 0))
        xmax_tmp = isolate(round(input$plot_brush$xmax, 0))
        ymin_tmp = isolate(round(dim(img())[1]-input$plot_brush$ymax, 0))
        ymax_tmp = isolate(round(dim(img())[1]-input$plot_brush$ymin, 0))
        
        # The first time it is clicked, a csv file is created with the data
        if(input$cropButton==1){
          crop_data <- data.frame(n=input$cropButton, 
                                  fullDirVideo=paste0(dirInputVideos(),"/",dir(dirInputVideos())[next_count()]),
                                  x_min=xmin_tmp,
                                  x_max=xmax_tmp,
                                  y_min=ymin_tmp,
                                  y_max=ymax_tmp)
          write.table(file = paste0(dirThumbnails(),"/crop_data.csv"),crop_data,row.names = F,append = F)
        } else {
          crop_data <- data.frame(n=input$cropButton, 
                                  fullDirVideo=paste0(dirInputVideos(),"/",dir(dirInputVideos())[next_count()]),
                                  x_min=xmin_tmp,
                                  x_max=xmax_tmp,
                                  y_min=ymin_tmp,
                                  y_max=ymax_tmp)
          write.table(file = paste0(dirThumbnails(),"/crop_data.csv"),crop_data,append = T,row.names = F,col.names = F)
        }
      }
      return("CropRead")
    })
  })
  
  #server
  observe({
    shinyjs::hide("runButton")
    
    if(values$count == 1+nVideos() & values$count!=0){
      shinyjs::show("runButton")}
  })
  
  
  
  output$runText <- renderText({
    input$runButton
    if(input$runButton==0){
      return("Click RUN when ready")
    }
    #depends on the crop Button
    dirResults_GUI<-paste0(dirname(isolate({dirThumbnails()})),"/results")
    if(dir.exists(dirResults_GUI)){
      unlink(dirResults_GUI,recursive = T)
    }
    source("./src/trackDem_source_shiny.R", local = TRUE)
    # Use master function
    st_GUI<-Sys.time()
    trackSingleParticle_Master(dir_cutCoordsTable = paste0(isolate({dirThumbnails()}),"/crop_data.csv"),
                               stTime_cut = isolate({input$start_time_video}),
                               endTime_cut = isolate({input$end_time_video}),
                               framesPerIteration_ToIterations = 100,
                               area_thr_toAnimation = 1000
    )
    et_GUI<-Sys.time()
    et_GUI-st_GUI
    tDif<-(et_GUI-st_GUI)
    print(paste0("All videos analyzed in ",round(as.numeric(tDif),1)," ",attributes(tDif)$units))
  })
  
})
#/Users/krichilskyea/Documents/Alvaro/trackDem_Bees/trackBees_shiny/VideosToTryShiny
#Should use read.table("/Users/krichilskyea/Documents/Alvaro/trackDem_Bees/trackBees_shiny/VideosToTryShiny/thumbnails/crop_data.csv",header = T)