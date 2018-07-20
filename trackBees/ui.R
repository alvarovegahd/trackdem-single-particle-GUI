# Author: Alvaro Vega Hidalgo | Wcislo Lab | alvarovegahd@gmail.com
# 

library(shiny)
library(shinyjs)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Wcislo Lab | Track single particle movement"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       h4("Step 1"),
       p(print("Enter directory where videos are stored. Be careful: don't include a final / in it")),
       
       # Copy the line below to make a text input box
       textInput(inputId = "videos_directory", 
                 label = ("Videos directory"), 
                 value = " Enter directory..."),
       fluidRow(column(12, verbatimTextOutput("videos_directory"))),
       actionButton("beginButton", "Load videos"),
       verbatimTextOutput("beginText"),
       hr(),
       h4("Step 2"),
       p("Draw a rectangle in the part of the frame of the video to be cropped and analyzed"),
       verbatimTextOutput("info"),
       actionButton("cropButton","Crop"),
       verbatimTextOutput("cropText"),
       actionButton("goButton", "Next"),p("Press Next when you selected the area of the image to be analyzed for that video"),
       verbatimTextOutput("nText"),
       hr(),
       h4("Step 3"),
       p(print("Enter start time and end time in format 00:00:00 (h,min,sec) to analyze videos in that interval")),
       # Copy the line below to make a text input box
       textInput(inputId = "start_time_video", 
                 label = ("Start time"), 
                 value = "Enter start time..."),
       textInput(inputId = "end_time_video", 
                 label = ("End time"), 
                 value = "Enter end time..."),
       p(print("Press RUN when all videos where cropped")),
       verbatimTextOutput("runText"),
       useShinyjs(),
       fluidRow(column(6, align="center", offset = 3, 
                       actionButton("runButton", "RUN")))#,
#       
       
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("plot1",
                  # click = "plot_click",
                  # dblclick = "plot_dblclick",
                  # hover = "plot_hover",
                  brush = "plot_brush"
       )
    )
    
  )
))
