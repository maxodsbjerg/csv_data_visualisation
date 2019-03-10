#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#install.packages("plotly")
library(shiny)
#library(plotly)
library(ggplot2)


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("csv-data explorer"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
     sidebarPanel(
       ## add fileInput widget ##
       fileInput("uploadedfile", "Choose CSV File",
                 multiple = FALSE,
                 accept = c("text/csv",
                            "text/comma-separated-values,text/plain",
                            ".csv")),
       ## add x-axis selector
       selectInput("x_axis", "Select x-axis", choices=NULL),
       #sliderInput("range", "Range:",
                   #min = 1750, max = 2009,
                   #value = c(1750,2009), sep = ""),
       ## add y-axis selector
       selectInput("y_axis", "Select y-axis", choices=NULL),
       ## add factor selector
       #selectInput("y_axis1", "Select y-axis1", choices=NULL)
       numericInput("min", "x-axis minimum value", 1750),
       numericInput("max", "x-axis maximum value", 2008),
       numericInput("size", "Line size", 1, min = 1, max = 10),
       selectInput("colour", "Colour",
                   c("blue" = "blue",
                     "green" = "green",
                     "yellow" = "yellow",
                     "black" = "black"))
     ),
      
     # Show a plot of the selected relationships
     mainPanel(
       ## add plotOutput("")
       plotOutput('plot'),
       tableOutput('table')
     )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
   
  uploaded_file <- eventReactive(input$uploadedfile, {
    read.csv(input$uploadedfile$datapath, skip = 1)
  })
  
  observeEvent(input$uploadedfile, {
      vars = names(uploaded_file())
      updateSelectInput(session, "x_axis", choices = vars)
      updateSelectInput(session, "y_axis", choices = vars)
      updateSelectInput(session, "y_axis1", choices = vars)
      updateSelectInput(session, "fac", choices = vars)
      
    })
  
  output$table <- renderTable({
    uploaded_file()[, c(input$x_axis, input$y_axis, input$fac), drop = FALSE]
  }, rownames = TRUE)
    
  output$plot <- renderPlot({
    ggplot(uploaded_file(), aes_string(x = input$x_axis, y = input$y_axis)) + 
      geom_line(colour= input$colour, 
                size = input$size)+
      xlim(input$min, input$max)+
      ggtitle(input$uploadedfile$name) +
      theme_minimal()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

