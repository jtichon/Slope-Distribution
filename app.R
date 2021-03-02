library(shiny)
library(tidyverse)

 #totalSlopes <-c()

ui <- fluidPage(
  
# Numeric Inputs for Model Parameters  
  fluidRow(
    
    # Beta0 between 0 and 10 
    column(4,
           numericInput("b0", HTML("&beta;<sub>0</sub>:"), value = 2,
                        min = 0, max = 10, step = 1)
           ),
    
    # Beta1 between 0 and 10
    column(4,
           numericInput("b1", HTML("&beta;<sub>1</sub>:"), value = 5,
                        min = 0, max = 10, step = 1)
           ),
    
    # sigma between 0.5 and 4
    column(4,
           numericInput("sigma", HTML("&sigma;<sub>&epsilon;</sub>:"), value =1,
                        min = 0.5, max = 4, step = 0.5)
           )
    
  ),
  
  # Population Picture
  sidebarLayout( 
    sidebarPanel(
      actionButton("makepop", "Simulate")
    ),
    
    mainPanel(
      
      #Equation of least-squares line
      textOutput("equation"),
      plotOutput("population")
      
      ) 
    
    ),
  
  # # Big Simulate Button
  # 
  # actionButton("sample", "Simulate Samples"),
  
  # Histogram Picture
 
  fluidPage(
    
    column(6, 
           plotOutput("sampleSlopes")),
    
    column(6,
           plotOutput("slopesDistrn"))
    
  )

)

server <- function(input, output){
  
  # Make equation of the line
  output$equation <-renderText({
    #Update only on simulate
    input$makepop
    isolate(paste0("y = ", input$b0, " + ", input$b1, "x + ", input$sigma))
    })
  
  # Generate x's

  x <- eventReactive(input$makepop, {
    runif(1000,0,10)
  })


  # Generate y's
  y <- eventReactive(input$makepop, {
    input$b0 + input$b1*x() + rnorm(1000, 0, input$sigma)
  })

  # Make grid of points
  grid.0<- eventReactive(input$makepop, {
    data.frame(x = x(), y = y())
  })
  
  # Make graph of the population
  output$population <-renderPlot({
    # Update only on simulate
    input$makepop
    isolate(ggplot(grid.0(), aes(x = x, y = y)) +
      geom_point())
  })
  
  
  # Timer for simulating points
  timer <- reactiveTimer(500)
  
  # On a timer, select the points for the simulation one at a time
  points <- reactive({
    timer()
    # data.frame(points = sapply(1:1000, function(i){
    #   pick <- sample(1:1000, size = 10, replace = FALSE)}))
    pick <- sample(1:1000, size = 10, replace = FALSE)
    grid.0()[pick,]
  })
  
  
  output$sampleSlopes <- renderPlot({

    # Create model for sample points
    model.points <- lm(points()$y~points()$x)

    # Update underlay only on simulate, add layer for selected points in red
    input$makepop
    isolate(ggplot(grid.0(), aes(x = x, y = y)) +
             geom_point() +
     geom_point(data = points(), aes(x= x, y = y, colour = "red", size = 5)) +
     geom_abline(slope = model.points$coefficients[2],
                 intercept = model.points$coefficients[1], colour = "red",
                 size = 2) )
  })
  
  # Find new slope on timer
  newSlope <- reactive({
    timer()
    model.points <- lm(points()$y~points()$x)
    model.points$coefficients[2]
  })

  observeEvent(input$makepop, {
    totalSlopes <<- c()
  })

  # Create plot of slopes by adding new slope to previous slopes
  output$slopesDistrn <- renderPlot({
    timer()
    currentSlope <- isolate({newSlope()})
    totalSlopes <<- c(totalSlopes, currentSlope)
    hist(totalSlopes)
  })
    
}

shinyApp(ui = ui, server = server)