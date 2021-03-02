library(shiny)
library(tidyverse)

slopes <-c()

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
  
  # Histogram Picture
  sidebarLayout( 
    
    sidebarPanel(
      
      verticalLayout(
        
        actionButton("sample1", "1 Sample"),
        actionButton("sample10", "10 Samples"),
        actionButton("sample50", "50 Samples"),
        actionButton("clear", "Clear")
        
      )

    ),
    
    mainPanel(
      
      #Equation of least-squares line
      plotOutput("slopePlot"),
      textOutput("check")
      
    ) 
    
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

  x <- reactive({
    runif(1000,0,10)
  })


  # Generate y's
  y <- reactive({
    input$b0 + input$b1*x() + rnorm(1000, 0, input$sigma)
  })

  # Make grid of points
  grid.0<- reactive({
    data.frame(x = x(), y = y())
  })
  
  # Make graph of the population
  output$population <-renderPlot({
    # Update only on simulate
    input$makepop
    isolate(ggplot(grid.0(), aes(x = x, y = y)) +
      geom_point())
  })
  
  # data.frame(dat=sapply(1:1000, function(i) mean(rexp(input$n,10))))
  slopes <- reactive({
  
      data.frame(slopes=sapply(1:1000, function(i){pick <- sample(1:1000, size = 10, replace = FALSE)
                               points <- grid.0()[pick,]
                               lm(points$y~points$x)$coefficients[2]}))
  })
  
  output$slopePlot <- renderPlot({
  input$sample10
   isolate(hist(slopes()$slopes))
  })
  
  # output$check <- renderText({
  #   paste0("slopes =", output$slopes)
  # })
  
##### FOR FUTURE FIXES WITH MULTIPLE PRESSES  
  
  # Create samples of slopes for each press of sample
  
  # # Intialize slopes vector
  # slopes <- reactiveValues(data=NULL)
    
    # If sample1 is clicked, generate one slope to add
    # newSlope<-reactive({
    # 
    #   input$sample1
    #   pick <- sample(1:1000, size = 10, replace = FALSE)
    #   points <- grid.0()[pick,]
    #   lm(points$y~points$x)$coefficients[2]
    #   #slopes <- c(slopes, newSlope)
    #   
    #   })
    # 
    # 
    # slopes <- reactive({
    #   input$sample1
    #   isolate({
    #     slopes <<- append(slopes, newSlope())
    #   })
    # })
    # 
    # output$check <-renderText({
    #   input$sample1
    #   isolate(paste0("New Slope =", newSlope(), "Slopes =", slopes()))
    # })
    
    
    # output$slopePlot <- renderPlot({
    #   req(slopes)
    #   ggplot(data = slopes, aes(x=slopes))+
    #     geom_hist()
    # })
}

shinyApp(ui = ui, server = server)