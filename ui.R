shinyUI(
  fluidPage(
  titlePanel("State of the Ecosystem time series analysis"),
  
  
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      selectInput(inputId="Indicator",label="choose SOE indicator",choices = c("Landings_Managed"="Landings_Managed","LTSST"="LTSST"),
                  selected = "LTSST",multiple = F),
      
      selectInput(inputId="Model",label="Choose model type",choices = c("SOE_Standard"="SOE_Standard",
                                                                        "GAM"="GAM",
                                                                        "STARS"="STARS"),
                  selected = "SOE_Standard",multiple = F),
      
      sliderInput(inputId = "knots",
                  label = "Number of knots:",
                  min = 0,
                  max = 10,
                  value = 1),
      
      sliderInput(inputId = "gamma",
                  label = "Smoother Function",
                  min = 0,
                  max = 5,
                  value = 1 )
    ),

    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "timeseries")
      #plotOutput(outputId = "distPlot1"),
      #plotOutput(outputId = "distPlot2")
    )
  )
)
)


