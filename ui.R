shinyUI(
  fluidPage(
  titlePanel("State of the Ecosystem Time Series Analysis"),
  
  
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      selectInput(inputId="epu_abbr",label="Choose Region",choices = c("MAB"="MAB",
                                                                        "NE"="NE", 
                                                                       "GB" = "GB", 
                                                                       "GOM" = "GOM", 
                                                                       "All"="All"),
                  selected = "All",multiple = F),
      
      
      selectInput(inputId="Indicator",label="Choose SOE indicator",choices = c("Revenue_Managed"="Revenue_Managed",
                                                                               "Landings_Managed"="Landings_Managed", 
                                                                               "Recreational_Seafood_Harvest" = "Recreational_Seafood_Harvest", 
                                                                               "Recreational_Effort" = "Recreational_Effort",
                                                                               "Recreational_Catch_Diversity" = "Recreational_Catch_Diversity",
                                                                               "Zooplankton_Diversity" = "Zooplankton_Diversity", 
                                                                               "Bottom_Temp" = "Bottom_Temp", 
                                                                               "Heatwave_Cumulative_Intensity" = "Heatwave_Cumulative_Intensity", 
                                                                               "Heatwave_Maximum_Intensity" = "Heatwave_Maximum_Intensity", 
                                                                               "LTSST"="LTSST"),
                  selected = "LTSST",multiple = F),
      
      selectInput(inputId="Model",label="Choose model type",choices = c("SOE_Standard"="SOE_Standard",
                                                                        "GAM"="GAM"),
                  selected = "SOE_Standard",multiple = F),
      
      sliderInput(inputId = "knots",
                  label = "Number of knots:",
                  min = 0,
                  max = 30,
                  value = 1, 
                  step = 1),
      
      sliderInput(inputId = "gamma",
                  label = "Smoother Function:",
                  min = 0,
                  max = 10,
                  value = 1, 
                  step = 0.01)
    ),

    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "timeseries"),
      
      h4("AIC"),
      verbatimTextOutput(outputId = "modelSummary")
      #plotOutput(outputId = "distPlot1"),
      #plotOutput(outputId = "distPlot2")
    )
  )
)
)


