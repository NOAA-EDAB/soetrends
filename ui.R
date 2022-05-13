shinyUI(
  fluidPage(
  titlePanel("State of the Ecosystem Time Series Analysis"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: drop down for Regions ----
      selectInput(inputId="epu_abbr",label="Choose Region/EPU",
                  choices = c("Mid-Atlantic Bight"="MAB",
                              "New England"="NE",
                              "Georges Bank" = "GB",
                              "Gulf of Maine" = "GOM",
                              "Shelfwide"="All"),
                  selected = "All",multiple = F),
      conditionalPanel( # NE #
        condition = "input.epu_abbr == 'NE'", 
        selectInput(inputId="Indicator",label="Choose SOE Indicator",
                    choices = c(#"Revenue_Managed"="Revenue_Managed",
                      #"Landings_Managed"="Landings_Managed",
                      "Recreational_Seafood_Harvest" = "Recreational_Seafood_Harvest",
                      "Recreational_Effort" = "Recreational_Effort",
                      "Recreational_Catch_Diversity" = "Recreational_Catch_Diversity"),
                    #"Zooplankton_Diversity" = "Zooplankton_Diversity",
                    #"Bottom_Temp" = "Bottom_Temp",
                    #"Heatwave_Cumulative_Intensity" = "Heatwave_Cumulative_Intensity",
                    #"Heatwave_Maximum_Intensity" = "Heatwave_Maximum_Intensity"),
                    #selected = "Recreational_Seafood_Harvest",multiple = F),
                    selected = "Recreational_Seafood_Harvest" ,multiple = FALSE),
      ), 
      conditionalPanel( # MAB #
        condition = "input.epu_abbr == 'MAB'", 
        selectInput(inputId="Indicator",label="Choose SOE Indicator",
                    choices = c("Revenue_Managed"="Revenue_Managed",
                                "Landings_Managed"="Landings_Managed",
                                "Recreational_Seafood_Harvest" = "Recreational_Seafood_Harvest",
                                "Recreational_Effort" = "Recreational_Effort",
                                "Recreational_Catch_Diversity" = "Recreational_Catch_Diversity",
                                "Zooplankton_Diversity" = "Zooplankton_Diversity",
                                "Bottom_Temp" = "Bottom_Temp",
                                "Heatwave_Cumulative_Intensity" = "Heatwave_Cumulative_Intensity",
                                "Heatwave_Maximum_Intensity" = "Heatwave_Maximum_Intensity"),
                    selected = NULL,multiple = F),
      ), 

     
      conditionalPanel( # GB #
        condition = "input.epu_abbr == 'GB'", 
        selectInput(inputId="Indicator",label="Choose SOE Indicator",
                    choices = c("Revenue_Managed"="Revenue_Managed",
                      "Landings_Managed"="Landings_Managed",
                      #"Recreational_Seafood_Harvest" = "Recreational_Seafood_Harvest",
                      #"Recreational_Effort" = "Recreational_Effort",
                      #"Recreational_Catch_Diversity" = "Recreational_Catch_Diversity",
                      "Zooplankton_Diversity" = "Zooplankton_Diversity",
                      "Bottom_Temp" = "Bottom_Temp",
                      "Heatwave_Cumulative_Intensity" = "Heatwave_Cumulative_Intensity",
                      "Heatwave_Maximum_Intensity" = "Heatwave_Maximum_Intensity"),
                      selected = "Revenue_Managed",multiple = F),
        ), 
      conditionalPanel( # GOM #
        condition = "input.epu_abbr == 'GOM'", 
        selectInput(inputId="Indicator",label="Choose SOE Indicator",
                    choices = c("Revenue_Managed"="Revenue_Managed",
                      "Landings_Managed"="Landings_Managed",
                      #"Recreational_Seafood_Harvest" = "Recreational_Seafood_Harvest",
                      #"Recreational_Effort" = "Recreational_Effort",
                      #"Recreational_Catch_Diversity" = "Recreational_Catch_Diversity",
                      "Zooplankton_Diversity" = "Zooplankton_Diversity",
                      "Bottom_Temp" = "Bottom_Temp",
                      "Heatwave_Cumulative_Intensity" = "Heatwave_Cumulative_Intensity",
                      "Heatwave_Maximum_Intensity" = "Heatwave_Maximum_Intensity"),
                      selected = "Revenue_Managed",multiple = F),
        ), 
      conditionalPanel( # Shelfwide #
        condition = "input.epu_abbr == 'All'", 
        selectInput(inputId="Indicator",label="Choose SOE Indicator",
                    choices = c(#"Revenue_Managed"="Revenue_Managed",
                      #"Landings_Managed"="Landings_Managed",
                      #"Recreational_Seafood_Harvest" = "Recreational_Seafood_Harvest",
                      #"Recreational_Effort" = "Recreational_Effort",
                      #Recreational_Catch_Diversity" = "Recreational_Catch_Diversity",
                      #"Zooplankton_Diversity" = "Zooplankton_Diversity",
                      #"Bottom_Temp" = "Bottom_Temp",
                      #"Heatwave_Cumulative_Intensity" = "Heatwave_Cumulative_Intensity",
                      #"Heatwave_Maximum_Intensity" = "Heatwave_Maximum_Intensity"),
                      "LTSST"="LTSST"),
                      selected = "LTSST",multiple = F),
        ), 
    ),


    #   selectInput(inputId="Indicator",label="Choose SOE Indicator",
    #               choices = c("Revenue_Managed"="Revenue_Managed",
    #                           "Landings_Managed"="Landings_Managed",
    #                           "Recreational_Seafood_Harvest" = "Recreational_Seafood_Harvest",
    #                           "Recreational_Effort" = "Recreational_Effort",
    #                           "Recreational_Catch_Diversity" = "Recreational_Catch_Diversity",
    #                           "Zooplankton_Diversity" = "Zooplankton_Diversity",
    #                           "Bottom_Temp" = "Bottom_Temp",
    #                           "Heatwave_Cumulative_Intensity" = "Heatwave_Cumulative_Intensity",
    #                           "Heatwave_Maximum_Intensity" = "Heatwave_Maximum_Intensity",
    #                           "LTSST"="LTSST"),
    #               selected = "LTSST",multiple = F),
    # ),

    # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(type = "tabs",

      # Output: Histogram ----
      tabPanel("Plot/Summary", 
               plotOutput(outputId = "timeseries"),
               DT::dataTableOutput("summarytable")), 
               #plotOutput(outputId = "residuals")),
      tabPanel("Table",
               DT::dataTableOutput("tableout")),#, 
               #fluidPage(downloadButton('downloadData', "Download data"))), # in a tabitem)), 
      tabPanel("Documentation",
               htmlOutput("markdown"))#,
      #tabPanel("Summary Stats",
       #        htmlOutput("markdown"))

    )
    )
  )
  )
)



























# 
# 
# 
# fluidPage(
#   h1("State of the Ecosystem Trend Analysis"),
#     
#     # indicator rendered from package ----
#     tabPanel(
#       sidebarPanel(
#       "Trend Analysis",
#       h3("Choose Region/EPU"),
#       selectInput(inputId="epu_abbr",
#                   label="Choose Region",
#                   choices = c("Mid-Atlantic Bight"="MAB",
#                               "New England"="NE", 
#                               "Georges Bank" = "GB", 
#                               "Gulf of Maine" = "GOM", 
#                               "Shelfwide"="All"),
#                   selected = "All",multiple = F),
#       
#       h3("Choose SOE Indicator"),
#       selectInput(inputId="Indicator",
#                   label="Choose SOE indicator",
#                   choices = c("Revenue_Managed"="Revenue_Managed",
#                               "Landings_Managed"="Landings_Managed", 
#                               "Recreational_Seafood_Harvest" = "Recreational_Seafood_Harvest", 
#                               "Recreational_Effort" = "Recreational_Effort",
#                               "Recreational_Catch_Diversity" = "Recreational_Catch_Diversity",
#                               "Zooplankton_Diversity" = "Zooplankton_Diversity", 
#                               "Bottom_Temp" = "Bottom_Temp", 
#                               "Heatwave_Cumulative_Intensity" = "Heatwave_Cumulative_Intensity", 
#                               "Heatwave_Maximum_Intensity" = "Heatwave_Maximum_Intensity", 
#                               "LTSST"="LTSST"),
#                   selected = "LTSST",multiple = F),
#       
#       h3("Choose Model"),
#       selectInput(inputId="Model",
#                   label="Choose model type",
#                   choices = c("SOE_Standard"="SOE_Standard", "GAM"="GAM"),
#                   selected = "SOE_Standard",multiple = F),
#       
#       h3("Choose number of knots"),
#       sliderInput(inputId = "knots",
#                   label = "Number of knots:",
#                   min = 0,
#                   max = 30,
#                   value = 1, 
#                   step = 1),
#       
#       h3("Choose smoother value"),
#       sliderInput(inputId = "gamma",
#                   label = "Smoother Function:",
#                   min = 0,
#                   max = 10,
#                   value = 1, 
#                   step = 0.01)
#       ),
#       mainPanel(
#         plotOutput(outputId = "timeseries"),
#       
#         h4("AIC"),
#         verbatimTextOutput(outputId = "modelSummary"),
#   
#         h4("Documentation"),
#         htmlOutput("markdown")
#       )
#     )
# )
# )
# 
