#############################
# Thomas Cerbelaud | Shravan Kumar Chandrasekaran | Mathilde Sirbu
# SIEO 4150 
# Homework 3 , Group problem
# Due date: 09/15
#############################

# Set the path to our folder
getwd()
dir = "Historical prices/"

source("HMWK#1.R")
library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Energy companies stocks analysis"),
  
  # Side panel design
  sidebarLayout(
    sidebarPanel(
      
      # Slide bar
      selectInput("var",
                  label = "Choose a variable to display",
                  choices = col[!col %in% c("BRENT")],
                  selected = "XOM"),
      
      # Slide bar
      selectInput("comp",
                  label = "Choose a variable to compare to",
                  choices = col[!col %in% c("BRENT")],
                  selected = "IPWR"),
      
      # Sidebar with a slider
      sliderInput("bins",
                  "Number of bins for the stock log-returns:",
                  min = 1,
                  max = 250,
                  value = 40),
      
      # Checkbox input: whether or not to superimpose the normal distribution
      checkboxInput(
        "checkbox",
        "Normal distribution"),
      
      # Checkbox input: whether or not to display the confidence interval for the mean
      checkboxInput(
        "checkbox_confidence_mean",
        "Mean confidence interval",
        value = FALSE),
      # Condition of what to display within the side panel if checkboxed is checked.
      conditionalPanel(
        condition = "input.checkbox_confidence_mean == true",
        # Sidebar with a slider for the level of confidence. If 95% is chosen for example, then the
        # confidence interval is the interval in which the value of the true mean has 95% probability
        # to be, if repeating the operation when renewing the experiment.
        sliderInput("confidence_mean",
                    "Confidence level for the mean [%]:",
                    min = 1,
                    max = 100,
                    value = 95)
      ),
      
      # Checkbox input: whether or not to display the confidence interval for the mean
      checkboxInput(
        "checkbox_confidence_variance",
        "Variance confidence interval",
        value = FALSE),
      # Condition of what to display within the side panel if checkboxed is checked.
      conditionalPanel(
        condition = "input.checkbox_confidence_variance == true",
        # Sidebar with a slider for the level of confidence. If 95% is chosen for example, then the
        # confidence interval is the interval in which the value of the true mean has 95% probability
        # to be, if repeating the operation when renewing the experiment.
        sliderInput("confidence_variance",
                    "Confidence level for the variance [%]:",
                    min = 1,
                    max = 100,
                    value = 95)
      ),
      
      # Checkbox: plot residuals or not
      checkboxInput(
        "checkbox_residuals",
        "Display residual plot",
        value = FALSE)
    ),
    
    # Show a plot of the generated distribution, Output, Main panel
    mainPanel(
      tabsetPanel(
        tabPanel("Stock analysis",  plotOutput("distPlot"),
                 # Display the confidence interval numerically
                 conditionalPanel(
                   condition = "input.checkbox_confidence_mean == true",
                   textOutput("CI_mean")),
                 
                 # Display the confidence interval numerically
                 conditionalPanel(
                   condition = "input.checkbox_confidence_variance == true",
                   textOutput("CI_variance"))),
        tabPanel("Brent-regression", plotOutput("regPlotBrent"),
                 textOutput("textResBrent"),
                 # Data of regression
                 dataTableOutput("regBrentTable")),
        tabPanel("Two stocks comparison", plotOutput("regPlotStocks"),
                 textOutput("textResStocks"),
                 dataTableOutput("regStocksTable"),
                 textOutput("twoMeans"),
                 textOutput("mean_Comp"),
                 # Display the confidence interval numerically
                 textOutput("CI_var"),
                 textOutput("CI_comp")),
        tabPanel("Time-regression", plotOutput("regPlotTime"),
                 textOutput("textResTime"),
                 # Data of regression
                 dataTableOutput("regTimeTable")),
        tabPanel("Brent prices", plotOutput("brentPlotTime"))
      )
    )
  )
))