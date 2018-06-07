library(shiny)
library(tidyverse)
data(cars)

# Define UI for miles per gallon app ----
ui <- fluidPage(
     
     # App title ----
     headerPanel("Ian's Special Power Test"),
     
     # Sidebar panel for inputs ----
     sidebarPanel(
          
          # Input: Test Type
          radioButtons(
               "testType", 
               label = h3("Type of Hypothesis Test"),
               choices = list("Difference of Means" = "means", "Difference of Proportions" = "proportions"), 
               selected = 'means'
          ),
          
          # Input: One sided or two sided
          radioButtons(
               "sidedness", 
               label = h3("One- or Two-Sided Test"),
               choices = list("One-Sided" = 'one', "Two-Sided" = 'two'), 
               selected = 'two'
          ),
          
          # Input: Alpha
          sliderInput(
               "significance", 
               label = h3("Significance Level"), 
               min = 0, 
               max = 0.1, 
               value = c(0.05, 0.05)
          ),
          
          # Input: Power
          sliderInput(
               "power", 
               label = h3("Power"), 
               min = 0, 
               max = 1, 
               value = c(0.8, 0.8)
          ),
          
          # Input: Effect Size
          conditionalPanel(
               condition = "input.testType == 'means'",
               sliderInput(
                    "effect", 
                    label = h3("Effect Size"), 
                    min = 0, 
                    max = 3, 
                    value = c(0.5, 0.5)
               )
          ),
          
          # Input: Proportions
          conditionalPanel(
               condition = "input.testType == 'proportions'",
               sliderInput(
                    "pi", 
                    label = h3("Proportions"), 
                    min = 0, 
                    max = 1, 
                    value = c(0.4, 0.6)
               )
          )
          
     ),
     
     # Main panel for displaying outputs ----
     mainPanel(
          
          conditionalPanel(
               condition = "output.numRanges == 1",
               plotOutput("goodGraph")
          ),
          conditionalPanel(
               condition = "output.numRanges != 1",
               h2(textOutput("result"))
          ),
          
          h6(textOutput("numRanges"))
     )
)

# Functions to be called server side
sampleSizeCalulatorMeans <- function(sig, pow, effect, k=1, sided='two'){
     if (sided == 'two') sidedness <- 2 else sidedness <- 1
     z_alpha <- qnorm(sig/sidedness, lower.tail = FALSE)
     z_beta <- qnorm(pow, lower.tail = FALSE)
     n <- ((1/k + 1) * (z_alpha - z_beta)^2) / effect^2
     
     return(n)
}

sampleSizeCalulatorProps <- function(sig, pow, pi1, pi2, k=1, sided='two'){
     if (sided == 'two') sidedness <- 2 else sidedness <- 1
     z_alpha <- qnorm(sig/sidedness, lower.tail = FALSE)
     z_beta <- qnorm(pow, lower.tail = FALSE)
     effect <- pi2 - pi1
     n <- ( ((z_alpha - z_beta)^2)*( (pi1*(1-pi1))/k + (pi2*(1-pi2))) ) / effect^2
     
     return(n)
}

sampleSizeCalulator <- function(input, k=1){
     if (input$sidedness == "one"){
          z_alpha <- qnorm(input$significance[1], lower.tail = FALSE)
     } else {
          z_alpha <- qnorm(input$significance[1] / 2, lower.tail = FALSE)
     }
     z_beta <- qnorm(input$power[1], lower.tail = FALSE)
     if (input$testType == 'means'){
          return(((1/k + 1) * (z_alpha -z_beta)^2) / input$effect[1]^2)
     } else {
          pi_portion1 <- input$pi[1] * (1 - input$pi[1])
          pi_portion2 <- input$pi[2] * (1 - input$pi[2])
          return( ( ((z_alpha - z_beta)^2)*( pi_portion1/k + pi_portion2) ) / (input$pi[1] - input$pi[2])^2 )
     }
}

numRangesSum <- function(input) {
     rangeValueInputs <- logical()
     rangeValueInputs[1] <- input$significance[2] - input$significance[1] != 0
     rangeValueInputs[2] <- input$power[2] - input$power[1] != 0
     if (input$testType == 'means'){
          rangeValueInputs[3] <- input$effect[2] - input$effect[1] != 0
     }
     
     return(sum(rangeValueInputs))
}

# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
     
     numRanges <- reactive({
          numRangesSum(input)
     })
     
     output$numRanges <- reactive({
          numRanges()
     })
     
     output$result <- reactive({
          if (numRanges() == 0) {
               temp <- sampleSizeCalulator(input)
               result <- paste("Optimal sample size for each group:", ceiling(temp))
          } else if (numRanges() > 1) {
               result <- "Please only input one (or zero) range(s) of parameters k thanks"
          }
     })
     
     dfPlotting <- reactive({
          if (numRanges() == 1){
               if (input$significance[2] - input$significance[1] != 0){
                    x <- seq(input$significance[1], input$significance[2], length.out = 5)
                    if (input$testType == 'means'){
                         y <- sampleSizeCalulatorMeans(x, input$power[1], input$effect[1], sided = input$sidedness)
                    } else {
                         y <- sampleSizeCalulatorProps(x, input$power[1], input$pi[1], input$pi[2], sided = input$sidedness)
                    }
                    
               } else if (input$power[2] - input$power[1] != 0){
                    x <- seq(input$power[1], input$power[2], length.out = 5)
                    if (input$testType == 'means'){
                         y <- sampleSizeCalulatorMeans(input$significance[1], x, input$effect[1], sided = input$sidedness)
                    } else {
                         y <- sampleSizeCalulatorProps(input$significance[1], x, input$pi[1], input$pi[2], sided = input$sidedness)
                    }
               } else {
                    x <- seq(input$effect[1], input$effect[2], length.out = 5)
                    y <- sampleSizeCalulatorMeans(input$significance[1], input$power[1], x, sided = input$sidedness)
               }
               dfPlotting <- data.frame(x,y)
          }
     })
     
     output$goodGraph <- renderPlot({
          ggplot(data = dfPlotting()) + 
               geom_line(aes(x,y)) +
               labs(y = 'Sample Size', x = '')
     })
     
}


shinyApp(ui, server)