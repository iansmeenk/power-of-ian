library(shiny)
library(tidyverse)
data(cars)

# Define UI for miles per gallon app ----
ui <- fluidPage(
     
     # App title ----
     headerPanel("Ian's Special Power Test"),
     
     # Sidebar panel for inputs ----
     sidebarPanel(
          # Input: Selector for variable to plot against mpg ----
          # selectInput("variable", "Variable:", 
          #             c("Cylinders" = "cyl",
          #               "Transmission" = "am",
          #               "Gears" = "gear")),
          # 
          
          radioButtons(
               "testType", 
               label = h3("Type of Hypothesis Test"),
               choices = list("Difference of Means" = "means", "Difference of Proportions" = "proportions"), 
               selected = 'means'
          ),
          
          radioButtons(
               "sidedness", 
               label = h3("One- or Two-Sided Test"),
               choices = list("One-Sided" = 'one', "Two-Sided" = 'two'), 
               selected = 'two'
          ),
          
          sliderInput(
               "significance", 
               label = h3("Significance Level"), 
               min = 0, 
               max = 0.1, 
               value = c(0.05, 0.05)
          ),
          
          sliderInput(
               "power", 
               label = h3("Power"), 
               min = 0, 
               max = 1, 
               value = c(0.8, 0.8)
          ),
          
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
          
          # # Output: Formatted text for caption ----
          # h3(textOutput("numRanges")),
          # 
          # # Output: Plot of the requested variable against mpg ----
          # plotOutput("mpgPlot"),
          
          
          conditionalPanel(
               condition = "output.numRanges == 1",
               plotOutput("goodGraph")
          ),
          conditionalPanel(
               condition = "output.numRanges != 1",
               h2(textOutput("result"))
          ),
          
          h3(textOutput("numRanges"))
     )
)


# mpgData <- mtcars
# mpgData$am <- factor(mpgData$am, labels = c("Automatic", "Manual"))

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
          ggplot(data = dfPlotting()) + geom_line(aes(x,y))
     })
     
     # Compute the formula text ----
     # This is in a reactive expression since it is shared by the
     # output$caption and output$mpgPlot functions
     # formulaText <- reactive({
     #      paste("mpg ~", input$variable)
     # })
     # 
     # # Return the formula text for printing as a caption ----
     # output$caption <- renderText({
     #      formulaText()
     # })
     # 
     # # You can access the values of the second widget with input$slider2, e.g.
     # output$range <- renderPrint({ input$power })
     # 
     # # Generate a plot of the requested variable against mpg ----
     # # and only exclude outliers if requested
     # output$mpgPlot <- renderPlot({
     #      boxplot(as.formula(formulaText()),
     #              data = mpgData,
     #              col = "#75AADB", pch = 19)
     # })
     
}


shinyApp(ui, server)