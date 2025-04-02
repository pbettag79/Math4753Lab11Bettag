library(shiny)
library(dplyr)

# Load and preprocess data globally
data(ddt, package = "Math4753Lab11PatriceBettag")
df <- ddt %>% filter(SPECIES == "SMBUFFALO")
L <- df$LENGTH

# Optimized bootstrapping function
myboot <- function(x, fun) {
  xs <- sample(x = x, size = length(x), replace = TRUE)
  match.fun(fun)(xs)
}

# UI definition
ui <- fluidPage(
  titlePanel("Bootstrapping"),

  sidebarLayout(
    sidebarPanel(
      sliderInput("iter", "Number of iterations:", min = 100, max = 10000, value = 500, step = 100),
      sliderInput("alpha", "Alpha value:", min = 0, max = 0.2, value = 0.05, step = 0.01),
      selectInput(inputId = "fun", label = "Function to calculate stats", choices = c("mean", "var", "sd", "IQR"))
    ),

    mainPanel(
      plotOutput("bootPlot"),
      textOutput("ci")
    )
  )
)

# Server definition
server <- function(input, output) {

  # Generate raw bootstrap samples (depends only on iterations)
  samples <- reactive({
    replicate(input$iter, sample(L, replace = TRUE))
  }) %>% bindCache(input$iter)

  # Apply selected function to cached samples
  stats <- reactive({
    apply(samples(), 2, match.fun(input$fun))
  })

  # Calculate confidence intervals (depends on alpha and stats)
  ci <- reactive({
    quantile(stats(), c(input$alpha / 2, 1 - input$alpha / 2))
  })

  # Render histogram plot
  output$bootPlot <- renderPlot({
    hist_data <- hist(stats(), freq = FALSE, plot = FALSE)
    hcol <- hist_data$density / max(hist_data$density)
    hist(stats(),
         xlab = "Length",
         ylab = "Density",
         main = paste0("SMBUFFALO lengths", ", iterations=", input$iter),
         col = rgb(hcol, hcol^2, 0))
  }, res = 96)

  # Render confidence interval text
  output$ci <- renderText({
    paste("Confidence Interval:", round(ci()[1], 2), "-", round(ci()[2], 2))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
