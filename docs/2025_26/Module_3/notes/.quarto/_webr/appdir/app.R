library(shiny)
library(ExtremalDep)

# Function to generate block maxima
generate_max <- function(N, n, rdist, param, a, b, seed=pi){
  set.seed(seed)
  Mn <- numeric(N)
  for(i in 1:N){
    sample <- rdist(n, param[1], param[2])
    Mn[i] <- (max(sample) - b) / a
  }
  return(Mn)
}

# UI
iu <- fluidPage(
  titlePanel("Block Maxima Simulation"),
  sidebarLayout(
    sidebarPanel(
      selectInput("dist", "Choose Distribution:", choices = c("Uniform(0,1)" = "uniform", "Exponential(1)" = "exponential")),
      sliderInput("n", "Observations per Block (n):", min = 5, max = 100, value = 5, step = 5),
      sliderInput("N", "Number of Blocks (N):", min = 10000, max = 50000, value = 50000, step = 10000)
    ),
    mainPanel(
      plotOutput("histogram")
    )
  )
)

# Server
server <- function(input, output) {
  output$histogram <- renderPlot({
    if (input$dist == "uniform") {
      a <- 1 / input$n
      b <- 1
      Mn <- generate_max(input$N, input$n, runif, c(0,1), a, b)
      true_gev <- function(x) dGEV(x, -1, 1, -1)
    } else {
      a <- 1
      b <- log(input$n)
      Mn <- generate_max(input$N, input$n, rgamma, c(1,1), a, b)
      true_gev <- function(x) dGEV(x, 0, 1, 0)
    }
    
    hist(Mn, breaks = "FD", col = "lightblue", prob = TRUE, main = "Histogram of Block Maxima", xlab = "Maxima")
    curve(true_gev, col = "red", lwd = 2, add = TRUE)
  })
}

# Run App
shinyApp(ui = iu, server = server)

