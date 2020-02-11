library(shiny)
library(deSolve)

plotSIER <- function(beta = 0.00000009788, gamma = 0.5, S0 = 6800000, I0 = 10, R0 = 0, 
                    tmax = 100,theta = 0.25,e0 = 5000)#theta = 0,e0 = 0
  
  
  {
  times <- seq(0, tmax, 1)
  
  parameters = c(beta = beta, gamma = gamma, theta = theta)#thera = theta
  
  iniState <- c(S = S0, I = I0, R = R0, e0= e0)#e0 = e0
  
  solution <- ode(iniState, times, SIERmodel, parameters)
  
  par(mar = c(5, 5, 0, 0), oma = c(0, 0, 1, 1), mgp = c(2.5, 1, 0), xpd = T)
  plot(NA, xlab = "Time", ylab = "Number of individuals", ylim = c(0, 200), 
       xlim = c(0, 60), cex.lab = 2)
  lines(x = solution[, "time"], y = solution[, "S"], col = "black", lwd = 3)
  lines(x = solution[, "time"], y = solution[, "I"], col = "red", lwd = 3)
  lines(x = solution[, "time"], y = solution[, "R"], col = "blue", lwd = 3)
  lines(x = solution[, "time"], y = solution[, "e0"], col = "green", lwd = 3)

  legend(x = 'top', y = 1100, legend = c("S", "I", "R","E"), #E
         col = c("black", "red", "blue","green"), lwd = 3, horiz = T, bty = "n", cex = 2) 
  
  }

SIERmodel <- function(times, state, parameters) {
  with (as.list(c(state, parameters)), {
    dS <- - beta * I * S 
    dI <- theta * e0 - gamma*I #beta * I * S - gamma * I -
    dR <- gamma * I
    dE <- beta * I * S - theta * e0
    return(list(c(dS, dI, dR,dE)))
  })
}

app <- shinyApp(
  ui = fluidPage(
    titlePanel("SIER Model"),
    
    sidebarLayout(
      sidebarPanel(      
        sliderInput("transmission", "Transmission Rate:",
                    min = 0, max = 0.001,
                    value = 0.02, step = 0.000001),
        
        sliderInput("recovery", "Recovery Rate:",
                    min = 0, max = 1,
                    value = 0.5, step = 0.001),
        
        sliderInput("S0", "Initial Susceptible Individuals:",
                    min = 100, max = 1000,
                    value = 800, step = 100),
        
        sliderInput("I0", "Initial Infected Individuals:",
                    min = 1, max = 100,
                    value = 10, step = 1),
        
        sliderInput("tmax", "Time:",
                    min = 100, max = 1000,
                    value = 200, step = 100),
        
        sliderInput("e0", "Closely affected by Infected:",
                    min = 0, max = 100,
                    value = 0.02, step = 0.0005),
        sliderInput("theta", "Exposure Rate:",
                    min = 0, max = 1,
                    value = 0.01, step = 0.05)
        
      ),
      
      mainPanel(
        plotOutput("plot")
      ),
      
      "left"
    )
  ),
  
  server = function(input, output) {
    output$plot <- renderPlot({
      plotSIER(beta = input$transmission, gamma = input$recovery, S0 = input$S0, 
              I0 = input$I0, R0 = 0, tmax = input$tmax, e0 = input$e0, theta = input$theta )
    })
  }
)

runApp(app)