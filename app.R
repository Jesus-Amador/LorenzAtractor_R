
setwd("~/Documentos/GitHub/LorenzR/")

library(shiny)
library(bslib)
library(deSolve)
library(plotly)


atractor <- function(t,x,params){
  sigma <- params$sigma
  rho <- params$rho
  beta <- params$beta
  dxdt <- c(sigma*(x[2]-x[1]), # dx/dt
            x[1]*(rho - x[3]) - x[2], #dy/dt
            x[1]*x[2] - beta*x[3]) #dz/dt
  return(list(dxdt))
}

# parameters
t_0 = 0
t_f = 50
x0_1 = c(2,3,0)
x0_2 = c(1,1,1)
sigma = 10
beta = 8/3

ui <- fluidPage(
    theme = bs_theme(bg = "#0a0a0a", fg = "#ffffff", primary = "#00ff88", 
                     base_font = font_collection(font_google("Poppins"))),

    titlePanel("Lorenz Atractor"),

    sidebarLayout(
      sidebarPanel(
        numericInput("rho1",paste0("\u03C1","1 value"), 25, min = 1, max = 50, step = 0.5),
        numericInput("rho2",paste0("\u03C1","2 value"), 26, min = 1, max = 50, step = 0.5),
        br(),
        helpText("Choose a diferent \u03C1's values to see system's trajectories, 
                 if you choose \u03c1 > 24.74, you will observe chaotic behavior")
        ),
        mainPanel(
          card(
            card_header("Visualización 3D"),
            plotlyOutput("LorenzPlot", height = "500px"),
            card_footer("Drag to rotate the view")
          )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  trajectories <- reactive({
    
    req(input$rho1,input$rho2)
    
    rho1_eval <- as.numeric(input$rho1)
    rho2_eval <- as.numeric(input$rho2)
    
    times <- seq(t_0, t_f, length.out = 6000)
    
    # paramentros
    parms1 <- list(sigma = sigma, rho = rho1_eval, beta = beta)
    parms2 <- list(sigma = sigma, rho = rho2_eval, beta = beta)
    
    # trajectory 1
    sol1 <- ode(y = x0_1, times = times, func = atractor, parms = parms1)
    
    # trajectory 2
    sol2 <- ode(y = x0_2, times = times, func = atractor, parms = parms2)
    
    list(
      x1 = sol1[, 2], y1 = sol1[, 3], z1 = sol1[, 4],
      x2 = sol2[, 2], y2 = sol2[, 3], z2 = sol2[, 4]
    )
  })
  
    output$LorenzPlot <- renderPlotly({
      
      data <- trajectories()
      
      plot_ly() %>% 
        add_trace(
          x = data$x1, y = data$y1, z = data$z1,
          type = "scatter3d", mode = "lines",
          name = paste0("\u03C1 = ", input$rho1),
          line = list(color = "#39FF14", width = 3)
        ) %>% 
        add_trace(
          x = data$x2, y = data$y2, z = data$z2,
          type = "scatter3d", mode = "lines",
          name = paste0("\u03C1 = ", input$rho2),
          line = list(color = "#FFFF00", width = 3)
        ) %>% 
        layout(
          paper_bgcolor = "#000000",
          plot_bgcolor = "#000000",
          scene = list(
            xaxis = list(title = "X", color = "#ffffff", gridcolor = "#333333", zerolinecolor = "#444444"),
            yaxis = list(title = "Y", color = "#ffffff", gridcolor = "#333333", zerolinecolor = "#444444"),
            zaxis = list(title = "Z", color = "#ffffff", gridcolor = "#333333", zerolinecolor = "#444444"),
            bgcolor = "#000000",
            camera = list(
              eye = list(x = 2.5, y = 2.5, z = 1.5)
            )
          ),
          title = "Comparation of two Lorenz Atractor's systems",
          legend = list(
            x = 0.8, y = 0.9,
            font = list(color = "#ffffff", size = 12),
            bgcolor = 'rgba(0, 0, 0, 0.7)',
            bordercolor = '#ffffff',
            borderwidth = 1
          )
        )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
