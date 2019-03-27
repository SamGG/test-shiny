# Load packages -----------------------------------------------------
library(shiny)

# Define UI ---------------------------------------------------------
ui <- pageWithSidebar(
  headerPanel("Plot de distribution statistiques"),
  
  sidebarPanel(
    selectInput("dist", "Distribution (population):",
                choices = c("Normal" = "rnorm",
                            "Uniform" = "runif")),
    br(),  # saut a la ligne
    
    # uiOutput("parameters"),  # pour plus tard
    # Ajouter ici le selecteur de valeur moyenne "mu"
    # Ajouter ici le selecteur d'ecart type "et"
    
    br(),  # saut a la ligne
    
    sliderInput("n", 
                "Nb valeurs par echantillon:", 
                value = 30,
                min = 2, 
                max = 500),
    br(),
    
    sliderInput("seed",
                "Graine aleatoire",
                value = 0,
                min = 0, 
                max = 100, animate = TRUE)
    
  ),
  
  mainPanel(
    plotOutput("thePlot"),
    br(),
    div(h3(textOutput("num.samples")), align = "center")
  )
)

# Define server function --------------------------------------------

server <- function(input, output) {
  aleatoire = reactive({
    req(input$mu, input$et, input$dist)
    if (input$dist == "gauss") {
      alea = rnorm(input$n, mean = input$mu, sd = input$et)
    }
    # if (input$dist == "runif") {
    #   alea = runif(input$n, input$min, input$max)
    # }
    alea
  })
  
  output$thePlot = renderPlot({
    req(input$mu, input$et, input$dist)
    mu = input$mu
    et = input$et
    if (input$dist == "gauss") 
      plot(curve(dnorm, mu - 5 * et, mu + 5 * et))
    # Ajouter ici l'affichage des donnees aleatoires
  })
  
}

# Create the Shiny app object ---------------------------------------
shinyApp(ui = ui, server = server)
