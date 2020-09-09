library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Število prestopov skozi leta"),
  
  #Levi uporabniški vmesnik
  sidebarLayout(
    sidebarPanel(width=7,
                 sliderInput("Leto", 
                             "Izberi leto", 
                             min=2010, max=2019, value=2015, step=1, 
                             sep='', 
                             animate=animationOptions(interval=650)
                 )
    ),
    mainPanel(
      plotOutput("zemljevid")
    )
  )
))
