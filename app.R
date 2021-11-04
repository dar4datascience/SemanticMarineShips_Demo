
# Call tools needed for app -----------------------------------------------




# Create useful variables -------------------------------------------------



library(shiny)

# Define UI for application that draws a histogram
ui <- semanticPage(

  title = "Semantic Marine Ships",
  h1("Port movements analyzer"),
    main_panel(
      segment(
        ship_dropdowns_UI("conditional_dropdowns")
      ),
      segment(
        cards(
          class = "two",
          card(class = "red",
               div(class = "content",
                   div(class = "header", "Main title card 1"),
                   div(class = "meta", "Sub title card 1"),
                   div(class = "description", "More detail description card 1")
               )
          ),
          card(class = "blue",
               div(class = "content",
                   div(class = "header", "Main title card 2"),
                   div(class = "meta", "Sub title card 2"),
                   div(class = "description", "More detail description card 2")
               )
          )
        )
      ),
      grid(grid_charts,
           chart1 = plotOutput("histogram"),
           chart2 = plotOutput("plot")
      )
    ) # end of main panel
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  

# call module servers -----------------------------------------------------

  ship_dropdowns_server("conditional_dropdowns")
  
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
