
# Call tools needed for app -----------------------------------------------




# Create useful variables -------------------------------------------------

# Hard code ship types since its easy
vessel_types <- c(
  "Cargo - 7",       
  "Tanker - 8",      
  "Unspecified - 0", 
  "Tug - 3",         
  "Fishing - 2",     
  "Passenger - 6",   
  "Pleasure - 9",    
  "Navigation - 1",  
  "High Special - 4"
)

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

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
