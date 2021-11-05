
# Call tools needed for app -----------------------------------------------
library(dtplyr)
library(dplyr)
library(shiny)
library(shiny.semantic)



# Create useful variables -------------------------------------------------


# Define UI for application that draws a histogram
ui <- semanticPage(

  title = "Semantic Marine Ships",
  h1("Port movements analyzer"),

# main panel begins -------------------------------------------------------

    main_panel(

# begin of selectors segment: row 1 ----------------------------------------------

      segment(
        ship_dropdowns_UI("conditional_dropdowns")
      ), #end of selectors segment

# row 2 -------------------------------------------------------------------

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
      ) # end of cards segment
    ) # end of main panel
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  

# call module servers -----------------------------------------------------

  ship_dropdowns_server("conditional_dropdowns")
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
