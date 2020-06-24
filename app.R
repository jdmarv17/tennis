library(shiny)
library(shinythemes)
#source("plotting_script.R")


ui <- fluidPage(
  selectInput(inputId = "opponent", label = "Choose opponent(s)", choices = c(matches_keep$player), multiple = T),
  plotOutput("fedBT")
)

server <- function(input, output, session) {
  plot_df <- reactive(combined_abilities %>% filter(player %in% input$opponent))
  output$fedBT <- renderPlot({
      ggplot(plot_df(), aes(x = first_serve, y = pred_prob, colour = player)) +
      geom_line() +
      labs(x = "First Serve Percentage", y = "Predicted Match Win Probability") 
    })
}

shinyApp(ui, server)
