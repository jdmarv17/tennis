library(shiny)
library(shinythemes)
library(tidyverse)
#source("plotting_script.R")


ui <- fluidPage(
  selectInput(inputId = "player", label = "Choose player of interest", choices = c(matches_keep$player)),
  selectInput(inputId = "opponent", label = "Choose opponent(s)", choices = c(matches_keep$player), multiple = T),
  plotOutput("fedBT")
)

server <- function(input, output, session) {
    # get intercept and slope
    fed_intercept <- reactive(
      test_df %>%
      filter(player == input$player) %>%
      select(intercept))
    fed_slopes <- reactive(
      test_df %>%
      filter(player == input$player) %>%
      select(first_serve))
    
    # find max and min
    fed_decade <- reactive(
      gs_decade_small %>%
      filter(winner_name == input$player | loser_name == input$player) %>%
      mutate(f_serve = case_when(
        winner_name == input$player ~ w_serveperc,
        loser_name == input$player ~ l_serveperc)) %>%
        summarise(min = round(min(f_serve),2), max = round(max(f_serve),2)))
    
    # vector with serve range
    fed_serves <- reactive(
      c(seq(from = fed_decade()$min, to = fed_decade()$max, by = 0.01)))
    # df with fed abilities
    small_fed_abilities <- reactive (
      data.frame(ability = fed_intercept()$intercept + (fed_slopes()$first_serve*fed_serves()) 
    ))
    
    fed_ability <- reactive(
      do.call(rbind, replicate(28, small_fed_abilities()))) 
      
   # final_ability <- reactive(
    #    do.call(rbind, replicate(28, fed_ability(), simplify=FALSE)))
    
    #fed_ability <- reactive(
    #  do.call(rbind, replicate(28, (data.frame(ability = fed_intercept()$intercept + (fed_slopes()$first_serve*fed_serves())) %>%
    #                             mutate(first_serve = fed_serves())), simplify=FALSE)))
    

    #opponent_ability <- reactive(
     # left_join(mean_serve, test_df, by = "player") %>%
     # mutate(ability = intercept + (first_serve*avg_serve)) %>%
     # select(player, ability) %>%
     # filter(player != input$player))
    
    opponent_ability <- reactive(
      rep(data.frame(seq_len(nrow(left_join(mean_serve, test_df, by = "player") %>%
                         mutate(ability = intercept + (first_serve*avg_serve)) %>%
                         select(player, ability) %>%
                         filter(player != input$player))), each = ((fed_decade()$max -  fed_decade()$min + .01)*100)),))
    
    #data.frame(rep(fed_ability, times = length(matches_keep) - 1))
    combined_abilities <- reactive(
      bind_cols(opponent_ability(), fed_ability()) %>%
      mutate(fed_logodds = (ability - ability1)) %>%
      mutate(pred_prob = exp(fed_logodds) / (1 + exp(fed_logodds))))
  
  
  plot_df <- reactive(combined_abilities() %>% filter(player %in% input$opponent))
  output$fedBT <- renderPlot({
      ggplot(plot_df(), aes(x = first_serve, y = pred_prob, colour = player)) +
      geom_line() +
      labs(x = "First Serve Percentage", y = "Predicted Match Win Probability") 
    })
}

shinyApp(ui, server)
