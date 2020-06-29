library(shiny)
library(shinythemes)
library(tidyverse)
source("plotting_script.R")


ui <- fluidPage(
  sidebarPanel(
    radioButtons(inputId = "tour", label = "Select a tour:", choices = c("ATP", "WTA")),
    
  conditionalPanel(
    condition = "input.tour == 'ATP'",
    selectInput(inputId = "player", label = "Choose player of interest", choices = c(matches_keep$player)),
    selectInput(inputId = "opponent", label = "Choose opponent(s)", choices = c(matches_keep$player), multiple = T)),
  
  conditionalPanel(
    condition = "input.tour == 'WTA'",
    selectInput(inputId = "player", label = "Choose player of interest", choices = c(matches_keep_wta$player)),
    selectInput(inputId = "opponent", label = "Choose opponent(s)", choices = c(matches_keep_wta$player), multiple = T)),
  ),
  
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
    combined_decade %>%
    filter(winner_name == input$player | loser_name == input$player) %>%
    mutate(f_serve = case_when(
      winner_name == input$player ~ w_serveperc,
      loser_name == input$player ~ l_serveperc)))
    
  fed_decade2 <- reactive(
    fed_decade() %>%
      summarise(min = round(min(f_serve), 2),
                max = round(max(f_serve), 2))
    )
    
    # vector with serve range
  fed_serves <- reactive(
    c(seq(from = fed_decade2()$min, to = fed_decade2()$max, length.out = 30)))
    # df with fed abilities
  small_fed_abilities <- reactive(
    data.frame(ability = fed_intercept()$intercept + (fed_slopes()$first_serve*fed_serves()), fir_serve = fed_serves()
    ))
      
  final_ability <- reactive(
      do.call(rbind, replicate(ifelse(input$tour == "ATP", 28, 25), small_fed_abilities(), simplify=FALSE)))

  tmp <- reactive(
    if (input$tour == "ATP") {
           left_join(mean_serve, test_df, by = "player") %>%
             mutate(ability = intercept + (first_serve*avg_serve)) %>%
             select(player, ability, first_serve) %>%
             filter(player != input$player) }
    else {
          left_join(mean_serve_wta, test_df, by = "player") %>%
            mutate(ability = intercept + (first_serve*avg_serve)) %>%
            select(player, ability, first_serve) %>%
            filter(player != input$player) }
  )
           

  opponent_ability <- reactive(
    tmp()[rep(seq_len(nrow(tmp())), each = 30),])  
    
  #data.frame(rep(fed_ability, times = length(matches_keep) - 1))
  combined_abilities <- reactive(
      bind_cols(final_ability(), opponent_ability()) %>%
      mutate(fed_logodds = (ability - ability1)) %>%
      mutate(pred_prob = exp(fed_logodds) / (1 + exp(fed_logodds))))
  
  
  plot_df <- reactive(combined_abilities() %>% filter(player %in% input$opponent))
  output$fedBT <- renderPlot({
      ggplot(plot_df(), aes(x = fir_serve, y = pred_prob, colour = player, group = player)) +
      geom_line(size = 2) +
      labs(x = "First Serve Percentage", y = "Predicted Match Win Probability") +
      coord_cartesian(ylim = c(0, 1)) +
      theme_bw() 
    #+fct_reorder2(player)
    })
  
}

shinyApp(ui, server)
