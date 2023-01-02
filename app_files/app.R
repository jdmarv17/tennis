library(shiny)
library(shinythemes)
library(tidyverse)
library(BradleyTerry2)
library(ggthemes)
library(ggrepel)

atp2010 <- read_csv("data/atp_matches_2010.csv")
atp2011 <- read_csv("data/atp_matches_2011.csv")
atp2012 <- read_csv("data/atp_matches_2012.csv")
atp2013 <- read_csv("data/atp_matches_2013.csv")
atp2014 <- read_csv("data/atp_matches_2014.csv")
atp2015 <- read_csv("data/atp_matches_2015.csv")
atp2016 <- read_csv("data/atp_matches_2016.csv")
atp2017 <- read_csv("data/atp_matches_2017.csv")
atp2018 <- read_csv("data/atp_matches_2018.csv")
atp2019 <- read_csv("data/atp_matches_2019.csv")
atp2020 <- read_csv("data/atp_matches_2020.csv")

wta2010 <- read_csv("data/wta_matches_2010.csv")
wta2011 <- read_csv("data/wta_matches_2011.csv")
wta2012 <- read_csv("data/wta_matches_2012.csv")
wta2013 <- read_csv("data/wta_matches_2013.csv")
wta2014 <- read_csv("data/wta_matches_2014.csv")
wta2015 <- read_csv("data/wta_matches_2015.csv")
wta2016 <- read_csv("data/wta_matches_2016.csv")
wta2017 <- read_csv("data/wta_matches_2017.csv")
wta2018 <- read_csv("data/wta_matches_2018.csv")
wta2019 <- read_csv("data/wta_matches_2019.csv")
wta2020 <- read_csv("data/wta_matches_2020.csv")

# change variable type to merge
atp2019$winner_seed <- as.numeric(as.character(atp2019$winner_seed))
atp2020$winner_seed <- as.numeric(as.character(atp2020$winner_seed))

atp2019$loser_seed <- as.numeric(as.character(atp2019$loser_seed))
atp2020$loser_seed <- as.numeric(as.character(atp2020$loser_seed))

wta2016$winner_seed <- as.numeric(as.character(wta2016$winner_seed))
wta2019$winner_seed <- as.numeric(as.character(wta2019$winner_seed))
wta2020$winner_seed <- as.numeric(as.character(wta2020$winner_seed))

wta2016$loser_seed <- as.numeric(as.character(wta2016$loser_seed))
wta2019$loser_seed <- as.numeric(as.character(wta2019$loser_seed))
wta2020$loser_seed <- as.numeric(as.character(wta2020$loser_seed))

# bind all years
atp_decade <- bind_rows(atp2010, atp2011, atp2012, atp2013,atp2014, atp2015, atp2016, atp2017, 
                        atp2018, atp2019, atp2020)

wta_decade <- bind_rows(wta2010, wta2011, wta2012, wta2013, wta2014, wta2015, wta2016, wta2017, 
                        wta2018, wta2019, wta2020)

# make first and second serve variables
atp_decade <-
  atp_decade %>%
  mutate(`w_serveperc` = w_1stIn/w_svpt) %>%
  mutate(`l_serveperc` = l_1stIn/l_svpt) %>%
  mutate(`w_secondserve` = ((w_svpt - w_1stIn - w_df)/(w_svpt - w_1stIn ))) %>%
  mutate(`l_secondserve` = ((l_svpt - l_1stIn - l_df)/(l_svpt - l_1stIn )))

wta_decade <-
  wta_decade %>%
  mutate(`w_serveperc` = w_1stIn/w_svpt) %>%
  mutate(`l_serveperc` = l_1stIn/l_svpt) %>%
  mutate(`w_secondserve` = ((w_svpt - w_1stIn - w_df)/(w_svpt - w_1stIn ))) %>%
  mutate(`l_secondserve` = ((l_svpt - l_1stIn - l_df)/(l_svpt - l_1stIn )))

# filter for grand slam matches only
gs_decade <-
  atp_decade %>%
  filter(tourney_name == "Australian Open" | tourney_name == "Wimbledon" | 
           tourney_name == "Roland Garros" | tourney_name == "US Open")

wta_gs_decade <-
  wta_decade %>%
  filter(tourney_name == "Australian Open" | tourney_name == "Wimbledon" | 
           tourney_name == "Roland Garros" | tourney_name == "US Open")

# player column
gs_decade_long <-
  gs_decade %>%
  pivot_longer(c(winner_name, loser_name), names_to = "win_loss", values_to = "player")

wta_gs_decade_long <-
  wta_gs_decade %>%
  pivot_longer(c(winner_name, loser_name), names_to = "win_loss", values_to = "player")

# matches lost
losers <- 
  gs_decade_long %>%
  select(-starts_with("w_")) %>%
  filter(win_loss == "loser_name")

losers2 <- 
  wta_gs_decade_long %>%
  select(-starts_with("w_")) %>%
  filter(win_loss == "loser_name")

# matches won
winners <- 
  gs_decade_long %>%
  select(-starts_with("l_")) %>%
  filter(win_loss == "winner_name")

winners2 <- 
  wta_gs_decade_long %>%
  select(-starts_with("l_")) %>%
  filter(win_loss == "winner_name")


# bind winners and losers
gs_matches_final <- bind_rows(losers, winners)

wta_gs_final <- bind_rows(losers2, winners2)

# make just one variable for wether the player won
gs_matches_final <-
  gs_matches_final %>%
  mutate(win = case_when(
    win_loss == "loser_name" ~ 0,
    win_loss == "winner_name" ~ 1
  )) 

wta_gs_final <-
  wta_gs_final %>%
  mutate(win = case_when(
    win_loss == "loser_name" ~ 0,
    win_loss == "winner_name" ~ 1
  )) 

# one variable for first serve percentage
gs_matches_final <-
  gs_matches_final %>%
  mutate(first_serve = case_when(
    win == 0 ~ l_serveperc,
    win == 1 ~ w_serveperc
  ))

wta_gs_final <-
  wta_gs_final %>%
  mutate(first_serve = case_when(
    win == 0 ~ l_serveperc,
    win == 1 ~ w_serveperc
  ))

# one variable for second serve
gs_matches_final <-
  gs_matches_final %>%
  mutate(second_serve = case_when(
    win == 0 ~ l_secondserve,
    win == 1 ~ w_secondserve
  ))

wta_gs_final <-
  wta_gs_final %>%
  mutate(second_serve = case_when(
    win == 0 ~ l_secondserve,
    win == 1 ~ w_secondserve
  ))


# categories for rank and opponent rank
gs_matches_final <-
  gs_matches_final %>%
  mutate(top30 = case_when(
    win == 0 & loser_rank <= 30 ~ "top30",
    win == 0 & loser_rank > 30 ~ "outside_top30",
    win == 0 & is.na(loser_rank) == TRUE ~ "outside_top30",
    win == 1 & winner_rank <= 30 ~ "top30",
    win == 1 & winner_rank > 30 ~ "outside_top30",
    win == 1 & is.na(winner_rank) == TRUE ~ "outside_top30"
  )) %>%
  mutate(opponent_top30 = case_when(
    win == 0 & winner_rank <= 30 ~ "top30",
    win == 0 & winner_rank > 30 ~ "outside_top30",
    win == 0 & is.na(winner_rank) == TRUE ~ "outside_top30",
    win == 1 & loser_rank <= 30 ~ "top30",
    win == 1 & loser_rank > 30 ~ "outside_top30",
    win == 1 & is.na(loser_rank) == TRUE ~ "outside_top30"
  )) %>%
  mutate(opponent_ranking = case_when(
    win == 0 & winner_rank <= 10 ~ "top10",
    win == 0 & 10 < winner_rank & winner_rank <= 30 ~ "10_to_30",
    win == 0 & winner_rank > 30 ~ "outside_top30",
    win == 0 & is.na(winner_rank) == TRUE ~ "outside_top30",
    win == 1 & loser_rank <= 10 ~ "top10",
    win == 1 & 10 < loser_rank & loser_rank <= 30 ~ "10_to_30",
    win == 1 & loser_rank > 30 ~ "outside_top30",
    win == 1 & is.na(loser_rank) == TRUE ~ "outside_top30"))

wta_gs_final <-
  wta_gs_final %>%
  mutate(top30 = case_when(
    win == 0 & loser_rank <= 30 ~ "top30",
    win == 0 & loser_rank > 30 ~ "outside_top30",
    win == 0 & is.na(loser_rank) == TRUE ~ "outside_top30",
    win == 1 & winner_rank <= 30 ~ "top30",
    win == 1 & winner_rank > 30 ~ "outside_top30",
    win == 1 & is.na(winner_rank) == TRUE ~ "outside_top30"
  )) %>%
  mutate(opponent_top30 = case_when(
    win == 0 & winner_rank <= 30 ~ "top30",
    win == 0 & winner_rank > 30 ~ "outside_top30",
    win == 0 & is.na(winner_rank) == TRUE ~ "outside_top30",
    win == 1 & loser_rank <= 30 ~ "top30",
    win == 1 & loser_rank > 30 ~ "outside_top30",
    win == 1 & is.na(loser_rank) == TRUE ~ "outside_top30"
  )) %>%
  mutate(opponent_ranking = case_when(
    win == 0 & winner_rank <= 10 ~ "top10",
    win == 0 & 10 < winner_rank & winner_rank <= 30 ~ "10_to_30",
    win == 0 & winner_rank > 30 ~ "outside_top30",
    win == 0 & is.na(winner_rank) == TRUE ~ "outside_top30",
    win == 1 & loser_rank <= 10 ~ "top10",
    win == 1 & 10 < loser_rank & loser_rank <= 30 ~ "10_to_30",
    win == 1 & loser_rank > 30 ~ "outside_top30",
    win == 1 & is.na(loser_rank) == TRUE ~ "outside_top30"))
# end 

gs_matches_final2 <- gs_matches_final

# only keep players with enough matches
matches_keep <-
  gs_matches_final2 %>%
  group_by(player) %>%
  summarise(matches = n()) %>%
  filter(matches >= 80) 

matches_keep_wta <-
  wta_gs_final %>%
  group_by(player) %>%
  summarise(matches = n()) %>%
  filter(matches >= 80)

# read in new files
gs_decade_small <-
  read_csv("data/final_atp.csv")

wta_decade_small <-
  read_csv("data/final_wta.csv")
# separate winners and losers
gs_decade_small <-
  gs_decade_small %>%
  filter(is.na(w_serveperc) == F & is.na(l_serveperc) == F)

wta_decade_small <-
  wta_decade_small %>%
  filter(is.na(w_serveperc) == F & is.na(l_serveperc) == F)


winners_df <-
  gs_decade_small %>%
  select(winner_name, w_serveperc, w_secondserve) %>%
  mutate(win1 = 1, id = as.factor(winner_name), first_serve = w_serveperc) %>%
  mutate(player = winner_name)

losers_df <-
  gs_decade_small %>%
  select(loser_name, l_serveperc, l_secondserve) %>%
  mutate(win2 = 0, id = as.factor(loser_name), first_serve = l_serveperc) %>%
  mutate(player = loser_name) 

# wta
winners_wta_df <-
  wta_decade_small %>%
  select(winner_name, w_serveperc, w_secondserve) %>%
  mutate(win1 = 1, id = as.factor(winner_name), first_serve = w_serveperc) %>%
  mutate(player = winner_name)

losers_wta_df <-
  wta_decade_small %>%
  select(loser_name, l_serveperc, l_secondserve) %>%
  mutate(win2 = 0, id = as.factor(loser_name), first_serve = l_serveperc) %>%
  mutate(player = loser_name)


# bind 
winners_losers_df <-
  bind_cols(winners_df, losers_df)

winners_losers_wta_df <-
  bind_cols(winners_wta_df, losers_wta_df)


# define levels
ifelse(length(levels(winners_df$id)) > length(levels(losers_df$id)), 
       levels(losers_df$id) <- levels(winners_df$id), 
       ifelse(length(levels(losers_df$id)) > length(levels(winners_df$id)),
              levels(winners_df$id) <- levels(losers_df$id),
              levels(losers_df$id) <- levels(losers_df$id)))

ifelse(length(levels(winners_wta_df$id)) > length(levels(losers_wta_df$id)), 
       levels(losers_wta_df$id) <- levels(winners_wta_df$id), 
       ifelse(length(levels(losers_wta_df$id)) > length(levels(winners_wta_df$id)),
              levels(winners_wta_df$id) <- levels(losers_wta_df$id),
              levels(losers_wta_df$id) <- levels(losers_wta_df$id)))

mod1 <- BTm(cbind(win1, win2), player1 = winners_df, player2 = losers_df, 
            formula = ~ id + first_serve:id + first_serve, id = "id", data = gs_decade_small)

wta_mod1 <- BTm(cbind(win1, win2), player1 = winners_wta_df, player2 = losers_wta_df, 
                formula = ~ id + first_serve:id + first_serve, id = "id", data = wta_decade_small)


# atp
test <- mod1$coefficients  ## extract model coefficients
test2 <- names(mod1$coefficients) %>%
  str_replace("id", "") ## extract names of model coefficients
ref <- levels(mod1$player1$id)[1] ## see who is the reference group

nonref <- data.frame(name = test2, coefficient = test) ## coefficients
## for the non-reference group

serve_coef <- mod1$coefficients[names(mod1$coefficients) == "first_serve"]
## extract base serve coefficient

## create a data frame for the reference group
reference_group <- data.frame(name = c(ref, str_c(ref, ":first_serve")),
                              coefficient = c(0, 0))

test_df <- bind_rows(reference_group, nonref) %>% ## bind reference and non-reference coefficients
  separate(name, into = c("player", "term"), sep = ":") %>% ## separate names from the interaction with serve percentage
  mutate(term = case_when(is.na(term) == TRUE ~ "intercept",
                          TRUE ~ "first_serve")) %>% ## label terms intercept for intercepts and first_serve for slopes
  mutate(new_coefficient = case_when(term == "intercept" ~ coefficient,
                                     term == "first_serve" ~ coefficient + serve_coef)) %>% ## define the slope by adding the serve coefficient to the value of first_serve for a particular player (similar to what you would do in STAT 213 when you had an interaction between a categorical and quantitative variable and you wanted to make predictions)
  filter(player != "first_serve") %>% ## get rid of the slope that we just added: it's useless now
  select(player, term, new_coefficient) %>% ## only keep relevant variables
  pivot_wider(names_from = c(term), values_from = new_coefficient) ## tidy up by having each player occupy his own row
test_df <-
  test_df %>%
  mutate(tour = "ATP")

#wta
test_wta <- wta_mod1$coefficients  ## extract model coefficients
test2_wta <- names(wta_mod1$coefficients) %>%
  str_replace("id", "") ## extract names of model coefficients
ref_wta <- levels(wta_mod1$player1$id)[1] ## see who is the reference group

nonref_wta <- data.frame(name = test2_wta, coefficient = test_wta) ## coefficients
## for the non-reference group

serve_coef_wta <- wta_mod1$coefficients[names(wta_mod1$coefficients) == "first_serve"]
## extract base serve coefficient

## create a data frame for the reference group
reference_group_wta <- data.frame(name = c(ref_wta, str_c(ref_wta, ":first_serve")),
                                  coefficient = c(0, 0))

test_df_wta <- bind_rows(reference_group_wta, nonref_wta) %>% ## bind reference and non-reference coefficients
  separate(name, into = c("player", "term"), sep = ":") %>% ## separate names from the interaction with serve percentage
  mutate(term = case_when(is.na(term) == TRUE ~ "intercept",
                          TRUE ~ "first_serve")) %>% ## label terms intercept for intercepts and first_serve for slopes
  mutate(new_coefficient = case_when(term == "intercept" ~ coefficient,
                                     term == "first_serve" ~ coefficient + serve_coef_wta)) %>% ## define the slope by adding the serve coefficient to the value of first_serve for a particular player (similar to what you would do in STAT 213 when you had an interaction between a categorical and quantitative variable and you wanted to make predictions)
  filter(player != "first_serve") %>% ## get rid of the slope that we just added: it's useless now
  select(player, term, new_coefficient) %>% ## only keep relevant variables
  pivot_wider(names_from = c(term), values_from = new_coefficient) ## tidy up by having each player occupy his own row
test_df_wta <-
  test_df_wta %>%
  mutate(tour = "WTA")

# get df with opponents 
losers_df <-
  losers_df %>%
  select(first_serve, player)
winners_df <-
  winners_df %>%
  select(first_serve, player)
fed_opponents_df <-
  bind_rows(winners_df, losers_df)
fed_opponents_df <-
  fed_opponents_df[complete.cases(fed_opponents_df),]

# add player column to join with test_df
losers_wta_df <-
  losers_wta_df %>%
  select(first_serve, player)
winners_wta_df <-
  winners_wta_df %>%
  select(first_serve, player)

wta_opponents_df <-
  bind_rows(winners_wta_df, losers_wta_df)
wta_opponents_df <-
  wta_opponents_df[complete.cases(wta_opponents_df),]

# get opponent abilities with mean serve
mean_serve <-
  fed_opponents_df %>%
  group_by(player) %>%
  mutate(avg_serve = mean(first_serve)) %>%
  select(player, avg_serve)
mean_serve <-
  mean_serve[!duplicated(mean_serve),]

mean_serve_wta <-
  wta_opponents_df %>%
  group_by(player) %>%
  mutate(avg_serve = mean(first_serve)) %>%
  select(player, avg_serve)
mean_serve_wta <-
  mean_serve_wta[!duplicated(mean_serve_wta),]

# bind test_df and test_df_wta
test_df <-
  bind_rows(test_df, test_df_wta)


ui <- fluidPage(
  theme = shinytheme("superhero"),
  
  titlePanel("Match Win Probability as a Function of First Serve Percentage"),
  
  sidebarLayout(
    
    sidebarPanel(
      radioButtons(inputId = "tour", label = "Select a tour:", choices = c("ATP", "WTA")),
      
      conditionalPanel(
        condition = "input.tour == 'WTA'",
        selectInput(inputId = "playerfemale", label = "Choose player of interest", choices = c(matches_keep_wta$player), selected = "Serena Williams"),
        selectInput(inputId = "opponentfemale", label = "Choose opponent(s)", choices = c(matches_keep_wta$player), multiple = T)),  
      
      conditionalPanel(
        condition = "input.tour == 'ATP'",
        selectInput(inputId = "playermale", label = "Choose player of interest", choices = c(matches_keep$player), selected = "Roger Federer"),
        selectInput(inputId = "opponentmale", label = "Choose opponent(s)", choices = c(matches_keep$player), multiple = T)),
      
      
    ),
    
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("Plot", plotOutput("fedBT"), br(), plotOutput("fedBT2"),
                           br(),
                           "A write-up of the entire project, including further description of the app, can be found at",
                           a("https://github.com/jdmarv17/tennis/blob/master/write_up_files/first_writeup.pdf", href="https://github.com/jdmarv17/tennis/blob/master/write_up_files/first_writeup.pdf", target="_blank"),
                           br(),
                           "________________________________"),
                  
                  
                  tabPanel("Explanation", 
                           "A write-up of the entire project, including further description of the app, can be found at",
                           a("https://github.com/jdmarv17/tennis/blob/master/write_up_files/first_writeup.pdf", href="https://github.com/jdmarv17/tennis/blob/master/write_up_files/first_writeup.pdf", target="_blank"),
                           "Code for the app can be found at",
                           a("https://github.com/jdmarv17/tennis/blob/master/app_files/app.R",
                             href="https://github.com/jdmarv17/tennis/blob/master/app_files/app.R", target="_blank"),
                           textOutput("summary"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  # get intercept and slope
  fed_intercept <- reactive(
    if (input$tour == "ATP") {
      test_df %>%
        filter(player == input$playermale) %>%
        select(intercept) }
    else {
      test_df %>%
        filter(player == input$playerfemale) %>%
        select(intercept) 
    })
  
  
  fed_slopes <- reactive(
    if (input$tour == "ATP") {
      test_df %>%
        filter(player == input$playermale) %>%
        select(first_serve) }
    else {
      test_df %>%
        filter(player == input$playerfemale) %>%
        select(first_serve) 
    })
  
  
  # find max and min
  fed_decade <- reactive(
    if (input$tour == "ATP") {
      gs_decade_small %>%
        filter(winner_name == input$playermale | loser_name == input$playermale) %>%
        mutate(f_serve = case_when(
          winner_name == input$playermale ~ w_serveperc,
          loser_name == input$playermale ~ l_serveperc)) }
    else {
      wta_decade_small %>%
        filter(winner_name == input$playerfemale | loser_name == input$playerfemale) %>%
        mutate(f_serve = case_when(
          winner_name == input$playerfemale ~ w_serveperc,
          loser_name == input$playerfemale ~ l_serveperc))
    })
  
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
        filter(player != input$playermale) }
    else {
      left_join(mean_serve_wta, test_df, by = "player") %>%
        mutate(ability = intercept + (first_serve*avg_serve)) %>%
        select(player, ability, first_serve) %>%
        filter(player != input$playerfemale) }
  )
  
  
  opponent_ability <- reactive(
    tmp()[rep(seq_len(nrow(tmp())), each = 30),])  
  
  #data.frame(rep(fed_ability, times = length(matches_keep) - 1))
  combined_abilities <- reactive({
    bind_cols(final_ability(), opponent_ability()) %>%
      mutate(fed_logodds = (ability...1 - ability...4)) %>%
      mutate(pred_prob = exp(fed_logodds) / (1 + exp(fed_logodds)))
    })
  
  
  plot_df <- reactive(
    if (input$tour == "ATP") {
      combined_abilities() %>% filter(player %in% input$opponentmale) }
    else {
      combined_abilities() %>% filter(player %in% input$opponentfemale)
    })
  
  plot_df2 <- reactive(
    if (input$tour == "ATP") {
      combined_abilities() %>% filter(player %in% matches_keep$player) }
    else {
      combined_abilities() %>% filter(player %in% matches_keep_wta$player)
    })
  
  output$summary <- renderText({
    "This application allows the user to plot lines that show predicted 
  match win probabilities for a selected player of interest against the
  selected opponents as a function of the player of interest's first 
  serve percentage. To obtain these lines, a Bradley-Terry model 
  was used which predicts the outcome of paired competitions. In
  this case the Bradley-Terry model predicts the probability of
  one player winning using a contest-level predictor of first serve percentage.
  The player of interst's serve range was used while the opponents' mean serve percentage
  was used to find these lines.
  The data used in the app was obtained from Jeff Sackmann under a Creative Commons License at https://github.com/JeffSackmann/tennis_atp and https://github.com/JeffSackmann/tennis_wta . We thank Jeff Sackmann for making this data available for use in student projects."}) # add something about opponent - 
  # mean serve
  
  
  output$fedBT <- renderPlot({
    ggplot(plot_df(), aes(x = fir_serve, y = pred_prob, colour = player, group = player)) +
      geom_line(size = 2) +
      geom_text_repel(aes(label = player), 
                      data = plot_df() %>% filter(fir_serve == max(fir_serve)),
                      xlim = c(0.85,0.9),
                      force = 25,
                      box.padding = ifelse(length(input$opponent) < 5, 2, 5),
                      segment.alpha = 0.5) +
      labs(title = "Predicted Match Win Probability Against Selected Opponents", x = "First Serve Percentage", 
           y = "Predicted Match Win Probability", colour = "Opponents") +
      coord_cartesian(ylim = c(0, 1), xlim = c(0.35, 1)) +
      theme_economist(base_size = 20) +
      theme(legend.position = "none") +
      scale_colour_viridis_d()
  })
  
  output$fedBT2 <-
    renderPlot({
      ggplot(plot_df2(), aes(x = fir_serve, y = pred_prob, group = player)) +
        geom_line(size = 2, alpha = 0.5) +
        labs(title = "Predicted Match Win Probability Against All Opponents", x = "First Serve Percentage", 
             y = "Predicted Match Win Probability") +
        coord_cartesian(ylim = c(0, 1), xlim = c(0.35, 1)) +
        theme_economist(base_size = 20) +
        theme(legend.position = "none")
      
    })
}

shinyApp(ui, server)
