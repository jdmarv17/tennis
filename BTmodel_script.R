

library(tidyverse)
library(BradleyTerry2)
source("merging_matches.R")

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

# leave out 2015 to check

pred_stats[i] <- NULL
pred_stats_wta[i] <- NULL

yearvec <- c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020)

# atp
for (i in 1:length(yearvec)) {
  gs_decade_small <- 
    gs_decade %>%
    filter(winner_name %in% matches_keep$player & loser_name %in% matches_keep$player) %>%
    separate(tourney_id, into = c("year", "tourn_id"), sep = "-" ) %>%
    filter(year != yearvec[i]) 

  gs_decade_small_2015 <-
    gs_decade %>%
    filter(winner_name %in% matches_keep$player & loser_name %in% matches_keep$player) %>%
    separate(tourney_id, into = c("year", "tourn_id"), sep = "-" ) %>%
    filter(year == yearvec[i])

# wta
  wta_decade_small <-
    wta_gs_decade %>%
    filter(winner_name %in% matches_keep_wta$player & loser_name %in% matches_keep_wta$player) %>%
    separate(tourney_id, into = c("year", "tourn_id"), sep = "-") %>%
    filter(year != yearvec[i])

  wta_decade_small_2015 <-
    wta_gs_decade %>%
    filter(winner_name %in% matches_keep_wta$player & loser_name %in% matches_keep_wta$player) %>%
    separate(tourney_id, into = c("year", "tourn_id"), sep = "-" ) %>%
    filter(year == yearvec[i])
  
  # split data into winners and losers
  
  # atp
  winners_df <-
    gs_decade_small %>%
    select(winner_name, w_serveperc, w_secondserve) %>%
    mutate(win1 = 1, id = as.factor(winner_name), first_serve = w_serveperc)
  
  losers_2015 <-
    gs_decade_small_2015 %>%
    select(loser_name, l_serveperc, l_secondserve) %>%
    mutate(win2 = 0, id = as.factor(loser_name), first_serve = l_serveperc)
  
  winners_2015 <-
    gs_decade_small_2015 %>%
    select(winner_name, w_serveperc, w_secondserve) %>%
    mutate(win1 = 1, id = as.factor(winner_name), first_serve = w_serveperc)
  
  losers_df <-
    gs_decade_small %>%
    select(loser_name, l_serveperc, l_secondserve) %>%
    mutate(win2 = 0, id = as.factor(loser_name), first_serve = l_serveperc) 
  
  # wta
  winners_wta_df <-
    wta_decade_small %>%
    select(winner_name, w_serveperc, w_secondserve) %>%
    mutate(win1 = 1, id = as.factor(winner_name), first_serve = w_serveperc)
  
  losers_wta_df <-
    wta_decade_small %>%
    select(loser_name, l_serveperc, l_secondserve) %>%
    mutate(win2 = 0, id = as.factor(loser_name), first_serve = l_serveperc) 
  
  winners_wta_2015 <-
    wta_decade_small_2015 %>%
    select(winner_name, w_serveperc, w_secondserve) %>%
    mutate(win1 = 1, id = as.factor(winner_name), first_serve = w_serveperc)
  
  losers_wta_2015 <-
    wta_decade_small_2015 %>%
    select(loser_name, l_serveperc, l_secondserve) %>%
    mutate(win2 = 0, id = as.factor(loser_name), first_serve = l_serveperc)
  
  # define levels
  levels(winners_df$id)
  levels(losers_df$id)
  
  levels(winners_2015$id)
  levels(losers_2015$id)
  
  levels(winners_wta_df$id)
  levels(losers_wta_df$id)
  
  levels(winners_wta_2015$id)
  levels(losers_wta_2015$id)
  
  
  # fit models
  # atp mod with just player 
  mod1 <- BTm(cbind(win1, win2), player1 = winners_df, player2 = losers_df, 
              formula = ~ id + first_serve:id + first_serve, id = "id", data = gs_decade_small)
  
  # wta mod with just player
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
  
  
  # add player column to join with test_df
  winners_2015 <-
    winners_2015 %>%
    mutate(player = winner_name) 
  
  losers_2015 <-
    losers_2015 %>%
    mutate(player = loser_name)
  
  ## obtain predicted "abilities"
  ab1 <- winners_2015 %>% left_join(test_df, by = "player") 
  ab1 <- ab1 %>%
    mutate(ability = intercept + first_serve.y * w_serveperc)
  
  ab2 <- losers_2015 %>% left_join(test_df, by = "player")
  ab2 <- ab2 %>%
    mutate(ability = intercept + first_serve.y * l_serveperc)
  
  ## subtract player abilities to obtain the logodds that the player in ab1
  ## beats the player in ab2
  logodds <- ab1 %>% select(ability) - 
    ab2 %>% select(ability)
  
  ## backtransform to get match predictions
  pred_prob <- exp(logodds) / (1 + exp(logodds))
  
  
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
  
  
  
  # add player column to join with test_df
  winners_wta_2015 <-
    winners_wta_2015 %>%
    mutate(player = winner_name) 
  
  losers_wta_2015 <-
    losers_wta_2015 %>%
    mutate(player = loser_name)
  
  ## obtain predicted "abilities"
  ab1_wta <- winners_wta_2015 %>% left_join(test_df_wta, by = "player") 
  ab1_wta <- ab1_wta %>%
    mutate(ability = intercept + first_serve.y * w_serveperc)
  
  ab2_wta <- losers_wta_2015 %>% left_join(test_df_wta, by = "player")
  ab2_wta <- ab2_wta %>%
    mutate(ability = intercept + first_serve.y * l_serveperc)
  
  ## subtract player abilities to obtain the logodds that the player in ab1
  ## beats the player in ab2
  logodds_wta <- ab1_wta %>% select(ability) - 
    ab2_wta %>% select(ability)
  
  ## backtransform to get match predictions
  pred_prob_wta <- exp(logodds_wta) / (1 + exp(logodds_wta))
  
  
  # join pred_prob to both winners and losers 
  
  # atp
  winners_2015 <-
    bind_cols(winners_2015, pred_prob) %>%
    mutate(win = win1) %>%
    within(., rm(id, winner_name, w_serveperc, w_secondserve, win1))
  
  
  losers_2015 <-
    bind_cols(losers_2015, pred_prob) %>%
    mutate(win = win2) %>%
    within(., rm(id, loser_name, l_serveperc, l_secondserve, win2)) 
  
  
  # add categorical variable for probability ranges
  winners_2015 <-
    winners_2015 %>%
    mutate(ranges = case_when(
      ability < 0.5 ~ "tmp",
      ability >= 0.5 & ability < 0.6 ~ "fifties",
      ability >= 0.6 & ability < 0.7 ~ "sixties",
      ability >= 0.7 & ability < 0.8 ~ "seventies",
      ability >= 0.8 & ability < 0.9 ~ "eighties",
      ability >= 0.9 ~ "nineties"
    ))
  
  losers_2015 <-
    losers_2015 %>%
    mutate(ranges = case_when(
      1 - ability < 0.5 ~ "tmp",
      1 - ability >= 0.5 & 1 - ability < 0.6 ~ "fifties",
      1 - ability >= 0.6 & 1 - ability < 0.7 ~ "sixties",
      1 - ability >= 0.7 & 1 - ability < 0.8 ~ "seventies",
      1 - ability >= 0.8 & 1 - ability < 0.9 ~ "eighties",
      1 - ability >= 0.9 ~ "nineties"
    ))
  
  winners_losers_2015 <-
    bind_rows(winners_2015, losers_2015) %>%
    filter(ranges == "fifties" | ranges == "sixties" | ranges == "seventies" | ranges == "eighties" | ranges == "nineties")
  

  # wta
  winners_wta_2015 <-
    bind_cols(winners_wta_2015, pred_prob_wta) %>%
    mutate(win = win1) %>%
    within(., rm(id, winner_name, w_serveperc, w_secondserve, win1))
  
  
  losers_wta_2015 <-
    bind_cols(losers_wta_2015, pred_prob_wta) %>%
    mutate(win = win2) %>%
    within(., rm(id, loser_name, l_serveperc, l_secondserve, win2)) 
  
  
  # add categorical variable for probability ranges
  winners_wta_2015 <-
    winners_wta_2015 %>%
    mutate(ranges = case_when(
      ability < 0.5 ~ "tmp",
      ability >= 0.5 & ability < 0.6 ~ "fifties",
      ability >= 0.6 & ability < 0.7 ~ "sixties",
      ability >= 0.7 & ability < 0.8 ~ "seventies",
      ability >= 0.8 & ability < 0.9 ~ "eighties",
      ability >= 0.9 ~ "nineties"
    ))
  
  losers_wta_2015 <-
    losers_wta_2015 %>%
    mutate(ranges = case_when(
      1 - ability < 0.5 ~ "tmp",
      1 - ability >= 0.5 & 1 - ability < 0.6 ~ "fifties",
      1 - ability >= 0.6 & 1 - ability < 0.7 ~ "sixties",
      1 - ability >= 0.7 & 1 - ability < 0.8 ~ "seventies",
      1 - ability >= 0.8 & 1 - ability < 0.9 ~ "eighties",
      1 - ability >= 0.9 ~ "nineties"
    ))
  
  winners_losers_wta_2015 <-
    bind_rows(winners_wta_2015, losers_wta_2015) %>%
    filter(ranges == "fifties" | ranges == "sixties" | ranges == "seventies" | ranges == "eighties" | ranges == "nineties")
  
  # check percents
  # atp
  pred_stats[i] <-
    winners_losers_2015 %>%
    group_by(ranges) %>%
    summarise(sum = sum(win), count = n(),
              prop = mean(win))
  
  # wta
  pred_stats_wta[i] <-
    winners_losers_wta_2015 %>%
    group_by(ranges) %>%
    summarise(sum = sum(win), count = n(),
              prop = mean(win))
}
  
  
  
  
  
  
  