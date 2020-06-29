
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


# atp
gs_decade_small <- 
  gs_decade %>%
  filter(winner_name %in% matches_keep$player & loser_name %in% matches_keep$player) %>%
  separate(tourney_id, into = c("year", "tourn_id"), sep = "-" ) %>%
  mutate(tour = "ATP")

# wta
wta_decade_small <-
  wta_gs_decade %>%
  filter(winner_name %in% matches_keep_wta$player & loser_name %in% matches_keep_wta$player) %>%
  separate(tourney_id, into = c("year", "tourn_id"), sep = "-") %>%
  mutate(tour = "WTA")

combined_decade <-
  bind_rows(gs_decade_small, wta_decade_small)

# separate winners and losers
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


