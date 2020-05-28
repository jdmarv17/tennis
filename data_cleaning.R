library(tidyverse)

## load in all data from 2017
## can source this file to do all of this data cleaning automatically


# 2017
wimbledonpoints2017 <- read_csv("data/2017-wimbledon-points.csv")
wimbledonmatches2017 <- read_csv("data/2017-wimbledon-matches.csv")
wimbledon2017 <- left_join(wimbledonmatches2017,
  wimbledonpoints2017, by = "match_id")

usopenpoints2017 <- read_csv("data/2017-usopen-points.csv")
usopenmatches2017 <- read_csv("data/2017-usopen-matches.csv")
usopen2017 <- left_join(usopenmatches2017, usopenpoints2017, by = "match_id")

ausopenpoints2017 <- read_csv("data/2017-ausopen-points.csv")
ausopenmatches2017 <- read_csv("data/2017-ausopen-matches.csv")
ausopen2017 <- left_join(ausopenmatches2017, ausopenpoints2017, by = "match_id")

frenchopenpoints2017 <- read_csv("data/2017-frenchopen-points.csv")
frenchopenmatches2017 <- read_csv("data/2017-frenchopen-matches.csv")
frenchopen2017 <- left_join(frenchopenmatches2017, frenchopenpoints2017, by = "match_id")

# 2018
wimbledonpoints2018 <- read_csv("data/2018-wimbledon-points.csv")
wimbledonmatches2018 <- read_csv("data/2018-wimbledon-matches.csv")
wimbledon2018 <- left_join(wimbledonmatches2018,
                           wimbledonpoints2018, by = "match_id")

usopenpoints2018 <- read_csv("data/2018-usopen-points.csv")
usopenmatches2018 <- read_csv("data/2018-usopen-matches.csv")
usopen2018 <- left_join(usopenmatches2018, usopenpoints2018, by = "match_id")

ausopenpoints2018 <- read_csv("data/2018-ausopen-points.csv")
ausopenmatches2018 <- read_csv("data/2018-ausopen-matches.csv")
ausopen2018 <- left_join(ausopenmatches2018, ausopenpoints2018, by = "match_id")

frenchopenpoints2018 <- read_csv("data/2018-frenchopen-points.csv")
frenchopenmatches2018 <- read_csv("data/2018-frenchopen-matches.csv")
frenchopen2018 <- left_join(frenchopenmatches2018, frenchopenpoints2018, by = "match_id")


#combined data

#allslams2017 <- rbind(wimbledon2017, usopen2017, ausopen2017, frenchopen2017)
## odd warning match: not sure what it means or why it's coming up

allslams2017_18 <- rbind(wimbledon2017, usopen2017, ausopen2017, frenchopen2017, 
                         wimbledon2018, usopen2018, ausopen2018, frenchopen2018)
## Warning message:
#Lossy cast from <character> to <hms> at position(s) 301, 302, 303, 441, 442, ... (and 11 more)
# Not sure what this warning message means but it looks like they merged properly

## create variables for whether point ended in unforced error
## or winner
allslams2017$unforcedind <- allslams2017$P1UnfErr + allslams2017$P2UnfErr
allslams2017$winnerind <- allslams2017$P1Winner + allslams2017$P2Winner

allslams2017_18$unforcedind <- allslams2017_18$P1UnfErr + allslams2017_18$P2UnfErr
allslams2017_18$winnerind <- allslams2017_18$P1Winner + allslams2017_18$P2Winner
## check to make sure these seem reasonable
ggplot(data = allslams2017, aes(x = unforcedind)) + geom_bar() 
ggplot(data = allslams2017, aes(x = winnerind)) + geom_bar()

ggplot(data = allslams2017_18, aes(x = unforcedind)) + geom_bar() 
ggplot(data = allslams2017_18, aes(x = winnerind)) + geom_bar()


## create some other variables of interest
allslams2017$netpoint <- allslams2017$P1NetPoint + allslams2017$P2NetPoint
allslams2017$breaksserve <- allslams2017$P1BreakPointWon +
  allslams2017$P2BreakPointWon
allslams2017$ace <- allslams2017$P1Ace +
  allslams2017$P2Ace
allslams2017$doublefault <- allslams2017$P1DoubleFault +
  allslams2017$P2DoubleFault
allslams2017$serverwin <- as.numeric((allslams2017$PointServer == 1 & allslams2017$PointWinner == 1) | 
                                       (allslams2017$PointServer == 2 & allslams2017$PointWinner == 2))

allslams2017_18$netpoint <- allslams2017_18$P1NetPoint + allslams2017_18$P2NetPoint
allslams2017_18$breaksserve <- allslams2017_18$P1BreakPointWon +
  allslams2017_18$P2BreakPointWon
allslams2017_18$ace <- allslams2017_18$P1Ace +
  allslams2017_18$P2Ace
allslams2017_18$doublefault <- allslams2017_18$P1DoubleFault +
  allslams2017_18$P2DoubleFault
allslams2017_18$serverwin <- as.numeric((allslams2017_18$PointServer == 1 & allslams2017_18$PointWinner == 1) | 
                                          (allslams2017_18$PointServer == 2 & allslams2017_18$PointWinner == 2))


## there are some observations where nobody wins a point....these seem
## to be filler so should be removed
allslams2017 %>% filter(PointWinner != 1 & PointWinner != 2) %>%
  select(PointWinner, everything())
allslams2017 <- allslams2017 %>%
  filter(PointWinner == 1 | PointWinner == 2)

allslams2017_18 %>% filter(PointWinner != 1 & PointWinner != 2) %>%
  select(PointWinner, everything())
allslams2017_18 <- allslams2017_18 %>%
  filter(PointWinner == 1 | PointWinner == 2)


#########################################
#########################################
## Create variables so that this data set 
## can be merged with importance data set
#########################################
#########################################

## create a variable for set score
allslams2017 <- allslams2017 %>% group_by(match_id) %>% 
  mutate(settotal = cumsum(SetWinner != 0)) %>%
  mutate(setaddition = cumsum(SetWinner)) %>%
  mutate(set_score = case_when(
    settotal < 1 ~ "0-0",
    settotal == 1 & setaddition == 1 ~ "1-0",
    settotal == 1 & setaddition == 2 ~ "0-1",
    settotal == 2 & setaddition == 2 ~ "2-0",
    settotal == 2 & setaddition == 3 ~ "1-1",
    settotal == 2 & setaddition == 4 ~ "0-2",
    settotal == 3 & setaddition == 3 ~ "3-0",
    settotal == 3 & setaddition == 4 ~ "2-1",
    settotal == 3 & setaddition == 5 ~ "1-2",
    settotal == 3 & setaddition == 6 ~ "0-3",
    settotal == 4 & setaddition == 5 ~ "3-1",
    settotal == 4 & setaddition == 6 ~ "2-2",
    settotal == 4 & setaddition == 7 ~ "1-3",
    settotal == 5 & setaddition == 7 ~ "3-2",
    settotal == 5 & setaddition == 8 ~ "2-3"))

allslams2017 %>% select(set_score, everything())

allslams2017_18 <- allslams2017_18 %>% group_by(match_id) %>% 
  mutate(settotal = cumsum(SetWinner != 0)) %>%
  mutate(setaddition = cumsum(SetWinner)) %>%
  mutate(set_score = case_when(
    settotal < 1 ~ "0-0",
    settotal == 1 & setaddition == 1 ~ "1-0",
    settotal == 1 & setaddition == 2 ~ "0-1",
    settotal == 2 & setaddition == 2 ~ "2-0",
    settotal == 2 & setaddition == 3 ~ "1-1",
    settotal == 2 & setaddition == 4 ~ "0-2",
    settotal == 3 & setaddition == 3 ~ "3-0",
    settotal == 3 & setaddition == 4 ~ "2-1",
    settotal == 3 & setaddition == 5 ~ "1-2",
    settotal == 3 & setaddition == 6 ~ "0-3",
    settotal == 4 & setaddition == 5 ~ "3-1",
    settotal == 4 & setaddition == 6 ~ "2-2",
    settotal == 4 & setaddition == 7 ~ "1-3",
    settotal == 5 & setaddition == 7 ~ "3-2",
    settotal == 5 & setaddition == 8 ~ "2-3"))

allslams2017_18 %>% select(set_score, everything())

## similarly, create variables for game score and point score
allslams2017$point_score <- paste(allslams2017$P1Score,
  allslams2017$P2Score, sep="-")
allslams2017$game_score <- paste(allslams2017$P1GamesWon,
  allslams2017$P2GamesWon, sep="-")

allslams2017_18$point_score <- paste(allslams2017_18$P1Score,
                                  allslams2017_18$P2Score, sep="-")
allslams2017_18$game_score <- paste(allslams2017_18$P1GamesWon,
                                 allslams2017_18$P2GamesWon, sep="-")


#########################################
#########################################
## Load Importance Data Set and Prepare 
## for Merging
#########################################
#########################################

library(deuce)
data(atp_importance)

## create rows for 40-AD that are identical to rows of 30-40
## so that data can be merged correctly
rowsdup <- subset(atp_importance, point_score == "30-40")
rowsdup$point_score <- "40-AD"
rowsdup2 <- subset(atp_importance, point_score == "40-30")
rowsdup2$point_score <- "AD-40"

atp_importance_ad <- rbind(atp_importance, rowsdup, rowsdup2)
atp_importance5 <- atp_importance_ad %>% filter(bestof == 5) %>%
  select(-bestof)


## repeat for the wta
data(wta_importance)

rowsdupw <- subset(wta_importance, point_score == "30-40")
rowsdupw$point_score <- "40-AD"
rowsdup2w <- subset(wta_importance, point_score == "40-30")
rowsdup2w$point_score <- "AD-40"

wta_importance_ad <- rbind(wta_importance, rowsdupw, rowsdup2w)

## in the allslams2017 data set, I think women's matches are always
## numbered in the 2000s and men's are in the 1000s. Should verify this
## and then delete this comment.

allslams2017 <- allslams2017 %>%
  mutate(gender = if_else(match_num < 2000, true = "Men",
  false = "Women"))

allslams2017_18 <- allslams2017_18 %>%
  mutate(gender = if_else(match_num < 2000, true = "Men",
                          false = "Women"))

## get rid of rows that are exact duplicates
atp_importance5 <- atp_importance5 %>% distinct()

placeholder_df <- allslams2017 %>% filter(gender == "Men")
atp_df <- right_join(atp_importance5, placeholder_df,
  by = c("point_score", "game_score", "set_score"))
atp_df %>% select(importance, everything())  

placeholder_df2 <- allslams2017_18 %>% filter(gender == "Men")
atp_df2 <- right_join(atp_importance5, placeholder_df2,
                     by = c("point_score", "game_score", "set_score"))
atp_df2 %>% select(importance, everything())  


atp_df$pointid <- 1:nrow(atp_df)
atp_df2$pointid <- 1:nrow(atp_df2)

tmp_importance <-
  atp_df %>%
  filter(set_score == "2-2") %>%
  separate(game_score, into = c("score1", "score2"), sep = "-",
           convert = TRUE) %>%
  filter(score1 >= 5 & score2 >= 5) %>%
  filter(score1 != 6 | score2 != 6 & point_score == "0-0") %>% ## these were entered in as errors 
  ## when the original merge happened....they received importance values at 0-0
  ## when they should not have received importance values
  mutate(grouping = case_when(
    score1 == score2 ~ "A",
    (score1 - score2) == 1 ~ "B",
    (score2 - score1) == 1 ~ "C"
  )) %>%
  group_by(point_score, grouping) %>%
  select(grouping, score1, score2, point_score, importance, everything()) %>%
  fill(importance, .direction = "downup") %>%
  arrange(grouping, point_score) %>%
  unite("game_score", c(score1, score2), sep = "-")


tmp_importance2 <-
  atp_df2 %>%
  filter(set_score == "2-2") %>%
  separate(game_score, into = c("score1", "score2"), sep = "-",
           convert = TRUE) %>%
  filter(score1 >= 5 & score2 >= 5) %>%
  filter(score1 != 6 | score2 != 6 & point_score == "0-0") %>% ## these were entered in as errors 
  ## when the original merge happened....they received importance values at 0-0
  ## when they should not have received importance values
  mutate(grouping = case_when(
    score1 == score2 ~ "A",
    (score1 - score2) == 1 ~ "B",
    (score2 - score1) == 1 ~ "C"
  )) %>%
  group_by(point_score, grouping) %>%
  select(grouping, score1, score2, point_score, importance, everything()) %>%
  fill(importance, .direction = "downup") %>%
  arrange(grouping, point_score) %>%
  unite("game_score", c(score1, score2), sep = "-")

#fifth_set_importance <-
#  atp_df %>%
#  filter(set_score == "2-2") %>%
#  separate(game_score, into = c("score1", "score2"), sep = "-", convert = TRUE) %>%
#  filter(score1 < 5 & score2 < 5)

#no_fifth_set_importance <-
#  atp_df %>%
#  separate(game_score, into = c("score1", "score2"), sep = "-", convert = TRUE) %>%
#  filter(set_score != "2-2")

#new_atp_df <-
#  bind_rows(tmp_importance, fifth_set_importance, no_fifth_set_importance)
  
## lost some rows in creating new_atp_df using this method:
nrow(atp_df); nrow(new_atp_df)


## one strategy to get the entire data set back is to create an id column
## at the beginning. see line 142

## then, can figure out which points need to be added to the tmp_importance
## by that id column:

## this reads "filter atp_df so that it gives me all of the points that have
## point ids that are NOT (exclamation point)  in th set of point ids
## in the tmp_importance data set
nottmp_importance <- atp_df %>% filter(!pointid %in% tmp_importance$pointid)
new_atp_df <- bind_rows(tmp_importance, nottmp_importance)
nrow(new_atp_df); nrow(atp_df)

nottmp_importance2 <- atp_df2 %>% filter(!pointid %in% tmp_importance2$pointid)
new_atp_df2 <- bind_rows(tmp_importance2, nottmp_importance2)
nrow(new_atp_df2); nrow(atp_df2)

## one last thing that needs to be done to get the importance data set into 
## shape is to replace the importance values where the game score is 6-6
## and the point score is 0-0 with what they should be when there is no
## tiebreak:

## just to look at:
new_atp_df %>% filter(game_score == "6-6" & point_score == "0-0" &
    set_score == "2-2") %>%
  select(point_score, game_score, slam, everything()) 

new_atp_df2 %>% filter(game_score == "6-6" & point_score == "0-0" &
                        set_score == "2-2") %>%
  select(point_score, game_score, slam, everything())
## those importance values are too high. They should match the importance
## of points at 5-5 for those slams

new_atp_df %>% filter(game_score == "5-5" & point_score == "0-0" &
    set_score == "2-2")
## or
new_atp_df %>% filter(game_score == "7-7" & point_score == "0-0" &
    set_score == "2-2")

# make new importance column for 2-2, 6-6, 0-0 case
new_atp_df <-
  new_atp_df %>%
  mutate(importance2 = ifelse(
    set_score == "2-2" & game_score == "6-6", 0.108, importance))
# I think this is good now, havent deleted old importance column yet

new_atp_df2 <-
  new_atp_df2 %>%
  mutate(importance2 = ifelse(
    set_score == "2-2" & game_score == "6-6", 0.108, importance))
# end of merging point data, duplicates for each manipulation
# can get rid of the old ones once we know new_atp_df2 merged properly

new_atp_df2 %>%
  filter(set_score == "2-2", game_score == "7-7", point_score == "15-0")


#wta importance
placeholder_df <- allslams2017 %>% filter(gender == "Women") 
wta_df <- right_join(wta_importance_ad, placeholder_df,
    by = c("point_score", "game_score", "set_score"))
wta_df %>% select(importance, everything()) %>%
  arrange(desc(importance))

placeholder_df3 <- allslams2017_18 %>% filter(gender == "Women") 
wta_df2 <- right_join(wta_importance_ad, placeholder_df3,
                     by = c("point_score", "game_score", "set_score"))
wta_df2 %>% select(importance, everything()) %>%
  arrange(desc(importance))

nrow(wta_df); nrow(filter(allslams2017, gender == "Women"))
## no 3rd set tiebreak in some slams so this doesn't match up exactly.

nrow(wta_df2); nrow(filter(allslams2017_18, gender == "Women"))





both_df <- rbind(atp_df, wta_df)



# plots
ggplot(data = wta_df, aes(x = importance, y = netpoint)) +
  geom_point() +
  stat_smooth(method = "glm", method.args = c("binomial")) +
  stat_smooth()
ggplot(data = atp_df, aes(x = importance, y = netpoint)) +
  geom_point() +
  stat_smooth(method = "glm", method.args = c("binomial")) +
  stat_smooth()

## this is kind of interesting....why would the server
## get less likely to win the more important points??
ggplot(data = wta_df, aes(x = importance, y = serverwin)) +
  geom_jitter(height = 0.12, alpha = 0.1) +
  stat_smooth(method = "glm", method.args = c("binomial")) +
  stat_smooth()

##Kovalchik & Ingram Hot heads, cool heads, and tacticians: Measuring the mental game in tennis found this pattern as well....is it fewer aces?
##slower serves? more double faults? longer rallies? worse placement?
##more returns in?



## plot empirical probabilities for the higher importance levels:
atp_df %>% filter(importance > 0.25) %>%
  group_by(factor(importance)) %>%
  summarise(serverwinemp = mean(serverwin),
    n = n())
ggplot(data = atp_df, aes(x = importance, y = serverwin)) +
  geom_jitter(height = 0.12, alpha = 0.1) +
  stat_smooth(method = "glm", method.args = c("binomial")) +
  stat_smooth()





