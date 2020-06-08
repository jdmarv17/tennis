##bt example

## set seed so we get the same results
set.seed(06032020)

player1 <- c(rep("playerA", 10), rep("playerA", 10))
player2 <- c(rep("playerB", 10), rep("playerC", 10))

win1 <- c(1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0)
win2 <- win1 * -1 + 1

firstserve1 <- c(0.7, 0.6, 0.5, 0.4, 0.6, 0.7, 0.6, 0.5, 0.5, 0.3,
  0.9, 0.9, 0.8, 0.85, 0.7, 0.75, 0.85, 0.8, 0.7, 0.8)

## simulate some random serve percentages bewtween 0.62 and 0.7 for
## players B and C (we will only focus on player A)
firstserve2 <- runif(20, 0.62, 0.7)

df <- data.frame(win1 = win1, firstserve1 = firstserve1, player2 = player2)

## problem: overall trend is slightly negative
ggplot(data = df, aes(x = firstserve1, y = win1)) +
  geom_jitter(height = 0.1, aes(colour = player2)) +
  stat_smooth(method = "glm", method.args = c("binomial"))
## but, after accounting for player effects, trend is positive:
ggplot(data = df, aes(x = firstserve1, y = win1, colour = player2)) +
  geom_jitter(height = 0.1) +
  stat_smooth(method = "glm", method.args = c("binomial"))

## put data in the form for the Bradley-Terry Model

## need a data set with everything
fulldf <- data.frame(player1 = as.factor(player1),
  player2 = as.factor(player2), firstserve1 = firstserve1,
  firstserve2 = firstserve2)

## need a data set with just player 1 information
dfplayer1 <- data.frame(id = as.factor(player1), serve = firstserve1)

## need a data set with just player 2 information
dfplayer2 <- data.frame(id = as.factor(player2), serve = firstserve2)

## function requires the id variable to have identical levels. 
## this code just makes sure that the levels of the player1 factor
## are identical to the levels of the player2 factor

levels(dfplayer1$id) <- c("playerA", "playerB", "playerC")
levels(dfplayer2$id) <- c("playerB", "playerC", "playerA")
dfplayer2$id <- relevel(dfplayer2$id, ref = "playerA")
levels(fulldf$player1) <- c("playerA", "playerB", "playerC")
levels(fulldf$player2) <- c("playerB", "playerC", "playerA")

library(BradleyTerry2)
## model that allows players to have the association between serve and 
## win probablility to be diffderent for different players (interaction)
btmod <- BTm(cbind(win1, win2), player1 = dfplayer1, player2 = dfplayer2, formula = ~ id + serve:id + serve,
  id = "id", data = fulldf)
summary(btmod)
btmod$random
## by default, player A's intercept "effect" is 0

## plot the results

##playerA vs. playerB
x <- seq(0.3, 0.9, by = 0.01) ## give a range of serve percentages for playerA

logity <-  0 + 26.26 * x - (28.70 + (26.26 - 50.97) * 0.66) ## where is the whole term after x coming from
y <- exp(logity) / (1 + exp(logity))
player2 <- "playerB"
df1 <- data.frame(x = x, y = y, player2 = player2)

## playerA vs. playerC
x <- seq(0.3, 0.9, by = 0.01) ## give a range of serve percentages for playerA

logity <-  0 + 26.26 * x - (43.46 + (26.26 - 58.46) * 0.66)
y <- exp(logity) / (1 + exp(logity))
player2 <- "playerC"
df2 <- data.frame(x = x, y = y, player2 = player2)

final_df <- bind_rows(df1, df2)
ggplot(data = final_df, aes(x = x, y = y, colour = player2)) + 
  geom_line()

## this fixes the problem of "doubling" the number of observations and 
## also accounts for player effects (the estimated trends are now positive 
## whereas ignoring opponent resulted in a slightly negative trend).

## but, the formula won't let you specify id as a random effect and
## have different slopes for the different players,
## so it's a lot harder to "average" over players. It's possible to do
## it, just not with this package: it gives you a warning if you try:

btmod <- BTm(cbind(win1, win2), player1 = dfplayer1, player2 = dfplayer2, formula = ~ serve + (1 + serve | id),
  id = "id", data = fulldf)

## so, if you wanted to use this, you could imagine having the app let
## you choose a player and then lines for 75 different opponents would 
## appear. The user could then choose an opponent and that line could
## get highlighted a different colour, which could be pretty cool. This
## would be more difficult to do without Bradley Terry since the competition
## model leverages information about all matches (A beats B, B beats C, then
## model assumes that A beats C)
##  But, that wasn't exactly the 
## original goal, so you may not want to use this at all. You can just think
## about what you actually want to have in the end. 


