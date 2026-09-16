# TidyTuesday Practice 1 - Videogames
# Eishan Mohammed - Second Year at SFU

# Goal 1: How does the score of the game spread look?
# Goal 2: Does Spending $60 Actually Get You a Better Game?
# Goal 3: Playtime vs Price
# Goal 4: What company makes the best games?

# Load the Data!
games <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2019/2019-07-30/video_games.csv")

# How many rows and columns are in the original
# dim(games)
# 26688    10


## Make sure the games actually have a score!
scored_games <- subset(games, !is.na(games$metascore))

### Goal 1 ###
## Histogram of the score of games

# How many rows and columns are in the ones with an actual score?
# dim(scored_games)
# 2850   10 (89% of games removed?)

hist(scored_games$metascore, col = "purple", xlab = "Metascore", main = "Game Score Distribution")
# Because most of the games have been filtered out, there is bias in this. 

summary(scored_games$metascore)


### Goal 2 ###
## Spending $60 get you a better game?

# make sure the games have a price, and a score!
priced_games <- subset(games, !is.na(games$price) & !is.na(games$metascore))

# finds a correlation between them
cor(priced_games$price, priced_games$metascore)
# Gives ~.172

# Plot it! x, y, pch (point style), color, x-label, y-label
plot(priced_games$price, priced_games$metascore, pch = 20, col = "purple", xlab = "Price of games ($)", ylab = "Score")

# lm = linear model of the score and price, from the data (priced_games)
# Finds the line of best fit
# The intercept is what the expected score for a Price $0 game is
# The slope is how many points we go up / down for every $1 spent
game_model <- lm(metascore ~ price, priced_games)
# "does the price predict the score"
# Intercept: 69.05
# Slope: 0.1788 for every $1 spent
# R-Squared = .029 --> means that only 3% of the scores are explained by price

# Prints out the game_model
summary(game_model)

# this draws the line that lm created (intercept = 69, slope = .17)
abline(game_model, v = 60, col = "red", lwd = 2)



### Goal 3 ###
## Playtime vs Price Paid

played_games <- subset(games, !is.na(games$average_playtime) & games$average_playtime > 0 & !is.na(games$price) & games$price > 0)
summary(played_games$average_playtime)

# Avg playtime is minutes, so /60 for hour, and then divide by price
played_games$hours_per_dollar <- (played_games$average_playtime / 60) / played_games$price
summary(hours_per_dollar)

#create histogram of the distribution
# use log to compress!
hist(log(played_games$hours_per_dollar), breaks = 10, col = "orange", main = "Price paid for hours of playtime")

# to find the best value games, use order
best_games <- order(hours_per_dollar, decreasing = TRUE)

# dataframe[rows, columns]
# LEFT: [1:5] means get the first 5 row numbers
# RIGHT: the columns c(...) means to only print those columns from the dataframe of played games
played_games[best_games[1:5], c("game", "price", "hours_per_dollar")]



### Goal 4 ###
## Which company makes better games?

# sort the publishers
# sort from biggest to smallest
# table counts how many games each publisher produced (how many times it occurs, because publisher is only once per row)
sorted_publishers <- sort(table(scored_games$publisher), decreasing = TRUE)

# we want to just take the names of the top publishers from the head
top_publishers_name <- names(head(sorted_publishers, 10))

# find the top games from the top publishers
# from all of the games with a score, 
top_publishers <- subset(scored_games, publisher %in% top_publishers_name)

View(games_from_top_publishers)

# make a box plot from the top publishers
boxplot(metascore ~ publisher, data = top_publishers, las = 2, ylab = "Metascore", xlab = "")

# aov tests test if the variation between publishers is significantly larger than the random variation within each publisher's game
publishers_aov <- aov(metascore ~ publisher, data = top_publishers)
summary(publishers_aov)

# If the ANOVA test rejects H0, Tukey's HSD tests every pair to see what actually differs
TukeyHSD(publishers_aov)

