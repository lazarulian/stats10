## Lab Four R Script for Ease

library(xtable)
library(tidyverse)
library(readxl)
library(ggthemes)


xdf <- read_tsv("/Users/apurvashah/Documents/GitHub/stats10/lab4/NHL_20202021_teamGame.tsv", col_names = TRUE)
head(xdf)

x <- data.frame(Team = xdf$goals, avg = xdf$assists)
head(x)

xmeanPR <- mean(x$Team)
xmeanPR

xmeanPnts <- mean(x$avg)
xmeanPnts


##### sample standard deviation of rating
xsdPR <- sd(x$Team)
xsdPR

##### sample standard deviation of total points
xsdPnts <- sd(x$avg)
xsdPnts

n <- nrow(xdf)
n

###### sample covariance
xcov <- sum((x$Team - xmeanPR) * (x$avg - xmeanPnts) ) / (n - 1)
xcov


###### sample correlation
xcorr <- xcov / (xsdPR * xsdPnts)
xcorr

###### sample regression line slope
xb1 <- xcorr * xsdPnts / xsdPR
xb1

###### sample regression line y-intercept
xb0 <- xmeanPnts - xb1 * xmeanPR
xb0

graph <- ggplot(data = x,  aes(Team, avg)) + geom_point(size = 1, color = "blue") + 
  labs(title = "NFL 2021: Average Points Scored / Game vs. QB Passer Rating",
       x = "Number of Goals", y = "Number of Assists") + theme_classic() +
  geom_abline(slope=xb1, intercept=xb0, color = "maroon")

graph


