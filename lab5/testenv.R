#lab5.R

library(tidyverse)
library(xtable)
library(readxl)
library(ggthemes)


nhldf <- read.table("/Users/apurvashah/Documents/GitHub/stats10/lab5/NHL_20202021_teamGame.tsv", sep="\t", header=TRUE)
# Penalty Minutes Team
penalty_minutes <- nhldf[ , "team_pim"]
penal_highlow <- c("low", "high")[ as.integer(penalty_minutes >= 8) + 1 ]
team_table <- table(penal_highlow)[ c("low", "high") ]
team_table <- team_table / sum(team_table)
## Penalty Minutes Other Team

other_penalty <- nhldf[ cross_ndx, "shots"]
other_penalty_min <- c("low", "high")[ as.integer(other_penalty >= 33) + 1 ]
other_min <- table(other_penalty_min)[ c("low", "high") ]
other_min <- other_min / sum(other_min)


totalAandB <- sum(penal_highlow == "high" & other_penalty_min == "low")
totalAandB
propAandB <- totalAandB/length(bruns)

xmaskB <- other_penalty_min == "low"
xcat_bruns_givenB <- penal_highlow[xmaskB]
propAgivenB <- sum(xcat_bruns_givenB == "high") / length(xcat_bruns_givenB)



paste("High Team Penalty Minutes: Proportion of G: ", team_table[2], sep = " ")
paste("Low Opponent Penalty Minutes: Proportion of H: ", other_min[1], sep = " ")
paste("c: Proportion of G and H: ", propAandB, sep = " ")
paste("d: Proportion of G given H: ", propAgivenB, sep = " ")