#lab5.R

library(tidyverse)
library(xtable)
library(readxl)
library(ggthemes)


nhldf <- read.table("/Users/apurvashah/Documents/GitHub/stats10/lab5/NHL_20202021_teamGame.tsv", sep="\t", header=TRUE)
head(nhldf)
# Penalty Minutes Team
penalty_minutes <- nhldf[ , "team_pim"]
penal_highlow <- c("low", "high")[ as.integer(penalty_minutes >= 8) + 1 ]
team_table <- table(penal_highlow)[ c("low", "high") ]
actual_high <- team_table[2]
team_table <- team_table / sum(team_table)
## Penalty Minutes Other Team
other_penalty <- nhldf[ , "penaltyMinutes"]
other_penalty_min <- c("low", "high")[ as.integer(other_penalty >= 8) + 1 ]
other_min <- table(other_penalty_min)[ c("low", "high") ]
actual_low <- other_min[1]
other_min <- other_min / sum(other_min)

totalAandB <- sum(penal_highlow == "high" & other_penalty_min == "low")
propAandB <- totalAandB/length(penal_highlow)

xmaskB <- other_penalty_min == "low"
xcat_bruns_givenB <- penal_highlow[xmaskB]
propAgivenB <- sum(xcat_bruns_givenB == "high") / length(xcat_bruns_givenB)


###################
## Answers
###################

paste("a: Proportion of C: ", team_table[2], sep = " ")
paste("b: Proportion of D: ", team_table[1], sep = " ")
paste("c: Proportion of C and D: ", propAandB, sep = " ")
paste("d: Proportion of C given D: ", propAgivenB, sep = " ")

paste("High team penalty minutes", actual_high, sep = " ")
paste("Low Opponent team penalty minutes", actual_low, sep = " ")