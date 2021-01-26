# 2020NFLAnalysisPBP
An R analysis of every play during the 2020 NFL regular season

library(tidyverse)
library(dplyr)

nfl <- read.csv('NFL2020.csv')
nflframe <- data.frame(nfl)

nrow(nflframe)
## 46,189 total rows (plays) during the 2020 NFL season
ncol(nflframe)
## 45 different variables or statistical points being that of string, integer, and binary types.

sum(nflframe$IsTouchdown)

## We can see there were 1585 total touchdowns in the 2020 NFL season

sum(nflframe$IsTwoPointConversionSuccessful)

## We can also see that 66 two point conversions were converted

kcoffense <- filter(nflframe, nflframe$OffenseTeam == 'KC')
View(kcoffense)
kcsub <- select(kcoffense, c(Yards,Formation,IsTouchdown,IsInterception,IsFumble,PassType,IsRush,IsPass))
View(kcsub)
chiefs <- data.frame(kcsub)

sum(chiefs$Yards)
## The Chiefs gained 7,146 yards of total offense in 2020.

formkc <- chiefs$Formation
shotkc <- sum(formkc == 'SHOTGUN', na.rm = TRUE)
undcenkc <- sum(formkc == 'UNDER CENTER', na.rm = TRUE)
fieldgkc <- sum(formkc == 'FIELD GOAL', na.rm = TRUE)
nohudkc <- sum(formkc == 'NO HUDDLE', na.rm = TRUE)
puntkc <- sum(formkc == 'PUNT', na.rm = TRUE)

formschiefs <- matrix(c(shotkc,undcenkc,fieldgkc,nohudkc,puntkc), nrow = 5, ncol = 1)
colnames(formschiefs) <- c('Plays Used')
rownames(formschiefs) <- c('Shotgun', 'Under Center', 'Field Goal', 'No Huddle', 'Punt')
formschiefs <- as.table(formschiefs)
formschiefs

## You can clearly see the Chiefs used the Shotgun as a massive favorite, something you would expect in a West Coast/Spread Offense
sum(chiefs$IsPass) ## 663
sum(chiefs$IsRush) ## 387
kcpass <- data.frame(kcsub$PassType)
newpasskc <-data.frame(kcpass[!apply(kcpass =='', 1, all), ])

summary(newpasskc)
nrow(newpasskc)


## We clearly see the Chiefs used short passes much more frequently than going downfield with pass plays. 
## They threw short passes 82.35% of the time

gboffense <- filter(nflframe, nflframe$OffenseTeam == 'GB')
View(gboffense)
gbsub <- select(gboffense, c(Yards,Formation,IsTouchdown,IsInterception,IsFumble,PassType,IsRush,IsPass))
View(gbsub)
packers <- data.frame(gbsub)

sum(packers$IsPass) ##554
sum(packers$IsRush) ##428
gbpass <- data.frame(gbsub$PassType)
newpassgb <-data.frame(gbpass[!apply(gbpass =='', 1, all), ])
summary(newpassgb)

## In comparison, Green Bay ran the ball about 6.73% more throughout the year. They also threw the ball short at a similar rate to 
## Kansas City at about 79.24%. These two teams were both the #1 seeds respectively in the AFC and NFC so it made sense to compare
## them offensively.

