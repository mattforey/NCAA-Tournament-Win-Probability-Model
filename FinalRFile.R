## Load the libraries need
library(caret)
library(e1071)
library(ggplot2)
library(corrplot)
library(partykit)
library(pROC)
library(tidyverse)
library(ggimage)
library(ggplot2)

### Set a seed
set.seed(69)

### Read in the data and remove the last two rows(Empty)
data <- read.csv("Dataset.csv", header= TRUE)
data <- data[1:10518,]

### Change the 0 and 1 to loss and win and set it as a factor
data$Win <- gsub("0", "Loss", data$Win)
data$Win <- gsub("1", "Win", data$Win)
data$Win <- as.factor(data$Win)

### Create a win margin column
data$WinMargin <- data$teamscore - data$oppscore

## Check the structure of the dataset
str(data)

## Attach the data to easily refer to it later
attach(data)

### Create a correlaion plot to show collinearity between predictors
corr_data <- data[,c(13:34)]
corr_matrix <- cor(corr_data)
corrplot(corr_matrix,type = "upper")

### Create Boxplots for the variables to show distribution
par(mfrow=c(3,3))
boxplot(data$WinMargin, main = "Distribution of Win Margin", xlab = "Win Margin")
boxplot(data$SOS, main = "Distribution of Strength of Schedule", xlab = "Strength of Schedule")
boxplot(data$FTr, main = "Distribution of Free Throw Rate", xlab = "Free Throw Rate")
boxplot(data$Pace, main = "Distribution of Pace", xlab = "Pace")
boxplot(data$eFG., main = "Distribution of Effective Field Goal %", xlab = "EFG%")
boxplot(data$ORtg, main = "Distribution of Offensive Rating %", xlab = "ORtg%")
boxplot(data$AST., main = "Distribution of Assist %", xlab = "AST%")
boxplot(data$STL., main = "Distribution of Steal %", xlab = "STL%")
boxplot(data$BLK., main = "Distribution of Block %", xlab = "BLK%")
dev.off()

### Get the skewness of all the variables
skewness(data$WinMargin)
skewness(data$SOS)
skewness(data$FTr)
skewness(data$Pace)
skewness(data$eFG.)

skewness(data$ORtg)
skewness(data$AST.)
skewness(data$STL.)
skewness(data$BLK.)

### Create a QQ plot for all the variables to check for normaility
par(mfrow = c(3,3))
qqnorm(data$WinMargin, main = "Win Margin", col = 1)
qqline(data$WinMargin, main = "Win Margin")
qqnorm(data$SOS, main = "Strength of Schedule", col = 2)
qqline(data$SOS, main = "Strength of Schedule")
qqnorm(data$FTr, main = "Free Throw Rate", col = 3)
qqline(data$FTr, main = "Free Throw Rate")
qqnorm(data$Pace, main = "Pace", col = 4)
qqline(data$Pace, main = "Pace")
qqnorm(data$eFG., main = "Effective Field Goal %", col = 5)
qqline(data$eFG., main = "Effective Field Goal %")
qqnorm(data$ORtg, main = "Offensive Rating", col = 6)
qqline(data$ORtg, main = "Offensive Rating")
qqnorm(data$AST., main = "Assist %", col = 7)
qqline(data$AST., main = "Assist %")
qqnorm(data$STL., main = "Steal %", col = 8)
qqline(data$STL., main = "Steal %")
qqnorm(data$BLK., main = "Block %", col = "orange")
qqline(data$BLK., main = "Block %")
dev.off()

### Create Histograms for the variables to show distribution
par(mfrow = c(3,3))
hist(data$WinMargin, main = "Distribution of Win Margin", col = 1, xlab = "Win Margin")
hist(data$SOS, main = "Distribution of Strength of Schedule", col = 2, xlab = "Strength of Schedule")
hist(data$FTr, main = "Distribution of Free Throw Rate", col = 3, xlab = "Free Throw Rate")
hist(data$Pace, main = "Distribution of Pace", col = 4, xlab = "Pace")
hist(data$eFG., main = "Distribution of Effective Field Goal %", col = 5, xlab = "EFG%")
hist(data$ORtg, main = "Distribution of Offensive Rating", col = 6, xlab = "ORtg")
hist(data$AST., main = "Distribution of Assist %", col = 7, xlab = "AST%")
hist(data$STL., main = "Distribution of Steal %", col = 8, xlab = "STL%")
hist(data$BLK., main = "Distribution of Block %", col = 'orange', xlab = "BLK%")
dev.off()

### Create plots that show the relationships in stats for every team
teams <- read.csv("TournamentTeams.csv", header = TRUE)

### THE FOLLOWING LINES HAVE BEEN COMMENTED OUT (Lines 108-178) AS THE CSV FILE
### "TournamentTeams.csv" INCLUDES THE LOGO OF EVERY TEAM WHICH WERE
### STORED ON A LOCAL COMPUTER. THE RESULTS ARE DISPLAYED IN THE PAPER AND
### THE FILE IS NOT USED ANYWHERE ELSE




### Pace by Effective field goals
#teams %>% ggplot(aes(x=teams$Pace, y=teams$eFG.)) + geom_image(image = teams$Logo, asp = 16/9) +
 # geom_vline(xintercept = mean(teams$Pace), linetype = "dashed", color = "red") +
  #geom_hline(yintercept = mean(teams$eFG.), linetype = "dashed", color = "red") +
  #labs(y = "Effective Field Goal %",
   #    x = "Pace",
    #   caption = "Figure: Ricardo Munoz, Nathan Keckley, Matthew Forey Andrew Locker",
     #  title = "Team eFG% by Pace",
      # subtitle = "2020 Season") +
 # theme_bw() +
  #theme(
   # axis.text = element_text(size = 10),
    #axis.title.x = element_text(size = 12),
    #axis.title.y = element_text(size = 12),
    #plot.title = element_text(size = 16),
    #plot.subtitle = element_text(size = 14),
    #plot.caption = element_text(size = 10))

### Offensive rebounding % by effective field goal %
#teams %>% ggplot(aes(x=teams$ORB., y=teams$eFG.)) + geom_image(image = teams$Logo, asp = 16/9) +
 # geom_vline(xintercept = mean(teams$ORB.), linetype = "dashed", color = "red") +
  #geom_hline(yintercept = mean(teams$eFG.), linetype = "dashed", color = "red") +
  #labs(y = "Effective Field Goal %",
   #    x = "Offensive Rebounding %",
    #   caption = "Figure: Ricardo Munoz, Nathan Keckley, Matthew Forey Andrew Locker",
     #  title = "Team eFG% and ORB %",
      # subtitle = "2020 Season") +
 # theme_bw() +
  #theme(
   # axis.text = element_text(size = 10),
    #axis.title.x = element_text(size = 12),
    #axis.title.y = element_text(size = 12),
    #plot.title = element_text(size = 16),
    #plot.subtitle = element_text(size = 14),
    #plot.caption = element_text(size = 10))

### turnover by Pace%
#teams %>% ggplot(aes(x=teams$Pace, y=teams$TOV.)) + geom_image(image = teams$Logo, asp = 16/9) +
 # geom_vline(xintercept = mean(teams$Pace), linetype = "dashed", color = "red") +
  #geom_hline(yintercept = mean(teams$TOV.), linetype = "dashed", color = "red") +
  #labs(y = "Turnover %",
   #    x = "Pace",
    #   caption = "Figure: Ricardo Munoz, Nathan Keckley, Matthew Forey Andrew Locker",
     #  title = "Pace and Turnover %",
      # subtitle = "2020 Season") +
  #theme_bw() +
  #theme(
   # axis.text = element_text(size = 10),
    #axis.title.x = element_text(size = 12),
    #axis.title.y = element_text(size = 12),
    #plot.title = element_text(size = 16),
    #plot.subtitle = element_text(size = 14),
    #plot.caption = element_text(size = 10))


## Change the factors to numeric columns
data$Win <- as.factor(data$Win)
data$G <- as.numeric(data$G)
data$Overall_W <- as.numeric(data$Overall_W)
data$Overall_L <- as.numeric(data$Overall_L)
data$SRS <- as.numeric(data$SRS)
data$SOS <- as.numeric(data$SOS)
data$Home_W <- as.numeric(data$Home_W)
data$Home_L <- as.numeric(data$Home_L)
data$Away_W <- as.numeric(data$Away_W)
data$Away_L <- as.numeric(data$Away_L)
data$Tm.Total.Pts <- as.numeric(data$Tm.Total.Pts)
data$Tm.Pts_Allowed <- as.numeric(data$Tm.Pts_Allowed)
data$Pace <- as.numeric(data$Pace)
data$ORtg <- as.numeric(data$ORtg)
data$FTr <- as.numeric(data$FTr)
data$X3PAr<- as.numeric(data$X3PAr)
data$TS.<- as.numeric(data$TS.)
data$TRB.<- as.numeric(data$TRB.)
data$AST.<- as.numeric(data$AST.)
data$STL.<- as.numeric(data$STL.)
data$BLK.<- as.numeric(data$BLK.)
data$eFG.<- as.numeric(data$eFG.)
data$TOV.<- as.numeric(data$TOV.)
data$ORB.<- as.numeric(data$ORB.)
data$FT.FGA<- as.numeric(data$FT.FGA)

## Change the factors to numeric columns
data$G_2 <- as.numeric(data$G_2)
data$Overall_W_2 <- as.numeric(data$Overall_W_2)
data$Overall_L_2 <- as.numeric(data$Overall_L_2)
data$SRS_2 <- as.numeric(data$SRS_2)
data$SOS_2 <- as.numeric(data$SOS_2)
data$Home_W_2 <- as.numeric(data$Home_W_2)
data$Home_L_2 <- as.numeric(data$Home_L_2)
data$Away_W_2 <- as.numeric(data$Away_W_2)
data$Away_L_2 <- as.numeric(data$Away_L_2)
data$Tm.Total.Pts_2 <- as.numeric(data$Tm.Total.Pts_2)
data$Tm.Pts_Allowed_2 <- as.numeric(data$Tm.Pts_Allowed_2)
data$Pace_2 <- as.numeric(data$Pace_2)
data$ORtg_2 <- as.numeric(data$ORtg_2)
data$FTr_2 <- as.numeric(data$FTr_2)
data$X3PAr_2 <- as.numeric(data$X3PAr_2)
data$TS._2 <- as.numeric(data$TS._2)
data$TRB_2. <- as.numeric(data$TRB._2)
data$AST._2 <- as.numeric(data$AST._2)
data$STL._2 <- as.numeric(data$STL._2)
data$BLK._2 <- as.numeric(data$BLK._2)
data$eFG._2 <- as.numeric(data$eFG._2)
data$TOV._2 <- as.numeric(data$TOV._2)
data$ORB._2<- as.numeric(data$ORB._2)
data$FT.FGA_2<- as.numeric(data$FT.FGA_2)

### Change the names of all the teams

### NOT REQUIRED TO RUN THIS PORTION (LINES 150 - 436)

# data$team <- gsub("A&M-Corpus Christi", "Texas A&M Corpus Christi", data$team)
# data$team <- gsub("Alabama St.", "Alabama State", data$team)
# data$team <- gsub("Alcorn", "Alcorn State", data$team)
# data$team <- gsub("Appalachian St.", "Appalachian State", data$team)
# data$team <- gsub("Arizona St.", "Arizona State", data$team)
# data$team <- gsub("Ark-Pine Bluff", "Arkansas-Pine Bluff", data$team)
# data$team <- gsub("Arkansas St.", "Arkansas State", data$team)
# data$team <- gsub("Army West Point", "Army", data$team)
# data$team <- gsub("Ball St.", "Ball State", data$team)
# data$team <- gsub("Boise St.", "TBoise State", data$team)
# data$team <- gsub("Boston U.", "Boston University", data$team)
# data$team <- gsub("Bowling Green", "Bowling Green State", data$team)
# data$team <- gsub("BYU", "Brigham Young", data$team)
# data$team <- gsub("Cal St. Fullerton", "Cal State Fullerton", data$team)
# data$team <- gsub("California", "University of California", data$team)
# data$team <- gsub("Central Ark.", "Central Arkansas", data$team)
# data$team <- gsub("Central Conn. St.", "Central Connecticut State", data$team)
# data$team <- gsub("Central Mich.", "Central Michigan", data$team)
# data$team <- gsub("Charleston So.", "Charleston Southern", data$team)
# data$team <- gsub("Chicago St.", "Chicago State", data$team)
# data$team <- gsub("Cleveland St.", "Cleveland State", data$team)
# data$team <- gsub("Col. of Charleston", "College of Charleston", data$team)
# data$team <- gsub("Colorado St.", "Colorado State", data$team)
# data$team <- gsub("Copponentin St.", "TCopponentin State", data$team)
# data$team <- gsub("CSU Bakersfield", "Cal State Bakersfield", data$team)
# data$team <- gsub("CSUN", "Cal State Northridge", data$team)
# data$team <- gsub("Delaware St.", "Delaware State", data$team)
# data$team <- gsub("Eastern Ill.", "Eastern Illinois", data$team)
# data$team <- gsub("Eastern Ky.", "Eastern Kentucky", data$team)
# data$team <- gsub("Eastern Mich.", "Eastern Michigan", data$team)
# data$team <- gsub("Eastern Wash.", "Eastern Washington", data$team)
# data$team <- gsub("ETSU", "East Tennessee State", data$team)
# data$team <- gsub("FGCU", "Florida Gulf Coast", data$team)
# data$team <- gsub("FIU", "Florida International", data$team)
# data$team <- gsub("Fla. Atlantic", "Florida Atlantic", data$team)
# data$team <- gsub("Florida St.", "Florida State", data$team)
# data$team <- gsub("Fresno St.", "Fresno State", data$team)
# data$team <- gsub("Ga. Southern", "Georgia Southern", data$team)
# data$team <- gsub("Georgia St.", "Georgia State", data$team)
# data$team <- gsub("Idaho St.", "Idaho State", data$team)
# data$team <- gsub("Illinois St.", "Illinois State", data$team)
# data$team <- gsub("Indiana St.", "Indiana State", data$team)
# data$team <- gsub("Iowa St.", "Iowa State", data$team)
# data$team <- gsub("Jackson St.", "Jackson State", data$team)
# data$team <- gsub("Jacksonville St.", "Jacksonville State", data$team)
# data$team <- gsub("Kansas City", "Missouri-Kansas City", data$team)
# data$team <- gsub("Kansas St.", "Kansas State", data$team)
# data$team <- gsub("Kennesaw St.", "Kennesaw State", data$team)
# data$team <- gsub("Kent St.", "Kent State", data$team)
# data$team <- gsub("La-Monroe", "Louisiana-Monroe", data$team)
# data$team <- gsub("Lamar University", "Lamar", data$team)
# data$team <- gsub("LIU", "Long Island University", data$team)
# data$team <- gsub("LMU (CA)", "Loyola Marymount", data$team)
# data$team <- gsub("Long Beach St.", "Cal State Longbeach", data$team)
# data$team <- gsub("Loyola Chicago", "Loyola (IL)", data$team)
# data$team <- gsub("Loyola Maryland", "Loyola (MD)", data$team)
# data$team <- gsub("LSU", "Louisana State", data$team)
# data$team <- gsub("McNeese", "McNeese State", data$team)
# data$team <- gsub("Michigan St.", "Michigan State", data$team)
# data$team <- gsub("Middle Tenn.", "Middle Tennessee", data$team)
# data$team <- gsub("Mississippi St.", "Mississippi State", data$team)
# data$team <- gsub("Mississippi Val.", "Mississippi Valley State", data$team)
# data$team <- gsub("Missouri St.", "Missouri State", data$team)
# data$team <- gsub("Montana St.", "Montana State", data$team)
# data$team <- gsub("Morehead St.", "Morehead State", data$team)
# data$team <- gsub("Morgan St.", "Morgan State", data$team)
# data$team <- gsub("Murray St.", "Murray State", data$team)
# data$team <- gsub("N.C. A&T", "North Carolina A&T", data$team)
# data$team <- gsub("N.C. Central", "North Carolina Central", data$team)
# data$team <- gsub("NC State", "North Carolina State", data$team)
# data$team <- gsub("New Mexico St.", "New Mexico State", data$team)
# data$team <- gsub("Nicholls St.", "Nicholls State", data$team)
# data$team <- gsub("Norfolk St.", "Norfolk State", data$team)
# data$team <- gsub("North Ala.", "North Alabama", data$team)
# data$team <- gsub("North Dakota St.", "North Dakota State", data$team)
# data$team <- gsub("Northern Ariz.", "Northern Arizona", data$team)
# data$team <- gsub("Northern Col.", "Northern Colorado", data$team)
# data$team <- gsub("Northern Ill.", "Northern Illinois", data$team)
# data$team <- gsub("Northern Ky.", "Norther Kentucky", data$team)
# data$team <- gsub("Northwestern St.", "Northwestern State", data$team)
# data$team <- gsub("Ohio St.", "Ohio State", data$team)
# data$team <- gsub("Oklahoma St.", "Oklahoma State", data$team)
# data$team <- gsub("Ole Miss", "Mississippi", data$team)
# data$team <- gsub("Omaha", "Omaha", data$team)
# data$team <- gsub("Oregon St.", "Oregon State", data$team)
# data$team <- gsub("Penn", "Pennsylvania", data$team)
# data$team <- gsub("Penn St.", "Penn State", data$team)
# data$team <- gsub("Portland St.", "Portland State", data$team)
# data$team <- gsub("Purdue Fort Wayne", "Purdue-Fort Wayne", data$team)
# data$team <- gsub("Sacramento St.", "Sacramento State", data$team)
# data$team <- gsub("Sam Houston St.", "Sam Houston State", data$team)
# data$team <- gsub("San Diego St.", "San Diego State", data$team)
# data$team <- gsub("San Jose St.", "San Jose State", data$team)
# data$team <- gsub("Seattle U", "Seattle University", data$team)
# data$team <- gsub("SFA", "Stephen F. Austin", data$team)
# data$team <- gsub("SIUE", "SIU Edwardsville", data$team)
# data$team <- gsub("SMU", "Southern Methodist", data$team)
# data$team <- gsub("South Carolina St.", "South Carolina State", data$team)
# data$team <- gsub("South Dakota St.", "South Dakota State", data$team)
# data$team <- gsub("South Fla.", "South Florida", data$team)
# data$team <- gsub("Southeast Mo. St.", "Southeast Missouri State", data$team)
# data$team <- gsub("Southeastern La.", "Southeastern Louisiana", data$team)
# data$team <- gsub("Southern Ill.", "Southern Illinois", data$team)
# data$team <- gsub("Southern Miss.", "Southern Mississippi", data$team)
# data$team <- gsub("Southern U.", "Southern", data$team)
# data$team <- gsub("St. Francis Brooklyn", "St. Francis (NY)", data$team)
# data$team <- gsub("TCU", "Texas Christian", data$team)
# data$team <- gsub("Texas St.", "Texas State", data$team)
# data$team <- gsub("The Citadel", "Citadel", data$team)
# data$team <- gsub("UAB", "Alabama-Birmingham", data$team)
# data$team <- gsub("UC Davis", "UC-Davis", data$team)
# data$team <- gsub("UC Riverside", "UC-Riverside", data$team)
# data$team <- gsub("UC Santa Barbara", "UC-Santa Barbara", data$team)
# data$team <- gsub("UCF", "Central Florida", data$team)
# data$team <- gsub("UConn", "Connecticut", data$team)
# data$team <- gsub("UIC", "Illinois-Chicago", data$team)
# data$team <- gsub("UIW", "Incarnate Word", data$team)
# data$team <- gsub("Umass Lowell", "Massachusetts-Lowell", data$team)
# data$team <- gsub("UMBC", "Maryland-Baltimore County", data$team)
# data$team <- gsub("UMES", "Maryland-Eastern Shore", data$team)
# data$team <- gsub("UNC Asheville", "North Carolina-Asheville", data$team)
# data$team <- gsub("UNC Greensboro", "North Carolina-Greensboro", data$team)
# data$team <- gsub("UNCW", "North Carolina-Wilmington", data$team)
# data$team <- gsub("UNI", "Northern Iowa", data$team)
# data$team <- gsub("UNLV", "Nevada-Las Vegas", data$team)
# data$team <- gsub("USC Upstate", "South Carolina Upstate", data$team)
# data$team <- gsub("UT Arlington", "Texas-Arlington", data$team)
# data$team <- gsub("UT Martin", "Tennessee-Martin", data$team)
# data$team <- gsub("Utah St.", "Utah State", data$team)
# data$team <- gsub("UTEP", "Texas-El Paso", data$team)
# data$team <- gsub("UTRGV", "Texas- Rio Grande Valley", data$team)
# data$team <- gsub("UTSA", "Texas-San Antonio", data$team)
# data$team <- gsub("VCU", "Virginia Commonwealth", data$team)
# data$team <- gsub("Washington St.", "Washington State", data$team)
# data$team <- gsub("Weber St.", "Weber State", data$team)
# data$team <- gsub("Western Caro.", "Western Carolina", data$team)
# data$team <- gsub("Western Ill.", "Western Illinois", data$team)
# data$team <- gsub("Western Ky.", "Western Kentucky", data$team)
# data$team <- gsub("Western Mich.", "Western Michigan", data$team)
# data$team <- gsub("Wichita St.", "Wichita State", data$team)
# data$team <- gsub("Wright St.", "Wright State", data$team)
# data$team <- gsub("Youngstown St.", "Youngstown State", data$team)
# 
# 
# ### Change the name of opponentonentent
# data$opponent <- gsub("A&M-Corpus Christi", "Texas A&M Corpus Christi", data$opponent)
# data$opponent <- gsub("Alabama St.", "Alabama State", data$opponent)
# data$opponent <- gsub("Alcorn", "Alcorn State", data$opponent)
# data$opponent <- gsub("Appalachian St.", "Appalachian State", data$opponent)
# data$opponent <- gsub("Arizona St.", "Arizona State", data$opponent)
# data$opponent <- gsub("Ark-Pine Bluff", "Arkansas-Pine Bluff", data$opponent)
# data$opponent <- gsub("Arkansas St.", "Arkansas State", data$opponent)
# data$opponent <- gsub("Army West Point", "Army", data$opponent)
# data$opponent <- gsub("Ball St.", "Ball State", data$opponent)
# data$opponent <- gsub("Boise St.", "TBoise State", data$opponent)
# data$opponent <- gsub("Boston U.", "Boston University", data$opponent)
# data$opponent <- gsub("Bowling Green", "Bowling Green State", data$opponent)
# data$opponent <- gsub("BYU", "Brigham Young", data$opponent)
# data$opponent <- gsub("Cal St. Fullerton", "Cal State Fullerton", data$opponent)
# data$opponent <- gsub("California", "University of California", data$opponent)
# data$opponent <- gsub("Central Ark.", "Central Arkansas", data$opponent)
# data$opponent <- gsub("Central Conn. St.", "Central Connecticut State", data$opponent)
# data$opponent <- gsub("Central Mich.", "Central Michigan", data$opponent)
# data$opponent <- gsub("Charleston So.", "Charleston Southern", data$opponent)
# data$opponent <- gsub("Chicago St.", "Chicago State", data$opponent)
# data$opponent <- gsub("Cleveland St.", "Cleveland State", data$opponent)
# data$opponent <- gsub("Col. of Charleston", "College of Charleston", data$opponent)
# data$opponent <- gsub("Colorado St.", "Colorado State", data$opponent)
# data$opponent <- gsub("Copponentin St.", "TCopponentin State", data$opponent)
# data$opponent <- gsub("CSU Bakersfield", "Cal State Bakersfield", data$opponent)
# data$opponent <- gsub("CSUN", "Cal State Northridge", data$opponent)
# data$opponent <- gsub("Delaware St.", "Delaware State", data$opponent)
# data$opponent <- gsub("Eastern Ill.", "Eastern Illinois", data$opponent)
# data$opponent <- gsub("Eastern Ky.", "Eastern Kentucky", data$opponent)
# data$opponent <- gsub("Eastern Mich.", "Eastern Michigan", data$opponent)
# data$opponent <- gsub("Eastern Wash.", "Eastern Washington", data$opponent)
# data$opponent <- gsub("ETSU", "East Tennessee State", data$opponent)
# data$opponent <- gsub("FGCU", "Florida Gulf Coast", data$opponent)
# data$opponent <- gsub("FIU", "Florida International", data$opponent)
# data$opponent <- gsub("Fla. Atlantic", "Florida Atlantic", data$opponent)
# data$opponent <- gsub("Florida St.", "Florida State", data$opponent)
# data$opponent <- gsub("Fresno St.", "Fresno State", data$opponent)
# data$opponent <- gsub("Ga. Southern", "Georgia Southern", data$opponent)
# data$opponent <- gsub("Georgia St.", "Georgia State", data$opponent)
# data$opponent <- gsub("Idaho St.", "Idaho State", data$opponent)
# data$opponent <- gsub("Illinois St.", "Illinois State", data$opponent)
# data$opponent <- gsub("Indiana St.", "Indiana State", data$opponent)
# data$opponent <- gsub("Iowa St.", "Iowa State", data$opponent)
# data$opponent <- gsub("Jackson St.", "Jackson State", data$opponent)
# data$opponent <- gsub("Jacksonville St.", "Jacksonville State", data$opponent)
# data$opponent <- gsub("Kansas City", "Missouri-Kansas City", data$opponent)
# data$opponent <- gsub("Kansas St.", "Kansas State", data$opponent)
# data$opponent <- gsub("Kennesaw St.", "Kennesaw State", data$opponent)
# data$opponent <- gsub("Kent St.", "Kent State", data$opponent)
# data$opponent <- gsub("La-Monroe", "Louisiana-Monroe", data$opponent)
# data$opponent <- gsub("Lamar University", "Lamar", data$opponent)
# data$opponent <- gsub("LIU", "Long Island University", data$opponent)
# data$opponent <- gsub("LMU (CA)", "Loyola Marymount", data$opponent)
# data$opponent <- gsub("Long Beach St.", "Cal State Longbeach", data$opponent)
# data$opponent <- gsub("Loyola Chicago", "Loyola (IL)", data$opponent)
# data$opponent <- gsub("Loyola Maryland", "Loyola (MD)", data$opponent)
# data$opponent <- gsub("LSU", "Louisana State", data$opponent)
# data$opponent <- gsub("McNeese", "McNeese State", data$opponent)
# data$opponent <- gsub("Michigan St.", "Michigan State", data$opponent)
# data$opponent <- gsub("Middle Tenn.", "Middle Tennessee", data$opponent)
# data$opponent <- gsub("Mississippi St.", "Mississippi State", data$opponent)
# data$opponent <- gsub("Mississippi Val.", "Mississippi Valley State", data$opponent)
# data$opponent <- gsub("Missouri St.", "Missouri State", data$opponent)
# data$opponent <- gsub("Montana St.", "Montana State", data$opponent)
# data$opponent <- gsub("Morehead St.", "Morehead State", data$opponent)
# data$opponent <- gsub("Morgan St.", "Morgan State", data$opponent)
# data$opponent <- gsub("Murray St.", "Murray State", data$opponent)
# data$opponent <- gsub("N.C. A&T", "North Carolina A&T", data$opponent)
# data$opponent <- gsub("N.C. Central", "North Carolina Central", data$opponent)
# data$opponent <- gsub("NC State", "North Carolina State", data$opponent)
# data$opponent <- gsub("New Mexico St.", "New Mexico State", data$opponent)
# data$opponent <- gsub("Nicholls St.", "Nicholls State", data$opponent)
# data$opponent <- gsub("Norfolk St.", "Norfolk State", data$opponent)
# data$opponent <- gsub("North Ala.", "North Alabama", data$opponent)
# data$opponent <- gsub("North Dakota St.", "North Dakota State", data$opponent)
# data$opponent <- gsub("Northern Ariz.", "Northern Arizona", data$opponent)
# data$opponent <- gsub("Northern Col.", "Northern Colorado", data$opponent)
# data$opponent <- gsub("Northern Ill.", "Northern Illinois", data$opponent)
# data$opponent <- gsub("Northern Ky.", "Norther Kentucky", data$opponent)
# data$opponent <- gsub("Northwestern St.", "Northwestern State", data$opponent)
# data$opponent <- gsub("Ohio St.", "Ohio State", data$opponent)
# data$opponent <- gsub("Oklahoma St.", "Oklahoma State", data$opponent)
# data$opponent <- gsub("Ole Miss", "Mississippi", data$opponent)
# data$opponent <- gsub("Omaha", "Omaha", data$opponent)
# data$opponent <- gsub("Oregon St.", "Oregon State", data$opponent)
# data$opponent <- gsub("Penn", "Pennsylvania", data$opponent)
# data$opponent <- gsub("Penn St.", "Penn State", data$opponent)
# data$opponent <- gsub("Portland St.", "Portland State", data$opponent)
# data$opponent <- gsub("Purdue Fort Wayne", "Purdue-Fort Wayne", data$opponent)
# data$opponent <- gsub("Sacramento St.", "Sacramento State", data$opponent)
# data$opponent <- gsub("Sam Houston St.", "Sam Houston State", data$opponent)
# data$opponent <- gsub("San Diego St.", "San Diego State", data$opponent)
# data$opponent <- gsub("San Jose St.", "San Jose State", data$opponent)
# data$opponent <- gsub("Seattle U", "Seattle University", data$opponent)
# data$opponent <- gsub("SFA", "Stephen F. Austin", data$opponent)
# data$opponent <- gsub("SIUE", "SIU Edwardsville", data$opponent)
# data$opponent <- gsub("SMU", "Southern Methodist", data$opponent)
# data$opponent <- gsub("South Carolina St.", "South Carolina State", data$opponent)
# data$opponent <- gsub("South Dakota St.", "South Dakota State", data$opponent)
# data$opponent <- gsub("South Fla.", "South Florida", data$opponent)
# data$opponent <- gsub("Southeast Mo. St.", "Southeast Missouri State", data$opponent)
# data$opponent <- gsub("Southeastern La.", "Southeastern Louisiana", data$opponent)
# data$opponent <- gsub("Southern Ill.", "Southern Illinois", data$opponent)
# data$opponent <- gsub("Southern Miss.", "Southern Mississippi", data$opponent)
# data$opponent <- gsub("Southern U.", "Southern", data$opponent)
# data$opponent <- gsub("St. Francis Brooklyn", "St. Francis (NY)", data$opponent)
# data$opponent <- gsub("TCU", "Texas Christian", data$opponent)
# data$opponent <- gsub("Texas St.", "Texas State", data$opponent)
# data$opponent <- gsub("The Citadel", "Citadel", data$opponent)
# data$opponent <- gsub("UAB", "Alabama-Birmingham", data$opponent)
# data$opponent <- gsub("UC Davis", "UC-Davis", data$opponent)
# data$opponent <- gsub("UC Riverside", "UC-Riverside", data$opponent)
# data$opponent <- gsub("UC Santa Barbara", "UC-Santa Barbara", data$opponent)
# data$opponent <- gsub("UCF", "Central Florida", data$opponent)
# data$opponent <- gsub("UConn", "Connecticut", data$opponent)
# data$opponent <- gsub("UIC", "Illinois-Chicago", data$opponent)
# data$opponent <- gsub("UIW", "Incarnate Word", data$opponent)
# data$opponent <- gsub("Umass Lowell", "Massachusetts-Lowell", data$opponent)
# data$opponent <- gsub("UMBC", "Maryland-Baltimore County", data$opponent)
# data$opponent <- gsub("UMES", "Maryland-Eastern Shore", data$opponent)
# data$opponent <- gsub("UNC Asheville", "North Carolina-Asheville", data$opponent)
# data$opponent <- gsub("UNC Greensboro", "North Carolina-Greensboro", data$opponent)
# data$opponent <- gsub("UNCW", "North Carolina-Wilmington", data$opponent)
# data$opponent <- gsub("UNI", "Northern Iowa", data$opponent)
# data$opponent <- gsub("UNLV", "Nevada-Las Vegas", data$opponent)
# data$opponent <- gsub("USC Upstate", "South Carolina Upstate", data$opponent)
# data$opponent <- gsub("UT Arlington", "Texas-Arlington", data$opponent)
# data$opponent <- gsub("UT Martin", "Tennessee-Martin", data$opponent)
# data$opponent <- gsub("Utah St.", "Utah State", data$opponent)
# data$opponent <- gsub("UTEP", "Texas-El Paso", data$opponent)
# data$opponent <- gsub("UTRGV", "Texas- Rio Grande Valley", data$opponent)
# data$opponent <- gsub("UTSA", "Texas-San Antonio", data$opponent)
# data$opponent <- gsub("VCU", "Virginia Commonwealth", data$opponent)
# data$opponent <- gsub("Washington St.", "Washington State", data$opponent)
# data$opponent <- gsub("Weber St.", "Weber State", data$opponent)
# data$opponent <- gsub("Western Caro.", "Western Carolina", data$opponent)
# data$opponent <- gsub("Western Ill.", "Western Illinois", data$opponent)
# data$opponent <- gsub("Western Ky.", "Western Kentucky", data$opponent)
# data$opponent <- gsub("Western Mich.", "Western Michigan", data$opponent)
# data$opponent <- gsub("Wichita St.", "Wichita State", data$opponent)
# data$opponent <- gsub("Wright St.", "Wright State", data$opponent)
# data$opponent <- gsub("Youngstown St.", "Youngstown State", data$opponent)

### Create new variables to capture the difference in stats between both teams
attach(data)
data$FTr_Mar <- (FTr-FTr_2)
data$Pace_Mar <- (Pace-Pace_2)
data$TOV_Mar <- (TOV.-TOV._2)
data$eFG_Mar <- (eFG.-eFG._2)
data$ORB_Mar <- (ORB.-ORB._2)
data$SOS_Mar <- (SOS-SOS_2)
data$AST_Mar <- (AST.-AST._2)
data$STL_Mar <- (STL.-STL._2)

## Histogram that shows the distribution of Strength of Schedule
hist(data$SOS_Mar, main = "Distribution of SOS Margin", col = 5, xlab = "Strength of Schedule Margin")

### Create a training and testing set using feburary and march games as testing sets
train <- which(month != c(2,3))
dataTrain <- data[train,]
dataTest  <- data[-train,]

### Control function for the models
ctrl <- trainControl(method = "LGOCV",
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE,
                     savePredictions = TRUE)

### GLM Model
### ROC = 0.7875898
### Train Accuracy = 0.7003 
### Testing Accuracy = 0.6661  

set.seed(69)
glm <- train(Win~ SOS_Mar+FTr_Mar+Pace_Mar+eFG_Mar+TOV_Mar+ORB_Mar,
             data = dataTrain,
             method = "glm",
             metric = "ROC",
             trControl = ctrl)
summary(glm)
glm

### Training Set
confusionMatrix(data = predict(glm, dataTrain), reference = dataTrain$Win)

### Testing Set
confusionMatrix(data = predict(glm, dataTest), reference = as.factor(dataTest$Win))

### Get the ROC and AUC for the model and plot
dataTest$glm <- predict(glm, dataTest, type = "prob")[,1]
glmROC <- roc(dataTest$Win, dataTest$glm)
auc(glmROC)
plot(glmROC, main = "GLM Model")
legend("bottomright", c("AUC = 0.7341 ","ROC = 0.7875"))

### LDA Model
### ROC = 0.7872555
### Training Accuracy = 0.7074  
### Testing Accuracy = 0.6672 

set.seed(69)
lda <- train(Win~ SOS_Mar+FTr_Mar+Pace_Mar+eFG_Mar+TOV_Mar+ORB_Mar,
             data = dataTrain,
             method = "lda",
             metric = "ROC",
             trControl = ctrl)
lda

## Training Set
confusionMatrix(data = predict(lda, dataTrain), reference = as.factor(dataTrain$Win))

## Testing Set
confusionMatrix(data = predict(lda, dataTest), reference = as.factor(dataTest$Win))

### Get the ROC and AUC for the model and plot
dataTest$lda <- predict(lda, dataTest, type = "prob")[,1]
ldaROC <- roc(dataTest$Win, dataTest$lda)
auc(ldaROC)
plot(ldaROC, main = "LDA Model")
legend("bottomright", c("AUC = 0.7344 ","ROC = 0.7872"))

### This is our best model so we all the games will be predicted based on the lDA model
### Read in the first round matchups
firstRound <- read.csv("FirstRound.csv", header = T)
attach(firstRound)

### Create variables that show the difference in useful variables
firstRound$FTr_Mar <- (FTr-FTr_2)
firstRound$Pace_Mar <- (Pace-Pace_2)
firstRound$TOV_Mar <- (TOV.-TOV._2)
firstRound$eFG_Mar <- (eFG.-eFG._2)
firstRound$ORB_Mar <- (ORB.-ORB._2)
firstRound$SOS_Mar <- (SOS-SOS_2)
firstRound$AST_Mar <- (AST.-AST._2)
firstRound$STL_Mar <- (STL.-STL._2)

### Make the predictions for the first round games
ldaPred <- predict(lda, firstRound, type = 'prob')
### Print the probabilites of the team to win that particular game
ldaPred

### Create a dataset that simply has the two teams that played in this round and the probability of the first team winning
firstRoundTeams <- firstRound[,5:6]
firstRoundTeams
firstRoundResults <- cbind(firstRoundTeams, ldaPred)

### These are the results for the first round, the probabilites
### in column 3 and 4 are the probabilites for the first team
firstRoundResults

### Read the second round data
secondRound <- read.csv("SecondRound.csv", header = T)

### Create variables that show the difference in useful variables
attach(secondRound)
secondRound$FTr_Mar <- (FTr-FTr_2)
secondRound$Pace_Mar <- (Pace-Pace_2)
secondRound$TOV_Mar <- (TOV.-TOV._2)
secondRound$eFG_Mar <- (eFG.-eFG._2)
secondRound$ORB_Mar <- (ORB.-ORB._2)
secondRound$SOS_Mar <- (SOS-SOS_2)
secondRound$AST_Mar <- (AST.-AST._2)
secondRound$STL_Mar <- (STL.-STL._2)

### Make the predictions for the second round games
ldaPredSecond <- predict(lda, secondRound, type = 'prob')
### Print the probabilites of the team to win that particular game
ldaPredSecond

### Create a dataset that simply has the two teams that played in this round and the probability of the first team winning
secondRoundTeams <- secondRound[,5:6]
secondRoundResults <- cbind(secondRoundTeams, ldaPredSecond)

#### These are the results for the second round, the probabilites
### in column 3 and 4 are the probabilites for the first team
secondRoundResults

### Read the third round data
thirdRound <- read.csv("Sweet16.csv", header = T)

### Create variables that show the difference in useful variables
attach(thirdRound)
thirdRound$FTr_Mar <- (FTr-FTr_2)
thirdRound$Pace_Mar <- (Pace-Pace_2)
thirdRound$TOV_Mar <- (TOV.-TOV._2)
thirdRound$eFG_Mar <- (eFG.-eFG._2)
thirdRound$ORB_Mar <- (ORB.-ORB._2)
thirdRound$SOS_Mar <- (SOS-SOS_2)
thirdRound$AST_Mar <- (AST.-AST._2)
thirdRound$STL_Mar <- (STL.-STL._2)

### Make the predictions for the third round games
ldaPredThirdRound <- predict(lda, thirdRound, type = 'prob')
ldaPredThirdRound

### Create a dataset that simply has the two teams that played in this round and the probability of the first team winning
thirdRoundTeams <- thirdRound[,5:6]
thirdRoundResults <- cbind(thirdRoundTeams, ldaPredThirdRound)

#### These are the results for the third round, the probabilites
### in column 3 and 4 are the probabilites for the first team to win
thirdRoundResults

### Read in the fourth round games
fourthRound <- read.csv("Elite8.csv", header = T)


### Create variables that show the difference in useful variables
attach(fourthRound)
fourthRound$FTr_Mar <- (FTr-FTr_2)
fourthRound$Pace_Mar <- (Pace-Pace_2)
fourthRound$TOV_Mar <- (TOV.-TOV._2)
fourthRound$eFG_Mar <- (eFG.-eFG._2)
fourthRound$ORB_Mar <- (ORB.-ORB._2)
fourthRound$SOS_Mar <- (SOS-SOS_2)
fourthRound$AST_Mar <- (AST.-AST._2)
fourthRound$STL_Mar <- (STL.-STL._2)

### Make the predictions for the fourth round games
ldaPredFourthRound <- predict(lda, fourthRound, type = 'prob')
ldaPredFourthRound

### Create a dataset that simply has the two teams that played in this round and the probability of the first team winning
fourthRoundTeams <- fourthRound[,5:6]
fourthRoundResults <- cbind(fourthRoundTeams, ldaPredFourthRound)

#### These are the results for the fourth round, the probabilites
### in column 3 and 4 are the probabilites for the first team
fourthRoundResults

### Read in the fifth round games
fifthRound <- read.csv("FinalFour.csv", header = T)

### Create variables that show the difference in useful variables
attach(fifthRound)
fifthRound$FTr_Mar <- (FTr-FTr_2)
fifthRound$Pace_Mar <- (Pace-Pace_2)
fifthRound$TOV_Mar <- (TOV.-TOV._2)
fifthRound$eFG_Mar <- (eFG.-eFG._2)
fifthRound$ORB_Mar <- (ORB.-ORB._2)
fifthRound$SOS_Mar <- (SOS-SOS_2)
fifthRound$AST_Mar <- (AST.-AST._2)
fifthRound$STL_Mar <- (STL.-STL._2)

### Make the predictions for the fifth round games
ldaPredFifthRound <- predict(lda, fifthRound, type = 'prob')
ldaPredFifthRound

### Create a dataset that simply has the two teams that played in this round and the probability of the first team winning
fifthRoundTeams <- fifthRound[,5:6]
fifthRoundResults <- cbind(fifthRoundTeams, ldaPredFifthRound)

#### These are the results for the fifth round, the probabilites
### in column 3 and 4 are the probabilites for the first team
fifthRoundResults

### Read in the championship data
Championship <- read.csv("Championship.csv", header = T)

### Create variables that show the difference in useful variables
attach(Championship)
Championship$FTr_Mar <- (FTr-FTr_2)
Championship$Pace_Mar <- (Pace-Pace_2)
Championship$TOV_Mar <- (TOV.-TOV._2)
Championship$eFG_Mar <- (eFG.-eFG._2)
Championship$ORB_Mar <- (ORB.-ORB._2)
Championship$SOS_Mar <- (SOS-SOS_2)
Championship$AST_Mar <- (AST.-AST._2)
Championship$STL_Mar <- (STL.-STL._2)

### Make the predictions for the fifth round games
ldaPredChampionship <- predict(lda, Championship, type = 'prob')
ldaPredChampionship

### Create a dataset that simply has the two teams that played in this round and the probability of the first team winning
championshipTeams <- Championship[,5:6]
championshipResults <- cbind(championshipTeams, ldaPredChampionship)

#### These are the results for the championship round, the probabilites
### in column 3 and 4 are the probabilites for the first team
championshipResults

"The winner of the 2020 Championship is predicted to be Kansas"

### QDA Model
### ROC = 0.7855829
### Training Accuracy = 0.7064  
### Testing Accurarcy = 0.6678 

set.seed(69)
qda <- train(Win~ SOS_Mar+FTr_Mar+Pace_Mar+eFG_Mar+TOV_Mar+ORB_Mar,
             data = dataTrain,
             method = "qda",
             metric = "ROC",
             trControl = ctrl)
qda

## Training Set
confusionMatrix(data = predict(qda, dataTrain), reference = as.factor(dataTrain$Win))

## Testing Set
confusionMatrix(data = predict(qda, dataTest), reference = as.factor(dataTest$Win))

### Get the ROC and AUC for the model and plot
dataTest$qda <- predict(qda, dataTest, type = "prob")[,1]
qdaROC <- roc(dataTest$Win, dataTest$qda)
auc(qdaROC)
plot(qdaROC, main = "QDA Model")
legend("bottomright", c("AUC = 0.7335 ","ROC = 0.7855"))

### KNN Model
### Training ROC = 0.7181127
### Training Accuracy = 0.6979 
### Testing Accurarcy = 0.6007 

set.seed(69)
knn <- train(Win~ SOS_Mar+FTr_Mar+Pace_Mar+eFG_Mar+TOV_Mar+ORB_Mar,
             data = dataTrain,
             method = "knn",
             metric = "ROC",
             tuneGrid = data.frame(k = 1:20),
             trControl = ctrl)
knn

## Training Set
confusionMatrix(data = predict(knn, dataTrain), reference = dataTrain$Win)

## Testing Set
confusionMatrix(data = predict(knn, dataTest), reference = dataTest$Win)

### Get the ROC and AUC for the model and plot
dataTest$knn <- predict(knn, dataTest, type = "prob")[,1]
knnROC <- roc(dataTest$Win, dataTest$knn)
auc(knnROC)
plot(knnROC, main = "KNN Model")
legend("bottomright", c("AUC = 0.634 ","ROC = 0.7181"))


### MDA Model
### Training ROC = 0.7865
### Training Accuracy = 0.7054
### Testing Accurarcy = 0.6615

set.seed(69)
mda <- train(Win~ SOS_Mar+FTr_Mar+Pace_Mar+eFG_Mar+TOV_Mar+ORB_Mar,
             data = dataTrain,
             method = "mda",
             metric = "ROC",
             trControl = ctrl)
mda

## Training Set
confusionMatrix(data = predict(mda, dataTrain), reference = dataTrain$Win)

## Testing Set
confusionMatrix(data = predict(mda, dataTest), reference = dataTest$Win)

### Get the ROC and AUC for the model and plot
dataTest$mda <- predict(mda, dataTest, type = "prob")[,1]
mdaROC <- roc(dataTest$Win, dataTest$mda)
auc(mdaROC)
plot(mdaROC, main = "MDA Model")
legend("bottomright", c("AUC = 0.7332 ","ROC = 0.7865"))

### FDA Model
### Training ROC = 0.7882667
### Training Accuracy = 0.7128  
### Testing Accurarcy = 0.6746 

set.seed(69)
fda <- train(Win~ SOS_Mar+FTr_Mar+Pace_Mar+eFG_Mar+TOV_Mar+ORB_Mar,
             data = dataTrain,
             method = "fda",
             metric = "ROC",
             trControl = ctrl)
fda

## Training Set
confusionMatrix(data = predict(fda, dataTrain), reference = dataTrain$Win)

## Testing Set
confusionMatrix(data = predict(fda, dataTest), reference = dataTest$Win)

### Get the ROC and AUC for the model and plot
dataTest$fda <- predict(fda, dataTest, type = "prob")[,1]
fdaROC <- roc(dataTest$Win, dataTest$fda)
auc(fdaROC)
plot(fdaROC, main = "FDA Model")
legend("bottomright", c("AUC = 0.7340 ","ROC = 0.7882"))


### Neural Net Model
### Training ROC = 0.78638
### Training Accuracy = 0.7139
### Testing Accurarcy = 0.6718 

##Neural Net takes apprx. 45 minutes to run

nnetGrid <- expand.grid(.size = 1:10,
                        .decay = c(0, .01, .1, 0.5))
maxSize <- max(nnetGrid$.size)

nnetFit <- train(Win~ SOS_Mar+FTr_Mar+Pace_Mar+eFG_Mar+TOV_Mar+ORB_Mar,
                 data = dataTrain,
                 method = "nnet",
                 metric = "ROC",
                 preProc = c("center", "scale", "spatialSign"),
                 tuneGrid = nnetGrid,
                 trace = FALSE,
                 maxit = 1000,
                 MaxNWts = 200,
                 trControl = ctrl)

nnetFit

## Training Set
confusionMatrix(data = predict(nnetFit, dataTrain), reference = dataTrain$Win)

## Testing Set
confusionMatrix(data = predict(nnetFit, dataTest), reference = dataTest$Win)


### Get the ROC and AUC for the model and plot
dataTest$nnetFit <- predict(nnetFit, dataTest, type = "prob")[,1]
nnetFitROC <- roc(dataTest$Win, dataTest$nnetFit)
auc(nnetFitROC)
plot(nnetFitROC, main = " Neural Net Model")
legend("bottomright", c("AUC = 0.7332 ","ROC = 0.7863"))



### Naive Bayes Model
### Training ROC = 
### Training Accuracy = 0.7061 
### Testing Accurarcy = 0.6746 

nBayesFit = train(Win~ SOS_Mar+FTr_Mar+Pace_Mar+eFG_Mar+TOV_Mar+ORB_Mar,
                   data = dataTrain,
                   useKernel = TRUE,
                   method = "nb")

nBayesFit

## Training Set
confusionMatrix(data = predict(nBayesFit, dataTrain), reference = dataTrain$Win)

## Testing Set
confusionMatrix(data = predict(nBayesFit, dataTest), reference = dataTest$Win)

### Get the ROC and AUC for the model and plot
dataTest$nBayesFit <- predict(nBayesFit, dataTest, type = "prob")[,1]
nBayesFitROC <- roc(dataTest$Win, dataTest$nBayesFit)
auc(nBayesFitROC)
plot(nBayesFitROC, main = "Naive Bayes Model")
legend("bottomright", c("AUC = 0.7285 "))

### Random Forest Model
### Training ROC = 0.7497
### Training Accuracy = 0.9325
### Testing Accurarcy =  0.6138  

mtryGrid <- data.frame(mtry = floor(seq(1,
                                        ncol(dataTrain),
                                        length = 10)))

set.seed(69)
rf <- train(Win ~ SOS_Mar+FTr_Mar+Pace_Mar+eFG_Mar+TOV_Mar+ORB_Mar+AST_Mar+STL_Mar,
            data=dataTrain,
            method = "rf",
            tuneGrid = mtryGrid,
            ntree = 200,
            importance = TRUE,
            trControl = ctrl)
rf

## Training Set
confusionMatrix(data = predict(rf, dataTrain), reference = dataTrain$Win)
## Testing Set
confusionMatrix(data = predict(rf, dataTest), reference = dataTest$Win)

### Get the ROC and AUC for the model and plot
dataTest$rf <- predict(rf, dataTest, type = "prob")[,1]
rfROC <- roc(dataTest$Win, dataTest$rf)
auc(rfROC)
plot(rfROC, main = "Random Forest Model")
legend("bottomright", c("AUC = 0.6739 ","ROC = 0.7497"))


### glmnet Model
### Training ROC = 0.7876321
### Training Accuracy = 0.7064 
### Testing Accurarcy = 0.6684   

glmnGrid <- expand.grid(.alpha = c(0, .1, .2, .4, .6, .8, 1),
                        .lambda = seq(.01, .2, length = 40))
set.seed(69)
glmnTuned <- train(Win~ SOS_Mar+FTr_Mar+Pace_Mar+eFG_Mar+TOV_Mar+ORB_Mar,
                   data = dataTrain,
                   method = "glmnet",
                   tuneGrid = glmnGrid,
                   metric = "ROC",
                   trControl = ctrl)
glmnTuned

## Training Set
confusionMatrix(data = predict(glmnTuned, dataTrain), reference = dataTrain$Win)

## Testing Set
confusionMatrix(data = predict(glmnTuned, dataTest), reference = dataTest$Win)


### Get the ROC and AUC for the model and plot
dataTest$glmnTuned <- predict(glmnTuned, dataTest, type = "prob")[,1]
glmnTunedROC <- roc(dataTest$Win, dataTest$glmnTuned)
auc(glmnTunedROC)
plot(glmnTunedROC, main = "GLM Net Model")
legend("bottomright", c("AUC = 0.7346 ","ROC = 0.7876"))

### Create a dot plot showing the results from all the models
res = resamples(list(MDA = mda,KNN = knn, LDA = lda, QDA = qda, GLM = glm, FDA = fda, GLMNET = glmnTuned, RandomForest = rf, NeuralNet = nnetFit ))
dotplot(res, metric="ROC", main = "Model Comparision")



###ROC curves
plot(glmROC, col=1, lty=1, main = 'Comparison of Models')
lines(ldaROC, col=2, lty=2)
lines(qdaROC, col=3, lty=3)
lines(knnROC, col=10, lty=4)
lines(mdaROC, col=5, lty=5)
lines(nBayesROC, col=6, lty=6)
lines(nnetFitROC, col=7, lty=7)
lines(rfROC, col=8, lty=8)
lines(fdaROC, col = 'orange', lty=9)
lines(glmnTunedROC, col = 4, lty=10)
legend('bottomright', c('GLM','LDA','QDA','KNN','MDA','NBayes','NN','RF','FDA','GlmNet'), col=1:10, lty=1:10,lwd=2)

