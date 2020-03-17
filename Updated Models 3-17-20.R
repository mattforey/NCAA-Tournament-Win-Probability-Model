## Load the libraries need
library(caret)
library(e1071)
library(ggplot2)
library(corrplot)
library(partykit)

set.seed(100)

data <- read.csv("C:/Users/matt4/Downloads/FINAL NCAA Results.csv", header= TRUE)
data <- data[1:10518,]

data2 <- read.csv('C:/Users/matt4/Downloads/First Round.csv', header = T)

data$Win <- gsub("0", "Loss", data$Win)
data$Win <- gsub("1", "Win", data$Win)

train <- which(data$month != c(2,3))
dataTrain <- data[train,]
dataTest  <- data[-train,]

data$Win <- as.factor(data$Win)


attach(data)

## Check the structure of the dataset
str(data)


data$WinMargin <- data$teamscore - data$oppscore

hist(data$WinMargin, main = "Distribution of Win Margin", col = 5, xlab = "Win Margin")
hist(data$SOS, main = "Distribution of Strength of Schedule", col = 5, xlab = "Strength of Schedule")
hist(data$FTr, main = "Distribution of Free Throw Rate", col = 5, xlab = "Free Throw Rate")
hist(data$Pace, main = "Distribution of Pace", col = 5, xlab = "Pace")
hist(data$eFG., main = "Distribution of Effective Field Goal %", col = 5, xlab = "EFG%")


corr_data <- data[, c(13:34)]
corr_matrix <- cor(corr_data)
corrplot(corr_matrix, type = "upper")
library("PerformanceAnalytics")
chart.Correlation(corr_data, histogram = TRUE, pch=19)


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

## Already Changed the names of teams
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


### already changed the names of opponent
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


### Logistic Regression

### Model 1
### Training: 0.7059361
### Testing:  0.6670455

lrModel <- glm(Win~ SOS+SOS_2+FTr+FTr_2+Pace+Pace_2+eFG.+eFG._2+TOV.+TOV._2+ORB.+ORB._2,
               data = dataTrain,
               family = binomial)


### Model 2
### Training: 0.6783105
### Testing:  0.6488636

data$FTr_Mar <- (FTr-FTr_2)
data$Pace_Mar <- (Pace-Pace_2)
data$TOV_Mar <- (TOV.-TOV._2)
data$eFG_Mar <- (eFG.-eFG._2)
data$ORB_Mar <- (ORB.-ORB._2)
data$SOS_Mar <- (SOS-SOS_2)
data$AST_Mar <- (AST.-AST._2)
data$STL_Mar <- (STL.-STL._2)


attach(data2)
data2$FTr_Mar <- (FTr-FTr_2)
data2$Pace_Mar <- (Pace-Pace_2)
data2$TOV_Mar <- (TOV.-TOV._2)
data2$eFG_Mar <- (eFG.-eFG._2)
data2$ORB_Mar <- (ORB.-ORB._2)
data2$SOS_Mar <- (SOS-SOS_2)
data2$AST_Mar <- (AST.-AST._2)
data2$STL_Mar <- (STL.-STL._2)
attach(data)
  
lrModel <- glm(Win~ (FTr/FTr_2)+(Pace/Pace_2)+(TOV./TOV._2)+(eFG./eFG._2)+(ORB./ORB._2)+(SOS/SOS_2),
               data = dataTrain,
               family = binomial)


### Model 3
### Training: 0.676484
### Testing:  0.6528409

lrModel <- glm(Win~ x1  + x2 + x3 + x4 + x5+ (SOS/SOS_2),
              data = dataTrain,
              family = binomial)


lrModel
modelFit = summary(lrModel)
modelFit

lrProb = predict(lrModel, dataTrain,type="response"); 
lrProb[2150:2160]; 
contrasts (dataTrain$Win);

# Assign "Win" if posterior prob>0.5
lrPred=rep("Lose",8760);
lrPred[lrProb >=.5]="Win"; 

# confusion matrix
lrTable = table(lrPred,dataTrain$Win)
lrTable

lrCCR <- (lrTable[1,1] +lrTable[2,2]) / 8760
lrCCR

varImp(lrModel)

### testing ###


lrProb = predict(lrModel, dataTest,type="response"); 

# Assign "Win" if posterior prob>0.5
lrPred=rep("Lose",1760);
lrPred[lrProb >=.5]="Win"; 

# confusion matrix
lrTable = table(lrPred,dataTest$Win)
lrTable

lrCCR <- (lrTable[1,1] +lrTable[2,2]) / 1760
lrCCR



### train



ctrl <- trainControl(method = "LGOCV",
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE,
                     savePredictions = TRUE)

### glm

glm <- train(Win~ SOS_Mar+FTr_Mar+Pace_Mar+eFG_Mar+TOV_Mar+ORB_Mar,
             data = data,
             method = "glm",
             metric = "ROC",
             trControl = ctrl)
summary(glm)
glm

confusionMatrix(data = predict(glm, data), reference = data$Win)

glmPred <- predict(glm, data2, type = 'prob')
glmPred

### lda

lda <- train(Win~ SOS+SOS_2+FTr+FTr_2+Pace+Pace_2+eFG.+eFG._2+TOV.+TOV._2+ORB.+ORB._2,
             data = data,
             method = "lda",
             metric = "ROC",
             trControl = ctrl)


confusionMatrix(data = predict(lda, data), reference = as.factor(data$Win))

### qda

qda <- train(Win~ SOS+SOS_2+FTr+FTr_2+Pace+Pace_2+eFG.+eFG._2+TOV.+TOV._2+ORB.+ORB._2,
             data = dataTrain,
             method = "qda",
             metric = "ROC",
             trControl = ctrl)

confusionMatrix(data = predict(qda, dataTest), reference = as.factor(dataTest$Win))

confusionMatrix(data = predict(qda, dataTrain), reference = as.factor(dataTrain$Win))


qda

### knn

knn <- train(Win~ SOS+SOS_2+FTr+FTr_2+Pace+Pace_2+eFG.+eFG._2+TOV.+TOV._2+ORB.+ORB._2,
             data = dataTrain,
             method = "knn",
             metric = "ROC",
             tuneGrid = data.frame(k = 1:20),
             trControl = ctrl)
knn

confusionMatrix(data = predict(knn, dataTrain), reference = dataTrain$Win)

confusionMatrix(data = predict(knn, dataTest), reference = dataTest$Win)

### mda

mda <- train(Win~ SOS+SOS_2+FTr+FTr_2+Pace+Pace_2+eFG.+eFG._2+TOV.+TOV._2+ORB.+ORB._2,
             data = data,
             method = "mda",
             metric = "ROC",
             trControl = ctrl)
mda

confusionMatrix(data = predict(mda, data), reference = as.factor(data$Win))


### nNet

nnetGrid <- expand.grid(.size = 1:10,
                        .decay = c(0, .01, .1, 0.5))
maxSize <- max(nnetGrid$.size)

nnetFit <- train(Win~ SOS+SOS_2+FTr+FTr_2+Pace+Pace_2+eFG.+eFG._2+TOV.+TOV._2+ORB.+ORB._2,
                data = data,
                method = "nnet",
                metric = "ROC",
                preProc = c("center", "scale", "spatialSign"),
                tuneGrid = nnetGrid,
                trace = FALSE,
                maxit = 200,
                MaxNWts = 200,
                trControl = ctrl)



### nb

nBayesFit1 = train(Win~ SOS+SOS_2+FTr+FTr_2+Pace+Pace_2+eFG.+eFG._2+TOV.+TOV._2+ORB.+ORB._2,
                   data = data,
                   method = "nb")

nBayesFit1

confusionMatrix(data = predict(nBayesFit1, data), reference = as.factor(data$Win))


### rf


ctrlOOB <- trainControl(method = "oob")
mtryGrid <- data.frame(mtry = floor(seq(1, ncol(data), length = 10)))
rf <- train(Win ~ SOS_Mar+FTr_Mar+Pace_Mar+eFG_Mar+TOV_Mar+ORB_Mar+AST_Mar+STL_Mar,
            data=data,
            method = "rf",
            tuneGrid = mtryGrid,
            ntree = 200,
            importance = TRUE,
            trControl = ctrlOOB)

plot(rf)
rf

rTree <- as.party(rf$finalModel)


cart <- train(Win ~ SOS_Mar+FTr_Mar+Pace_Mar+eFG_Mar+TOV_Mar+ORB_Mar+AST_Mar+STL_Mar,
              data=data,
              method = 'rpart',
              tunelength = 50,
              trControl = ctrl)

cart


### 

indx <- createFolds(data$WinMargin, returnTrain = TRUE)
ctrl <- trainControl(method = 'cv', index = indx)

lm <- train(WinMargin~ SOS_Mar+FTr_Mar+Pace_Mar+eFG_Mar+TOV_Mar+ORB_Mar+AST.+AST._2+STL.+STL._2,
            data = data,
            method = 'lm',
            trControl = ctrl,
            preProcess = c('center','scale','BoxCox','spatialSign'))

lmPred <- predict(lm, type = 'prob')


lm
summary(lm)

res <- resamples(list(glm=glm,mda=mda,knn=knn))

dotplot(res)

hist(data$WinMargin)



lm2 <- lm(y = Win, 
          x = SOS+SOS_2+FTr+FTr_2+Pace+Pace_2+eFG.+eFG._2+TOV.+TOV._2+ORB.+ORB._2+AST.+AST._2+STL.+STL._2,
          data = data)
lm2



### svm






