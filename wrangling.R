################################################################################
############################       libraries       #############################
################################################################################
library(tidyverse)
library(lubridate)

################################################################################
############################      Email Data       #############################
################################################################################
Emails <- rbind(email2017, email2018, email2019)
Emails <- Emails %>%
  mutate(EmailSent = ymd_hms(EmailSent),
         Year = year(EmailSent),
         Quarter = quarter(EmailSent),
         EmailOpen = ifelse(EmailOpen == "NULL", 0, 1),
         EmailClick = ifelse(EmailClick == "NULL", 0, 1)) %>%
  filter(GroupID >= min(CustomerDetail$GroupID))

Dates <- ymd(c("2017-01-01", 
               "2018-01-01",
               "2019-01-01")) 
FinalEmails <- data.frame()
t <- 1

for(i in 1:3) {
  CurrentYear = year(Dates[i])
  
  temp <- subset(Emails, Year == CurrentYear)
  
  Quarter1 <- temp %>%
    group_by(GroupID) %>%
    filter(Quarter == 1) %>%
    summarize("t" = t,
              "Year" = CurrentYear,
              "Quarter" = 1,
              "ReceivedEmails" = n(),
              "OpenRate" = sum(EmailOpen) / ReceivedEmails,
              "ClickRate" = sum(EmailClick) / ReceivedEmails)
  t <- t + 1
  
  Quarter2 <- temp %>%
    group_by(GroupID) %>%
    filter(Quarter == 2) %>%
    summarize("t" = t,
              "Year" = CurrentYear,
              "Quarter" = 2,
              "ReceivedEmails" = n(),
              "OpenRate" = sum(EmailOpen) / ReceivedEmails,
              "ClickRate" = sum(EmailClick) / ReceivedEmails)
  t <- t + 1
  
  Quarter3 <- temp %>%
    group_by(GroupID) %>%
    filter(Quarter == 3) %>%
    summarize("t" = t,
              "Year" = CurrentYear,
              "Quarter" = 3,
              "ReceivedEmails" = n(),
              "OpenRate" = sum(EmailOpen) / ReceivedEmails,
              "ClickRate" = sum(EmailClick) / ReceivedEmails)
  t <- t + 1
  
  Quarter4 <- temp %>%
    group_by(GroupID) %>%
    filter(Quarter == 4) %>%
    summarize("t" = t,
              "Year" = CurrentYear,
              "Quarter" = 4,
              "ReceivedEmails" = n(),
              "OpenRate" = sum(EmailOpen) / ReceivedEmails,
              "ClickRate" = sum(EmailClick) / ReceivedEmails)
  t <- t + 1
  
  FinalEmails <- rbind(FinalEmails, Quarter1, Quarter2, Quarter3, Quarter4)
}

################################################################################
##########################       Tickets Data      ############################
################################################################################
Ticketing <- Ticketing %>%
  mutate(TransactionPurchaseDate = ymd_hms(TransactionPurchaseDate),
         OcasionStartDate = ymd_hms(OccasionStartDate),
         Quarter = quarter(TransactionPurchaseDate),
         Year = year(TransactionPurchaseDate)) %>%
  filter(!is.na(TransactionPurchaseDate))

Dates <- ymd(c("2017-01-01", 
               "2018-01-01",
               "2019-01-01"))
FinalTicketing <- data.frame()
t <- 1
for(i in 1:3) {
  CurrentYear = year(Dates[i])
  
  temp <- subset(Ticketing, Year == CurrentYear)
  
  Quarter1 <- temp %>%
    group_by(GroupID) %>%
    filter(Quarter == 1) %>%
    summarize("t" = t,
              "Year" = CurrentYear,
              "Quarter" = 1,
              "TicketsBought" = n())
  t <- t + 1
  
  Quarter2 <- temp %>%
    group_by(GroupID) %>%
    filter(Quarter == 2) %>%
    summarize("t" = t,
              "Year" = CurrentYear,
              "Quarter" = 2,
              "TicketsBought" = n())
  t <- t + 1
  
  Quarter3 <- temp %>%
    group_by(GroupID) %>%
    filter(Quarter == 3) %>%
    summarize("t" = t,
              "Year" = CurrentYear,
              "Quarter" = 3,
              "TicketsBought" = n())
  t <- t + 1
  
  Quarter4 <- temp %>%
    group_by(GroupID) %>%
    filter(Quarter == 4) %>%
    summarize("t" = t,
              "Year" = CurrentYear,
              "Quarter" = 4,
              "TicketsBought" = n())
  t <- t + 1
  
  FinalTicketing <- rbind(FinalTicketing, Quarter1, Quarter2, Quarter3, Quarter4)
}

################################################################################
#########################      Merchandise Data      ###########################
################################################################################
Merchandise <- Merchandise %>%
  mutate(GroupID = ifelse(GroupID == "NULL", 0, GroupID)) %>%
  filter(GroupID != 0)

Merchandise$ProductCategory <- factor(Merchandise$ProductCategory)
levels(Merchandise$ProductCategory)

#Grouping similar categories together
#I am not able to share the actual product names in each category due to privacy constraints 
CasualClothing <- c('')
MatchClothing <- c('')
TrainingsWear <- c('')
Others <- c('')

#Adding Dummy Variables for Product Category
Merchandise <- Merchandise %>%
  mutate(CasualClothing = ifelse(ProductCategory %in% CasualClothing, 1, 0),
         MatchClothing = ifelse(ProductCategory %in% MatchClothing, 1, 0),
         TrainingsWear = ifelse(ProductCategory %in% TrainingsWear, 1, 0), 
         Others = ifelse(ProductCategory %in% Others, 1, 0))

Merchandise <- Merchandise %>%
  mutate(TransactionPurchaseDate = ymd_hms(TransactionPurchaseDate), 
         Year = year(TransactionPurchaseDate),
         Quarter = quarter(TransactionPurchaseDate)) 

#Total purchases per category
sum(Merchandise$CasualClothing)
sum(Merchandise$MatchClothing)
sum(Merchandise$TrainingsWear)
sum(Merchandise$Others)

#Constructing Final Merchandise data frame
Dates <- ymd(c("2017-01-01", 
               "2018-01-01",
               "2019-01-01",
               "2020-01-01"))
FinalMerchandise <- data.frame()
t <- 1

for(i in 1:3) {
  CurrentYear = year(Dates[i])
  
  temp <- subset(Merchandise, Year == CurrentYear)
  
  Quarter1 <- temp %>%
    group_by(GroupID) %>%
    filter(Quarter == 1) %>%
    summarize("t" = t,
              "Year" = CurrentYear,
              "Quarter" = 1,
              "MerchandisePurchase" = 1,
              "NumberOfMerchandiseBought" = n(),
              "CasualClothing" = sum(CasualClothing) / NumberOfMerchandiseBought,
              "MatchClothing" = sum(MatchClothing) / NumberOfMerchandiseBought,
              "TrainingsWear" = sum(TrainingsWear) / NumberOfMerchandiseBought,
              "Others" = sum(Others) / NumberOfMerchandiseBought)
  t <- t + 1
  
  Quarter2 <- temp %>%
    group_by(GroupID) %>%
    filter(Quarter == 2) %>%
    summarize("t" = t,
              "Year" = CurrentYear,
              "Quarter" = 2,
              "MerchandisePurchase" = 1,
              "NumberOfMerchandiseBought" = n(),
              "CasualClothing" = sum(CasualClothing) / NumberOfMerchandiseBought,
              "MatchClothing" = sum(MatchClothing) / NumberOfMerchandiseBought,
              "TrainingsWear" = sum(TrainingsWear) / NumberOfMerchandiseBought,
              "Others" = sum(Others) / NumberOfMerchandiseBought)
  t <- t + 1
  
  Quarter3 <- temp %>%
    group_by(GroupID) %>%
    filter(Quarter == 3) %>%
    summarize("t" = t,
              "Year" = CurrentYear,
              "Quarter" = 3,
              "MerchandisePurchase" = 1,
              "NumberOfMerchandiseBought" = n(),
              "CasualClothing" = sum(CasualClothing) / NumberOfMerchandiseBought,
              "MatchClothing" = sum(MatchClothing) / NumberOfMerchandiseBought,
              "TrainingsWear" = sum(TrainingsWear) / NumberOfMerchandiseBought,
              "Others" = sum(Others) / NumberOfMerchandiseBought)
  t <- t + 1
  
  Quarter4 <- temp %>%
    group_by(GroupID) %>%
    filter(Quarter == 4) %>%
    summarize("t" = t,
              "Year" = CurrentYear,
              "Quarter" = 4,
              "MerchandisePurchase" = 1,
              "NumberOfMerchandiseBought" = n(),
              "CasualClothing" = sum(CasualClothing) / NumberOfMerchandiseBought,
              "MatchClothing" = sum(MatchClothing) / NumberOfMerchandiseBought,
              "TrainingsWear" = sum(TrainingsWear) / NumberOfMerchandiseBought,
              "Others" = sum(Others) / NumberOfMerchandiseBought)
  t <- t + 1
  
  FinalMerchandise <- rbind(FinalMerchandise, Quarter1, Quarter2, Quarter3, Quarter4)
}

################################################################################
##########################      Customer Data       ############################
################################################################################
CustomerDetail <- CustomerDetail %>%
  mutate(DistanceFromClub = ifelse(DistanceFromClub == "NULL", NA, DistanceFromClub),
         DistanceFromClub = as.integer(DistanceFromClub),
         DateOfBirth = ymd_hms(DateOfBirth),
         Age = as.integer(Age),
         AgeOfChildren = ifelse(AgeOfChildren == "NULL", NA, AgeOfChildren),
         AgeOfChildren = as.integer(AgeOfChildren))
summary(CustomerDetail$AgeOfChildren) #All NA's, so dropped for final data

CustomerDetail <- CustomerDetail[, c(1:5)]

################################################################################
#########################      Match Results data     ##########################
################################################################################
MatchResults <- rbind(Results1617[, c(2:7)], Results1718[, c(2:7)], Results1819[, c(2:7)], Results1920[, c(2,4:8)])
MatchResults <- MatchResults %>%
  mutate(Date = dmy(Date),
         HomeTeam = factor(HomeTeam),
         AwayTeam = factor(AwayTeam),
         FTR = factor(FTR),
         Year = year(Date),
         Quarter = quarter(Date)) %>%
  filter(Year>=2016 & Year<=2019) %>%
  filter(HomeTeam == "XXX" | AwayTeam == "XXX")

HomeResults <- MatchResults %>%
  filter(HomeTeam == "XXX") %>%
  mutate(Points = ifelse(FTR == "H", 3, ifelse(FTR == "D", 1, 0)),
         GoalsScored = FTHG,
         GoalsConceded = FTAG)

AwayResults <- MatchResults %>%
  filter(AwayTeam == "XXX") %>%
  mutate(Points = ifelse(FTR == "H", 0, ifelse(FTR == "D", 1, 3)),
         GoalsScored = FTAG,
         GoalsConceded = FTHG)
MatchResults <- rbind(HomeResults, AwayResults)


Dates <- ymd(c("2016-01-01",
               "2017-01-01", 
               "2018-01-01",
               "2019-01-01"))
FinalMatchResults <- data.frame()

for(i in 1:4) {
  CurrentYear = year(Dates[i])
  
  temp <- MatchResults %>%
    filter(Year == CurrentYear ) %>%
    group_by(Quarter) %>%
    summarize("Year" = CurrentYear,
              "MatchesPlayed" = n(),
              "TotPoints" = sum(Points),
              "AvgPoints" = TotPoints / MatchesPlayed,
              "TotGoalsScored" = sum(GoalsScored),
              "TotGoalsConceded" = sum(GoalsConceded),
              "AvgGoalsScored" = TotGoalsScored / MatchesPlayed,
              "AvgGoalsConceded" = TotGoalsConceded / MatchesPlayed)
  FinalMatchResults <- rbind(FinalMatchResults, temp)
}
FinalMatchResults <- FinalMatchResults[c(2:13), c(3,5,8,9)]
t <- seq(from = 1, to = 12, by = 1)
FinalMatchResults <- cbind(t, FinalMatchResults)  

################################################################################
########################     Combining Data sets     ###########################
################################################################################
FinalMerchandise$GroupID <- as.integer(FinalMerchandise$GroupID)
FinalTicketing$GroupID <- as.integer(FinalTicketing$GroupID)

FinalData <- full_join(FinalEmails, FinalMerchandise, by = c("GroupID", "t", "Year", "Quarter"))
FinalData <- full_join(FinalData, FinalTicketing, by = c("GroupID", "t", "Year", "Quarter"))
FinalData <- left_join(FinalData, CustomerDetail, by = "GroupID")
FinalData <- left_join(FinalData, FinalMatchResults, by = 't')

################################################################################
##########################       Changing Age       ############################
################################################################################
summary(FinalData$DateOfBirth)
FinalData$DateOfBirth <- ymd(FinalData$DateOfBirth)
FinalData$StartDateQuarter <- ifelse(FinalData$Year == 2017 & FinalData$Quarter == 1, "2017-01-01",
                                     ifelse(FinalData$Year == 2017 & FinalData$Quarter == 2, "2017-04-01",
                                            ifelse(FinalData$Year == 2017 & FinalData$Quarter == 3, "2017-07-01",
                                                   ifelse(FinalData$Year == 2017 & FinalData$Quarter == 4, "2017-10-01",
                                                          ifelse(FinalData$Year == 2018 & FinalData$Quarter == 1, "2018-01-01",
                                                                 ifelse(FinalData$Year == 2018 & FinalData$Quarter == 2, "2018-04-01",
                                                                        ifelse(FinalData$Year == 2018 & FinalData$Quarter == 3, "2018-07-01",
                                                                               ifelse(FinalData$Year == 2018 & FinalData$Quarter == 4, "2018-10-01",
                                                                                      ifelse(FinalData$Year == 2019 & FinalData$Quarter == 1, "2019-01-01",
                                                                                             ifelse(FinalData$Year == 2019 & FinalData$Quarter == 2, "2019-04-01",
                                                                                                    ifelse(FinalData$Year == 2019 & FinalData$Quarter == 3, "2019-07-01",
                                                                                                           ifelse(FinalData$Year == 2019 & FinalData$Quarter == 4, "2019-10-01", "0")))))))))))
)
FinalData$StartDateQuarter <- ymd(FinalData$StartDateQuarter)
FinalData$Age <- floor(interval(FinalData$DateOfBirth, FinalData$StartDateQuarter) / dyears(1) )


################################################################################
#######################       Changing NA Values      ##########################
################################################################################
#Function to set NA values to zero
na.zero <- function (x) {
  x[is.na(x)] <- 0
  return(x)
}

#Setting NA values to zero
FinalData <- FinalData %>%
  mutate(MerchandisePurchase = na.zero(MerchandisePurchase),
         NumberOfMerchandiseBought = na.zero(NumberOfMerchandiseBought),
         CasualClothing = na.zero(CasualClothing),
         MatchClothing = na.zero(MatchClothing),
         TrainingsWear = na.zero(TrainingsWear),
         Others = na.zero(Others),
         TicketsBought = na.zero(TicketsBought))

#Chaning NA values of Age
summary(FinalData$Age)
FinalData$Age <- ifelse(is.na(FinalData$Age) | FinalData$Age <= 0, 34.7, FinalData$Age)
summary(FinalData$Age)

#Chaning NA values of Distance from club
summary(FinalData$DistanceFromClub)
FinalData$DistanceFromClub <- ifelse(is.na(FinalData$DistanceFromClub), 49.7, FinalData$DistanceFromClub)
summary(FinalData$DistanceFromClub)

#Changing NA values of Click through rate
#Getting all ID's with a NA value for CTR
NoCTR <- FinalData %>% 
  filter(is.na(FinalData$ClickRate))
vectorID <- as.vector(as.matrix(NoCTR[, 1]))

#Calculating the avg CTR for those individuals in periods they did have a CTR value
temp <- FinalData %>%
  filter(GroupID %in% vectorID & !is.na(ClickRate))  %>%
  group_by(GroupID) %>%
  summarise("Mean" = mean(ClickRate))

FinalData <- left_join(FinalData, temp, by = "GroupID")

#Changing the Click Rate NA values
summary(FinalData$ClickRate)
MeanCLickRate <- 0.05
FinalData$ClickRate <- ifelse(is.na(FinalData$ClickRate), 
                              ifelse(!is.na(FinalData$Mean), FinalData$Mean, MeanCLickRate),
                              FinalData$ClickRate)
summary(FinalData$ClickRate)
sum(is.na(FinalData$ClickRate)) #No more NA values

################################################################################
##########################       Final Check       #############################
################################################################################
FinalData <- FinalData[, c(1:4, 7, 8, 10:14, 16, 18, 20)]

#Still 11 NA values for ID, so deleting them
sum(is.na(FinalData$GroupID))
FinalData <- FinalData %>% 
  filter(!is.na(GroupID))

#No NA values in entire data set
sum(is.na(FinalData))
str(FinalData)
