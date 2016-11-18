# Objective: Using Linear Regression to Identify and prioritize the key drivers in Total Credit Card Spend 
# from Credit Card data for 5000 customers. 

#-----------------------------Importing the Data
credit.card <- read.csv("Linear Regression Case.csv", na.strings = "#NULL!")

#-----------------------------Preparing the data
str(credit.card)

# Removing the Customer ID
credit.card <- credit.card[,-1]
str(credit.card)
summary(credit.card)

# Converting Categorical Features to Factors
# labeling no response as NAs
# keeping N/A as it is

credit.card$region <- factor(credit.card$region, levels = c(1,2,3,4,5), labels = c("Zone 1","Zone 2","Zone 3","Zone 4","Zone 5"))
credit.card$townsize <- factor(credit.card$townsize, levels = c(1,2,3,4,5), labels = c("> 250,000", "50,000-249,999",
                                                                                       "10,000-49,999",
                                                                                       "2,500-9,999",
                                                                                       "< 2,500")) 
credit.card$gender <- factor(credit.card$gender, levels = c(0,1), labels = c("Male", "Female"))
credit.card$agecat <- factor(credit.card$agecat, levels = c(1,2,3,4,5,6), labels = c("<18", "18-24",
                                                                                     "25-34",
                                                                                     "35-49",
                                                                                     "50-64",
                                                                                     ">65"))
credit.card$edcat <- factor(credit.card$edcat, levels = c(1,2,3,4,5), labels = c("Did not complete high school","High school degree",
                                                                                 "Some college",
                                                                                 "College degree",
                                                                                 "Post-undergraduate degree"))
credit.card$jobcat <- factor(credit.card$jobcat, levels = c(1,2,3,4,5,6), labels = c("Managerial and Professional","Sales and Office",
                                                                                     "Service",
                                                                                     "Agricultural and Natural Resources",
                                                                                     "Precision Production, Craft, Repair",
                                                                                     "Operation, Fabrication, General Labor"))
credit.card$union <- factor(credit.card$union, levels = c(0,1), labels = c("No", "Yes"))
credit.card$employ <- factor(credit.card$employ, ordered = TRUE)
credit.card$empcat <- factor(credit.card$empcat, levels =c(1,2,3,4,5), labels = c("Less than 2",
                                                                                  "2 to 5",
                                                                                  "6 to 10",
                                                                                  "11 to 15",
                                                                                  "More than 15"))
credit.card$retire <- factor(credit.card$retire, levels = c(0,1), labels = c("No", "Yes"))
credit.card$inccat <- factor(credit.card$inccat, levels = c(1,2,3,4,5), labels = c("Under $25",
                                                                                   "$25 - $49",
                                                                                   "$50 - $74",
                                                                                   "$75 - $124",
                                                                                   "$125+"))
credit.card$default <- factor(credit.card$default, levels = c(0,1), labels = c("No", "Yes"))
credit.card$jobsat <- factor(credit.card$jobsat, levels = c(1,2,3,4,5), labels = c("Highly dissatisfied",
                                                                                   "Somewhat dissatisfied",
                                                                                   "Neutral",
                                                                                   "Somewhat satisfied",
                                                                                   "Highly satisfied"))
credit.card$marital <- factor(credit.card$marital, levels = c(0,1), labels=c("Unmarried",
                                                                             "Married"))
credit.card$spousedcat <- factor(credit.card$spousedcat, levels = c(-1,1,2,3,4,5), labels = c("Not married",
                                                                                              "Did not complete high school",
                                                                                              "High school degree",
                                                                                              "Some college",
                                                                                              "College degree",
                                                                                              "Post-undergraduate degree"))
credit.card$homeown <- factor(credit.card$homeown, levels = c(0,1), labels = c("Rent", "Own"))
credit.card$hometype <- factor(credit.card$hometype, levels = c(1,2,3,4), labels = c("Single-family",
                                                                                     "Multiple-Family",
                                                                                     "Condominium/Townhouse",
                                                                                     "Mobile Home"))
credit.card$address <- factor(credit.card$address)
credit.card$addresscat <- factor(credit.card$addresscat, levels = c(1,2,3,4,5), labels = c("Less than 3",
                                                                                           "4 to 7",
                                                                                           "8 to 15",
                                                                                           "16 to 25",
                                                                                           "More than 25"))
credit.card$cars <- factor(credit.card$cars, ordered = TRUE)
credit.card$carown <- factor(credit.card$carown, levels =c(-1,0,1), labels = c("N/A", "Lease","Own"))
credit.card$cartype <- factor(credit.card$cartype, levels = c(-1,0,1), labels = c("N/A","Domestic", "Import"))
credit.card$carcatvalue <- factor(credit.card$carcatvalue, levels = c(-1,1,2,3), labels = c("N/A",
                                                                                            "Economy",
                                                                                            "Standard",
                                                                                            "Luxury"))
credit.card$carbought <- factor(credit.card$carbought, levels = c(-1,0,1), labels = c("N/A",
                                                                                      "No",
                                                                                      "Yes"))
credit.card$carbuy <- factor(credit.card$carbuy, levels = c(0,1), labels= c("No","Yes"))
credit.card$commute <- factor(credit.card$commute, levels = c(1,2,3,4,5,6,7,8,9,10), labels = c("Car",
                                                                                                "Motorcycle",
                                                                                                "Carpool",
                                                                                                "Bus",
                                                                                                "Train/Subway",
                                                                                                "Other public transit",
                                                                                                "Bicycle",
                                                                                                "Walk",
                                                                                                "Other non-motorized transit",
                                                                                                "Telecommute"))
credit.card$commutecat <- factor(credit.card$commutecat, levels = c(1,2,3,4,5), labels = c("Single occupancy",
                                                                                           "Multiple occupancy",
                                                                                           "Public transportation",
                                                                                           "Non-motorized",
                                                                                           "Telecommute"))
credit.card$commutecar <- factor(credit.card$commutecar, levels = c(0,1), labels = c("No", "Yes"))
credit.card$commutemotorcycle <- factor(credit.card$commutemotorcycle, levels = c(0,1), labels = c("No", "Yes"))
credit.card$commutecarpool <- factor(credit.card$commutecarpool, levels = c(0,1), labels = c("No", "Yes")) 
credit.card$commutebus <- factor(credit.card$commutebus, levels = c(0,1), labels = c("No", "Yes"))
credit.card$commuterail <- factor(credit.card$commuterail, levels = c(0,1), labels = c("No", "Yes"))
credit.card$commutepublic <- factor(credit.card$commutepublic, levels = c(0,1), labels = c("No", "Yes"))
credit.card$commutebike <- factor(credit.card$commutebike, levels = c(0,1), labels = c("No", "Yes"))
credit.card$commutewalk <- factor(credit.card$commutewalk, levels = c(0,1), labels = c("No", "Yes"))
credit.card$commutenonmotor <- factor(credit.card$commutenonmotor, levels = c(0,1), labels = c("No", "Yes"))
credit.card$telecommute <- factor(credit.card$telecommute, levels = c(0,1), labels = c("No", "Yes"))

credit.card$reason <- factor(credit.card$reason, levels = c(1,2,3,4,8,9), labels = c("Prices",
                                                                                     "Convenience",
                                                                                     "Service",
                                                                                     "Other",
                                                                                     "N/A",
                                                                                     "Missing"))
credit.card$polview <- factor(credit.card$polview, levels = c(1,2,3,4,5,6,7), labels = c("Extremely liberal",
                                                                                         "Liberal",
                                                                                         "Slightly liberal",
                                                                                         "Moderate",
                                                                                         "Slightly conservative",
                                                                                         "Conservative",
                                                                                         "Extremely conservative"))
credit.card$polparty <- factor(credit.card$polparty, levels = c(0,1), labels = c("No", "Yes")) 
credit.card$polcontrib <- factor(credit.card$polcontrib, levels = c(0,1), labels = c("No", "Yes")) 
credit.card$vote <- factor(credit.card$vote, levels = c(0,1), labels = c("No", "Yes"))

credit.card$card <- factor(credit.card$card, levels = c(1,2,3,4,5), labels = c("American Express",
                                                                               "Visa",
                                                                               "Mastercard",
                                                                               "Discover",
                                                                               "Other"))
credit.card$cardtype <- factor(credit.card$cardtype, levels = c(1,2,3,4), labels = c("None",
                                                                                     "Gold",
                                                                                     "Platinum",
                                                                                     "Other")) 
credit.card$cardbenefit <- factor(creditcard$cardbenefit, levels = c(1,2,3,4), labels = c("None",
                                                                                          "Cash back",
                                                                                          "Airline miles",
                                                                                          "Other"))
credit.card$cardfee <- factor(credit.card$cardfee, levels = c(0,1), labels = c("No", "Yes"))
credit.card$cardtenure <- factor(credit.card$cardtenure, ordered = TRUE)
credit.card$cardtenurecat <- factor(credit.card$cardtenurecat, levels = c(1,2,3,4,5), labels = c("Less than 2",
                                                                                                 "2 to 5",
                                                                                                 "6 to 10",
                                                                                                 "11 to 15",
                                                                                                 "More than 15"))
credit.card$card2 <- factor(credit.card$card2, levels = c(1,2,3,4,5), labels = c("American Express",
                                                                                 "Visa",
                                                                                 "Mastercard",
                                                                                 "Discover",
                                                                                 "Other"))
credit.card$card2type <- factor(credit.card$card2type, levels = c(1,2,3,4), labels = c("None",
                                                                                       "Gold",
                                                                                       "Platinum",
                                                                                       "Other"))
credit.card$card2benefit <- factor(credit.card$card2benefit, levels = c(1,2,3,4), labels = c("None",
                                                                                             "Cash back",
                                                                                             "Airline miles",
                                                                                             "Other"))

credit.card$card2fee <- factor(credit.card$card2fee, levels = c(0,1), labels = c("No", "Yes"))
credit.card$card2tenure <- factor(credit.card$card2tenure, ordered = TRUE)
credit.card$card2tenurecat <-  factor(credit.card$card2tenurecat, levels = c(1,2,3,4,5), labels = c("Less than 2",
                                                                                                    "2 to 5",
                                                                                                    "6 to 10",
                                                                                                    "11 to 15",
                                                                                                    "More than 15"))
credit.card$active <- factor(credit.card$active, levels = c(0,1), labels = c("No", "Yes"))
credit.card$bfast <- factor(credit.card$bfast, levels = c(1,2,3), labels = c("Energy bar",
                                                                             "Oatmeal",
                                                                             "Cereal"))
credit.card$churn <- factor(credit.card$churn, levels = c(0,1), labels = c("No", "Yes"))
credit.card$tollfree <- factor(credit.card$tollfree, levels = c(0,1), labels = c("No", "Yes"))
credit.card$equip <- factor(credit.card$equip, levels = c(0,1), labels = c("No", "Yes"))
credit.card$callcard <- factor(credit.card$callcard, levels = c(0,1), labels = c("No", "Yes"))
credit.card$wireless <- factor(credit.card$wireless, levels = c(0,1), labels = c("No", "Yes"))
credit.card$multline <- factor(credit.card$multline, levels = c(0,1), labels = c("No", "Yes"))
credit.card$voice <- factor(credit.card$voice, levels = c(0,1), labels = c("No", "Yes"))
credit.card$pager <- factor(credit.card$pager, levels = c(0,1), labels = c("No", "Yes"))

credit.card$internet <- factor(credit.card$internet, levels = c(0,1,2,3,4), labels = c("None",
                                                                                       "Dial-up",
                                                                                       "DSL",
                                                                                       "Cable modem",
                                                                                       "Other"))

credit.card$callid <- factor(credit.card$callid, levels = c(0,1), labels = c("No", "Yes"))
credit.card$callwait <- factor(credit.card$callwait, levels = c(0,1), labels = c("No", "Yes"))
credit.card$forward <- factor(credit.card$forward, levels = c(0,1), labels = c("No", "Yes"))
credit.card$confer <- factor(credit.card$confer, levels = c(0,1), labels = c("No", "Yes"))
credit.card$ebill <- factor(credit.card$ebill, levels = c(0,1), labels = c("No", "Yes"))
credit.card$owntv <- factor(credit.card$owntv, levels = c(0,1), labels = c("No", "Yes"))
credit.card$ownvcr <- factor(credit.card$ownvcr, levels = c(0,1), labels = c("No", "Yes"))
credit.card$owndvd <- factor(credit.card$owndvd, levels = c(0,1), labels = c("No", "Yes"))
credit.card$owncd <- factor(credit.card$owncd, levels = c(0,1), labels = c("No", "Yes"))
credit.card$ownpda <- factor(credit.card$ownpda, levels = c(0,1), labels = c("No", "Yes"))
credit.card$ownpc <- factor(credit.card$ownpc, levels = c(0,1), labels = c("No", "Yes"))
credit.card$ownipod <- factor(credit.card$ownipod, levels = c(0,1), labels = c("No", "Yes"))
credit.card$owngame <- factor(credit.card$owngame, levels = c(0,1), labels = c("No", "Yes"))
credit.card$ownfax <- factor(credit.card$ownfax, levels = c(0,1), labels = c("No", "Yes"))
credit.card$news <- factor(credit.card$news, levels = c(0,1), labels = c("No", "Yes"))
credit.card$response_01 <- factor(credit.card$response_01, levels = c(0,1), labels = c("No", "Yes"))
credit.card$response_02 <- factor(credit.card$response_02, levels = c(0,1), labels = c("No", "Yes"))
credit.card$response_03 <- factor(credit.card$response_03, levels = c(0,1), labels = c("No", "Yes"))

summary(credit.card)

#------------------------------Dealing with Missing values
ln <- which(apply(credit.card, 2, function(x){sum(is.na(x))})>1000)
ln
library(dplyr)

# Removing variables with large number of missing values
credit.card <- credit.card %>% select(-lntollmon, -lntollten, -lnequipmon, -lnequipten, 
                                      -lncardmon,  -lncardten,  -lnwiremon,  -lnwireten)

# Status of no of missing values
apply(credit.card, 2, function(x){sum(is.na(x))})

# Keeping complete cases
credit.card <- credit.card[complete.cases(credit.card),]
str(credit.card)

#--------------------------Checking on Skewness of numeric predictors

# filtering out the numeric predictors
credit.card.num <- credit.card[,sapply(credit.card, is.numeric)]
str(credit.card.num)

# Skewness statistic
library(e1071)
apply(credit.card.num, 2, skewness)
summary(credit.card.num)

#--------------------------Looking at predictors with redundant information

#-------------near Zero Variance predictors
nz <- nearZeroVar(credit.card)
names(credit.card[,nz])

# Removing near Zero Variance predictors
credit.card <- credit.card[,-nz]

#-------------Correlated Predictors

str(credit.card)
# Numeric Predictors
credit.card.num <- credit.card[,sapply(credit.card, is.numeric)]
str(credit.card.num)

# correlations
correlations <- cor(credit.card.num)
library(corrplot)
corrplot(correlations, order = "hclust")

library(caret)
n <- findCorrelation(correlations, cutoff = 0.75)
n
names(credit.card.num[,n])

# Removing correlated predictors
#credit.card <- credit.card %>% select(-income, -lninc, -tenure, -lnlongten, -lnlongmon, -longten,       
                                       #-wireten, -equipten, -pets_freshfish)

#-----------------------------------Creating Outcome Variable
library(dplyr)
credit.card <- credit.card %>% mutate(total.card.spent = cardspent + card2spent) %>% 
  select(-cardspent, -card2spent)
str(credit.card)
summary(credit)

#-----------------------------------Checking on the Outliers
# Numeric Predictors
credit.card.num <- credit.card[,sapply(credit.card, is.numeric)]
str(credit.card.num)

n.outlier <- function(x){
  a <- mean(x) - 3*sd(x)
  b <- mean(x) + 3*sd(x)
  sum(x < a | x > b)
}

apply(credit.card.num, 2, n.outlier)

#------------------------------------Linear combinations 
findLinearCombos(credit.card.num)

#----------------------------------Splitting the Data

cc <- credit.card
set.seed(234)
cc.sampling.vector <- createDataPartition(cc$total.card.spent, p = 0.70, list = FALSE)

which(names(cc) == "total.card.spent")

cc.train <- cc[cc.sampling.vector,]
cc.train.pred <- cc.train[,-which(names(cc) == "total.card.spent")]
cc.train.output <- cc.train[,which(names(cc) == "total.card.spent")]

cc.test <- cc[-cc.sampling.vector,]
cc.test.pred <- cc.test[,-which(names(cc) == "total.card.spent")]
cc.test.output <- cc.test[,which(names(cc) == "total.card.spent")]

#---------------------------------Training the model

#-------------------Null model
cc.model.null <- lm(total.card.spent ~ 1, data = cc.train)
summary(cc.model.null)

# Baseline model performance
test.pred.null <- mean(cc.test.output)
RMSE(test.pred.null, cc.test.output) #355.42

# -----------------------------------Model1: all Variables
cc.model1 <- lm(total.card.spent~., data = cc.train)
summary(cc.model1)

par(mfrow = c(2,2))
plot(cc.model1)

#--------------------------------------Model 2: step function
# Using step function to select important predictors
cc.model2 <- step(cc.model.null, scope =
                    list(lower = cc.model.null, upper = cc.model1),
                  direction = "both")
summary(cc.model2)

# Test set performance
test.pred <- predict(cc.model2, newdata = cc.test.pred)
RMSE(test.pred, cc.test.output) #245.64

# Checking model assumptions
par(mfrow = c(2,2))
plot(cc.model2)

# The Residual plots appear fine, without any pattern, so the Linearity assumption holds
# Also, the variance of the residuals doesn't seem to vary with the fitted values. This gives
# a proof of homoscedasticity

par(mfrow = c(1,2))
plot(predict(cc.model2), rstudent(cc.model2))
plot(hatvalues(cc.model2))

# outliers
o <- which(rstudent(cc.model2) > 3)
o

#----------------------------------Model3: Removing influential outliers
# updating the model2 by removing outliers in the training data

cc.train.u <- cc.train[-o,]

cc.model3 <- lm(formula = total.card.spent ~ card2items + carditems + lninc + 
                  card + card2 + reason + income + voice + internet + forward + 
                  commuterail + gender + ownpc + ed + vote + equip + othdebt + 
                  lnothdebt + equipmon + ownpda + pets_cats + owngame, data = cc.train.u)
summary(cc.model3)

# Test set performance
test.pred2 <- predict(cc.model3, newdata = cc.test.pred)
RMSE(test.pred2, cc.test.output) # 247.8816
#RMSE has increased

#------------------------------Model4: Removing High leverage points and outliers

# removing both outliers and high leverage points
w <- abs(rstudent(cc.model2)) > 3 | abs(cooks.distance(cc.model2)) > 4/nrow(cc.model2$model)
ol <- which(w)
ol
cc.train.u2 <- cc.train[-ol,]

# training the model 4
cc.model4 <- lm(formula = total.card.spent ~ card2items + carditems + lninc + 
                  card + card2 + reason + income + voice + internet + forward + 
                  commuterail + gender + ownpc + ed + vote + equip + othdebt + 
                  lnothdebt + equipmon + ownpda + pets_cats + owngame, data = cc.train.u2)
summary(cc.model4)

# test set performance
test.pred3 <- predict(cc.model4, newdata = cc.test.pred)
RMSE(test.pred3, cc.test.output) #252.1227


# Multiple R-squared:  0.6308,	Adjusted R-squared:  0.627
# RMSE: 252.12
