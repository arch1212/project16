# Objective: Using Logistic Regression to create a classifier to Identify potential 
# churners from Proactive Attrition Management data for 70,000 customers.


# -----------------------------Importing the Data
pam <- read.csv("Proactive Attrition Management-Logistic Regression Case Study.csv")

# Exploring the data
str(pam)
summary(pam)

length(which(pam$INCOME==0)) #17750
length(which(pam$SETPRC==0)) #40249

#-----------------------------Preparing the Data

# changing labels for NA in 2 predictors
pam$INCOME[pam$INCOME == 0] <- NA
pam$SETPRC[pam$SETPRC == 0] <- NA

summary(pam$INCOME)
summary(pam$SETPRC)

# Changing data types for categorical predictors
fac <- c(22,33:64,67,68,70,72,74,76:78)

for(i in fac){
  pam[,i] <- as.factor(pam[,i])
}

str(pam)

# Removing customer id variable
which(names(pam) == "CUSTOMER")
pam <- pam[,-30]

#---------------------------------Examining Skewness of numerical predictors

# Separate out factors from numerical/interger predictors
fac <- which(sapply(pam, is.factor))
pam.num <- pam[,-fac]
str(pam.num)

# Calculating skewness for all numerical predictors using apply
library(e1071)
apply(pam.num, 2, skewness, na.rm = TRUE)
summary(pam.num)

    # Most predictors are right skewed, and include negative or zero values
    # therefore not suitable for log transformation.

#---------------------------------Examining missing values

# Number of missing values in all variables
apply(pam, 2, function(x){sum(is.na(x))})
mean(is.na(pam$INCOME))
mean(is.na(pam$SETPRC))
mean(is.na(pam$CHURNDEP))


    # The first 8 columns are missing the values of the same observations. 
    # additionally, 286 more observations are missing from 7th and 8th predictors
    # the AGE variables are missing values for 1244 observations. 

#----------------------------Removing predictors and keeping complete cases

# Removing INCOME, SETPRC and CHURNDEP
library(dplyr)
pam <- pam %>% select(-CHURNDEP, -INCOME, -SETPRC, -INCMISS, -SETPRCM)
str(pam)

# removing INCOME and SETPRC predictors and their associated predictors
pam <- pam %>% select(-INCOME, -SETPRC, -INCMISS, -SETPRCM)
str(pam)

# keeping all the complete cases
pam <- pam[complete.cases(pam),]
str(pam)

#----------------------------------------Examining predictors

#--------------------- near zero variance predictors [pam1]
nz <- nearZeroVar(pam)
nz
names(pam[,nz])

pam1 <- pam[,-nz]
str(pam1)

#--------------------- correlated predictors

# numeric/integer predictors

fac <- which(sapply(pam, is.factor))
pam.npred <- pam[,-fac]
str(pam.npred)

# converting interger predictors to numeric
for(i in 1:ncol(pam.npred)){
  pam.npred[,i] <- as.numeric(pam.npred[,i])
}

str(pam.npred)

# correlations
correlations <- cor(pam.npred)
corrplot(correlations, order = "hclust")

# numeric variables with pairwise correlation exceeding 0.70
cp <-findCorrelation(correlations, cutoff = 0.70)
cp
ncp <- names(pam.npred[,cp])
ncp

#------------------------------------Removing correlated predictors

pam.ncp <- pam %>% select(-MOU,-PEAKVCE, -OPEAKVCE,-OUTCALLS,-REVENUE,-DROPBLK,-PHONES,-UNIQSUBS,-RETACCPT)
str(pam.ncp)

#-----------------------------Splitting the data pam.ncp

pam.c1 <- pam.ncp %>% filter(CALIBRAT == "1")
pam.v1 <- pam.ncp %>% filter(CALIBRAT == "0")

str(pam.c1)
summary(pam.c1)

pam.c1 <- pam.c1 %>% select(-CALIBRAT)
pam.v1 <- pam.v1 %>% select(-CALIBRAT)

pam.c1.pred <- pam.c1 %>% select(-CHURN)
pam.c1.label <- pam.c1 %>% select(CHURN)

pam.v1.pred <- pam.v1 %>% select(-CHURN)
pam.v1.label <- pam.v1 %>% select(CHURN)

#---------------------------Baseline model

prop.table(table(pam.c1$CHURN))

# always predicting 0 i.e. no churn
# 50.2% accuracy of the baseline model

#-------------------Training Logistic Regression Classifier

pam.null.model <- glm(CHURN~1, data = pam.c1, family = binomial)

pam.model1 <- glm(CHURN~., data = pam.c1, family = binomial)
pred1 <- predict(pam.model1, type = "response")
table(pam.c1$CHURN, pred1 >= 0.5)
(11862+11403)/nrow(pam.c1)

# better than baseline model

# improving the model: feature selection
library(MASS)
#pam.model2 <- step(pam.null.model, scope = formula(pam.model1), direction = "forward", k = 2)

# removing CSA from the main data set
summary(pam.model1) #aic 53077
pam.model3 <- glm(CHURN~.-CSA, data = pam.c1, family = binomial)
pam.model4 <- step(pam.null.model, scope = formula(pam.model3), direction = "forward", k = 2)


#best model we get, add csa if aic improves keep it.
pam.model5 <- glm(CHURN ~ EQPDAYS + RETCALL + CREDITDE + AGE1 + REFURB + MONTHS + 
                    CHANGEM + CHANGER + RECCHRGE + OVERAGE + ROAM + CREDITC + 
                    MOUREC + MODELS + WEBCAP + MAILRES + CHILDREN + DROPVCE + 
                    INCALLS + CUSTCARE + NEWCELLY + PRIZMUB + THREEWAY + MARRYUN + 
                    BLCKVCE + CREDITAD + CREDITZ + OCCHMKR + MCYCLE + REFER + CSA,
                  data = pam.c1, family = binomial)
summary(pam.model5) #53046 vs. 52553.43 from model4

# ---------------------------------model4 gives the lowest AIC

#--------------------------------------choosing the threshold
# cost of making a FN > cost of making a FP
# i.e. we have to maximize the value of sensitivity and specificity

#----------------------------------Using ROC curve to choose a threshold
install.packages("ROCR")
library(ROCR)

ROCRpred <- prediction(pred2, pam.c1$CHURN)
ROCRperf <- performance(ROCRpred, "sens", "spec")
plot(ROCRperf)

ROCRperf@alpha.values[[1]][which.max(ROCRperf@x.values[[1]]+ROCRperf@y.values[[1]])]
# 0.4770975
max(ROCRperf@x.values[[1]]+ROCRperf@y.values[[1]])
# 1.162105

# The highest sensitivity plus specificity is achieved in this case when you predict 
# the positive outcome when the predicted probability exceeds 0.477 and predict the 
# negative outcome when the predicted probability does not exceed 0.477. 
# This yield a sensitivity plus specificity value of 1.16

#-----------------------------prediction over the training data
pred2 <- predict(pam.model4, type = "response")

# confusion matrix
library(caret)
cm <- table(pred2 > 0.477, pam.c1$CHURN)
cm
str(cm)
dimnames(cm)[[1]] = c("0","1")
cm

confusionMatrix(cm)


#------------------------------prediction on the validation set
pred.val <- predict(pam.model4, newdata = pam.v1, type = "response")

# confusion matrix
cm.val <- table(pred.val > 0.477, pam.v1$CHURN)
cm.val 
str(cm.val)
dimnames(cm.val)[[1]] = c("0","1")
cm.val
confusionMatrix(cm.val)

#calculating auc
ROCRpredVal <- prediction(pred.val, pam.v1$CHURN)
auc = as.numeric(performance(ROCRpredVal, "auc")@y.values)
auc #62%
  

# Interpretation of AUC
# Given a random customer from the dataset who actually churned, and 
# a random customer from the dataset who actually didn't churn,
# 62 % of the time our model will classify which is which correctly. 
