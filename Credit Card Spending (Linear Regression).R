# Essential Libraries
library(caret)
library(e1071)
library(corrplot)
library(MASS)
library(lattice)
library(leaps)

# Importing the Prepared data
cc <- read.csv("creditcard.csv")

#################################################
# Dealing with missing Values
#################################################

# Number of Missing values
numMissing <- apply(cc, 2, function(x){sum(is.na(x))})
numMissing

# Removing observations with missing values
cc <- cc[complete.cases(cc),]

#################################################
# Separate out Predictors and Outcome Variable
#################################################
ccPred <- cc[,-ncol(cc)]
ccOut <- cc[,ncol(cc)]

#################################################
# Fixing Skewed Predictors
#################################################

# Determining the skewness of continuous predictors
ccContPred <- ccPred[,sapply(ccPred, is.numeric)]

skewness <- apply(ccContPred, 2, skewness)
summary(skewness)

# Using Box-Cox Transformation to transform skewed predictors
boxcoxtrans <- preProcess(ccPred, method = "BoxCox")
boxcoxtrans

# 23 continuous predictors were not transformed due to zero or negative values 
# 5 predictors were transformed with lambda values between -1 and 1 

ccPred <- predict(boxcoxtrans, ccPred)

#################################################
# Two sets of Predictors: fullSet and reducedSet
#################################################

# full set: ccPred
fullSet <- ccPred

#################################################
# Preparing reducedSet
#################################################

# Checking for multicollinearity

# Using PCA to detect multi-collinearity
ccContPred <- ccPred[,sapply(ccPred, is.numeric)]
pcaObject <- prcomp(ccContPred, center = TRUE, scale. = TRUE)
ls(pcaObject)
percentVariance <- pcaObject$sdev^2/sum(pcaObject$sdev^2)*100
percentVariance[1:3]
plot(percentVariance, xlab = "Components", ylab = "Cumulative Percent of Variance", type = "b")

# The amount of variability summarized by components drop sharply, 
# with no one component accounting for more than 17% of the variance. 
# This profile indicates that the structure of the data is contained 
# in a much smaller number of dimensions than the number of dimensions of the original space. 

# This is often due to a large number of collinearities among the predictors.

# Correlation between Predictors
correlation <- cor(ccContPred)
corrplot(correlation, order = "hclust")

# There are many strong positive correlations (indicated by the large, dark blue circles)

# Removing Strongly piecewise-correlated predictors
pwcp <- findCorrelation(correlation, cutoff = 0.8)
names(ccContPred[pwcp])
ccContPred <- ccContPred[,-pwcp]

# Merging with Categorical predictors
ccCatPred <- ccPred[,sapply(ccPred, is.factor)]

reducedSet <- cbind(ccContPred, ccCatPred)

# Removing Near-Zero Variance Predictors
nzvp <- nearZeroVar(reducedSet)
names(reducedSet[nzvp])
reducedSet <- reducedSet[,-nzvp]

#################################################
# Splitting the data into Training and Test set
#################################################

fullData <- cbind(fullSet, total.card.spend = ccOut)
reducedData <- cbind(reducedSet, total.card.spend = ccOut)

# Using Simple Random Sampling for the Split
set.seed(144)
train <- sample(4995, 3500)

fulltrain <- fullData[train,]
fulltrainPred <- fulltrain[,-ncol(fulltrain)]
fulltrainOut <- fulltrain[,ncol(fulltrain)]

fulltest <- fullData[-train,]
fulltestPred <- fulltest[,-ncol(fulltest)]
fulltestOut <- fulltest[,ncol(fulltest)]

reducedtrain <- reducedData[train,]
redtrainPred <- reducedtrain[,-ncol(reducedtrain)]
redtrainOut <- reducedtrain[,ncol(reducedtrain)]

reducedtest <- reducedData[-train,]
redtestPred <- reducedtest[,-ncol(reducedtest)]
redtestOut <- reducedtest[,ncol(reducedtest)]

#################################################
# Model Training
#################################################

########## Model 1: All Predictors
set.seed(144)
lmFitAllPred <- train(total.card.spend ~.,
                      data = fulltrain,
                      method = "lm",
                      trControl = trainControl(method = "cv", number = 10))
lmFitAllPred

# 10-Fold Cross Validation Estimates
# RMSE = 255.4
# R2 = 0.49


########## Model 2: Reduced Data
set.seed(144)
lmFitRedPred <- train(total.card.spend ~.,
                      data = reducedtrain,
                      method = "lm",
                      trControl = trainControl(method = "cv", number = 10))
lmFitRedPred

# 10-Fold Cross Validation Estimates
# RMSE = 254.85
# R2 = 0.498

########## Model 3: Using Stepwise selection to select the best model
set.seed(144)
lmFit1 <- train(total.card.spend~.,
                data = reducedtrain,
                method = "lmStepAIC",
                trControl = trainControl(method = "cv", number = 10))
lmFit1
# 10-fold Cross Validation Estimates
# RMSE
# R2


#-------------------------------------------





lmFitAll <- lm(total.card.spend ~., data = fulltrain)
lmFitSw <- stepAIC(lmFitAll, direction = "both")

lmFit3 <- train(total.card.spend ~ retire + inccat + debtinc + creddebt + othdebt + 
                  hometype + carcatvalue + carbought + reason + card + cardbenefit + 
                  card2 + card2fee + carditems + card2items + equipten + voice + 
                  forward + ownpc, data = fulltrain, method = "lm", 
                  trControl = trainControl(method = "cv", number = 10))
lmFit3
# 10-fold Cross Validation Estimates
# RMSE = 249.6
# R2 = 0.51


########## Model 4: Removing Influential Observations
#ls(lmFit1)
#w <- abs(rstudent(lmFit1)) > 3 | abs(cooks.distance(lmFit1)) > 4/nrow(lmFit1$model)
#InfObs <- which(w)
#InfObs
#fulltrainwoInf <- fulltrain[-InfObs,]

#set.seed(144)
#lmFit4 <- train()
#lmFit4

# 10-fold Cross Validation Estimates
# RMSE
# R2

#################################################
# Model Selection
#################################################
model <- lmFit3

#################################################
# Test Set Prediction and Results
################################################# 

lmPred <- predict(model, fulltestPred)
defaultSummary(data.frame(obs = fulltestOut, pred = lmPred))


#################################################
# Checking model Assumptions using Visualizations
#################################################

# Observed vs Predicted Values
xyplot(fulltrainOut ~ predict(model),
       type = c("p", "g"),
       xlab = "Predicted", ylab = "Observed")

# Residual Plots
xyplot(resid(lmFit1) ~ predict(model),
       type = c("p", "g"),
       xlab = "Predicted", ylab = "Residuals")

#################################################
# Determining variable imortance
################################################# 

importance <- varImp(model, scale=FALSE)

# summarize importance
print(importance)

# plot importance
plot(importance)
