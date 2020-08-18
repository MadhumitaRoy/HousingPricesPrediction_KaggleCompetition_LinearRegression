library(tidyverse)
library(caret)
library(glmnet)

# Reading both train and test csv files
train.csv.file <- read.csv("train.csv",header = TRUE,sep = ",")
test.csv.file <- read.csv("test.csv",header = TRUE,sep = ",")
# Combining into one file 
test.csv.file$SalePrice <- 0
Housing.Price <- rbind(train.csv.file,test.csv.file)

str(Housing.Price)
summary(Housing.Price)
head(Housing.Price)
tail(Housing.Price)

####  Step 1: Data Cleaning Process####
#Checking the number of NA's in the train file
map(Housing.Price, ~sum(is.na(.)))

# Replacing the NA's in lot frontage with the mean value
Housing.Price$LotFrontage[is.na(Housing.Price$LotFrontage)] <- 70.05
any(is.na(Housing.Price$LotFrontage))

# Replacing the NA's in Alley with "None". Changing the factor to character to replace. Then changing back to factor.
Housing.Price$Alley <- as.character(Housing.Price$Alley)
Housing.Price$Alley[is.na(Housing.Price$Alley)] <- "None"
Housing.Price$Alley <- as.factor(Housing.Price$Alley)
any(is.na(Housing.Price$Alley))

# Replacing the NA's in MasVnrType with "None". Changing the factor to character to replace. Then changing back to factor.
Housing.Price$MasVnrType <- as.character(Housing.Price$MasVnrType)
Housing.Price$MasVnrType[is.na(Housing.Price$MasVnrType)] <- "None"
Housing.Price$MasVnrType <- as.factor(Housing.Price$MasVnrType)
any(is.na(Housing.Price$MasVnrType))

# Replacing the NA's in MasVnrArea with the 0
Housing.Price$MasVnrArea[is.na(Housing.Price$MasVnrArea)] <- 0
any(is.na(Housing.Price$MasVnrArea))

# Replacing the NA's in BsmtQual with TA
Housing.Price$BsmtQual[is.na(Housing.Price$BsmtQual)] <- "TA"
any(is.na(Housing.Price$BsmtQual))

# Replacing the NA's in BsmtCond with TA
Housing.Price$BsmtCond[is.na(Housing.Price$BsmtCond)] <- "TA"
any(is.na(Housing.Price$BsmtCond))

# Replacing the NA's in BsmtExposure with No
Housing.Price$BsmtExposure[is.na(Housing.Price$BsmtExposure)] <- "No"
any(is.na(Housing.Price$BsmtExposure))

# Replacing the NA's in BsmtFinType1 with Unf
Housing.Price$BsmtFinType1[is.na(Housing.Price$BsmtFinType1)] <- "Unf"
any(is.na(Housing.Price$BsmtFinType1))

# Replacing the NA's in BsmtFinType2 with Unf
Housing.Price$BsmtFinType2[is.na(Housing.Price$BsmtFinType2)] <- "Unf"
any(is.na(Housing.Price$BsmtFinType2))

# Replacing the NA's in Electrical with SBrkr
Housing.Price$Electrical[is.na(Housing.Price$Electrical)] <- "SBrkr"
any(is.na(Housing.Price$Electrical))

# Replacing the NA's in FireplaceQu with None
Housing.Price$FireplaceQu <- as.character(Housing.Price$FireplaceQu)
Housing.Price$FireplaceQu[is.na(Housing.Price$FireplaceQu)] <- "None"
Housing.Price$FireplaceQu <- as.factor(Housing.Price$FireplaceQu)
any(is.na(Housing.Price$FireplaceQu))

# Replacing the NA's in GarageType with None
Housing.Price$GarageType <- as.character(Housing.Price$GarageType)
Housing.Price$GarageType[is.na(Housing.Price$GarageType)] <- "None"
Housing.Price$GarageType <- as.factor(Housing.Price$GarageType)
any(is.na(Housing.Price$GarageType))

# Replacing the NA's in GarageYrBlt with None
Housing.Price$GarageYrBlt[is.na(Housing.Price$GarageYrBlt)] <- "None"
any(is.na(Housing.Price$GarageYrBlt))
Housing.Price$GarageYrBlt <- as.factor(Housing.Price$GarageYrBlt)

# Replacing the NA's in GarageFinish with None
Housing.Price$GarageFinish <- as.character(Housing.Price$GarageFinish)
Housing.Price$GarageFinish[is.na(Housing.Price$GarageFinish)] <- "None"
Housing.Price$GarageFinish <- as.factor(Housing.Price$GarageFinish)
any(is.na(Housing.Price$GarageFinish))

# Replacing the NA's in GarageQual with None
Housing.Price$GarageQual <- as.character(Housing.Price$GarageQual)
Housing.Price$GarageQual[is.na(Housing.Price$GarageQual)] <- "None"
Housing.Price$GarageQual <- as.factor(Housing.Price$GarageQual)
any(is.na(Housing.Price$GarageQual))

# Replacing the NA's in GarageCond with None
Housing.Price$GarageCond <- as.character(Housing.Price$GarageCond)
Housing.Price$GarageCond[is.na(Housing.Price$GarageCond)] <- "None"
Housing.Price$GarageCond <- as.factor(Housing.Price$GarageCond)
any(is.na(Housing.Price$GarageCond))

# Replacing the NA's in PoolQC with None
Housing.Price$PoolQC <- as.character(Housing.Price$PoolQC)
Housing.Price$PoolQC[is.na(Housing.Price$PoolQC)] <- "None"
Housing.Price$PoolQC <- as.factor(Housing.Price$PoolQC)
any(is.na(Housing.Price$PoolQC))

# Replacing the NA's in Fence with None
Housing.Price$Fence <- as.character(Housing.Price$Fence)
Housing.Price$Fence[is.na(Housing.Price$Fence)] <- "None"
Housing.Price$Fence <- as.factor(Housing.Price$Fence)
any(is.na(Housing.Price$Fence))

# Replacing the NA's in MiscFeature with None
Housing.Price$MiscFeature <- as.character(Housing.Price$MiscFeature)
Housing.Price$MiscFeature[is.na(Housing.Price$MiscFeature)] <- "None"
Housing.Price$MiscFeature <- as.factor(Housing.Price$MiscFeature)
any(is.na(Housing.Price$MiscFeature))

map(Housing.Price, ~sum(is.na(.)))

#Changing name of variables 1stFlrSF and 2ndFlrSF
names(Housing.Price)[44] <- "FirstFlrSF"
names(Housing.Price)[45] <- "SecondFlrSF"
#Housing.Price$TotalBaths <- Housing.Price$BsmtFullBath+Housing.Price$BsmtHalfBath+Housing.Price$HalfBath+Housing.Price$FullBath
str(Housing.Price)

##This exist in the test file alone. Replacing NA's with the mode
Housing.Price$MSZoning[is.na(Housing.Price$MSZoning)] <- "RL"
Housing.Price$Utilities[is.na(Housing.Price$Utilities)] <- "AllPub"
Housing.Price$Exterior1st[is.na(Housing.Price$Exterior1st)] <- "VinylSd"
Housing.Price$Exterior2nd[is.na(Housing.Price$Exterior2nd)] <- "VinylSd"
Housing.Price$BsmtFinSF1[is.na(Housing.Price$BsmtFinSF1)] <- 0
Housing.Price$BsmtFinSF2[is.na(Housing.Price$BsmtFinSF2)] <- 0
Housing.Price$BsmtUnfSF[is.na(Housing.Price$BsmtUnfSF)] <- 0
Housing.Price$TotalBsmtSF[is.na(Housing.Price$TotalBsmtSF)] <- 0
Housing.Price$BsmtFullBath[is.na(Housing.Price$BsmtFullBath)] <- 0
Housing.Price$BsmtHalfBath[is.na(Housing.Price$BsmtHalfBath)] <- 0
Housing.Price$KitchenQual[is.na(Housing.Price$KitchenQual)] <- "TA"
Housing.Price$Functional[is.na(Housing.Price$Functional)] <- "Typ"
Housing.Price$GarageCars[is.na(Housing.Price$GarageCars)] <- 0
Housing.Price$GarageArea[is.na(Housing.Price$GarageArea)] <- 0
Housing.Price$SaleType[is.na(Housing.Price$SaleType)] <- "WD"
#Outlier in the test file. Year is 2207
Housing.Price[Housing.Price$Id==2593, "GarageYrBlt"] <- 2007

summary(Housing.Price)

### Step 2: Will use R and Tableau for visualization ####
#plot(SalePrice~LotArea,Housing.Price.modeling.file)
par(mfrow=c(1,1))
plot(SalePrice~FirstFlrSF,Housing.Price)

#Snapping back the outliers with id 1299 and 524 to the nearest first floor sqft value
Housing.Price[Housing.Price$Id==1299, "FirstFlrSF"] <- 2207
Housing.Price[Housing.Price$Id==524, "FirstFlrSF"] <- 2207
plot(SalePrice~FirstFlrSF,Housing.Price)

#Need to adjust GrLivArea accordingly. GrLivArea=FistflSF+SecondFlrSF
Housing.Price[Housing.Price$Id==1299, "GrLivArea"] <- 3475
Housing.Price[Housing.Price$Id==524, "GrLivArea"] <- 3157
plot(SalePrice~GrLivArea,Housing.Price)

#BsmtFinSF1
Housing.Price[Housing.Price$Id==1299, "BsmtFinSF1"] <- 1606
Housing.Price[Housing.Price$Id==524, "BsmtFinSF1"] <- 1606
plot(SalePrice~BsmtFinSF1,Housing.Price)

#TotalBsmtSF
Housing.Price[Housing.Price$Id==1299, "TotalBsmtSF"] <- 2484
Housing.Price[Housing.Price$Id==524, "TotalBsmtSF"] <- 2072
plot(SalePrice~TotalBsmtSF,Housing.Price)
#LotArea
Housing.Price[Housing.Price$Id==1299, "BsmtFinSF1"] <- 1606
Housing.Price[Housing.Price$Id==524, "BsmtFinSF1"] <- 1606
plot(SalePrice~BsmtFinSF1,Housing.Price)

#### Step 3: Building the non-linear model ####

##Splitting the model.file into train, test and predict. 70% for train,30% test

train.set <- subset(Housing.Price,Id<=1022)
test.set <- subset(Housing.Price,Id>1022 & Id<=1460)
predict.set <- subset(Housing.Price,Id>1460)
#sample <- sample.int(n = nrow(Housing.Price), size = floor(.85*nrow(Housing.Price)), replace = F)
#train <- Housing.Price[sample, ]
#test  <- Housing.Price[-sample, ]


fit.1 <- step(lm(log(SalePrice)~MSZoning+log(LotFrontage)+log(LotArea)+Street
                 +LotShape+LandContour+LotConfig+LandSlope+Neighborhood
                 +Condition1+BldgType+HouseStyle+log(OverallQual)
                 +log(OverallCond)+log(YearBuilt)+log(YearRemodAdd)
                 +ExterQual+BsmtQual+BsmtCond+BsmtExposure+BsmtFinSF1
                 +BsmtFinType2+BsmtFinSF2+BsmtUnfSF+HeatingQC+CentralAir
                 +FirstFlrSF+SecondFlrSF+log(GrLivArea)+BsmtFullBath+BsmtHalfBath
                 +FullBath+HalfBath+BedroomAbvGr+KitchenAbvGr+TotRmsAbvGrd
                 +Fireplaces+GarageCars+GarageArea+WoodDeckSF+OpenPorchSF
                 +EnclosedPorch+PoolArea+SaleCondition,train.set),direction = "both")

# Using the best model from step regression
fit.3 <- lm(log(SalePrice) ~ MSZoning + log(LotFrontage) + log(LotArea) + 
              Street + LotShape + LandContour + LotConfig + LandSlope + 
              Neighborhood + Condition1 + BldgType + HouseStyle + log(OverallQual) + 
              log(OverallCond) + log(YearBuilt) + log(YearRemodAdd) + ExterQual + 
              BsmtQual + BsmtCond + BsmtExposure + BsmtFinSF1 + BsmtFinType2 + 
              BsmtFinSF2 + BsmtUnfSF  + CentralAir + FirstFlrSF + 
              SecondFlrSF + log(GrLivArea) + BsmtFullBath + BsmtHalfBath + 
              FullBath + HalfBath + BedroomAbvGr + KitchenAbvGr + TotRmsAbvGrd + 
              Fireplaces + GarageCars + GarageArea + WoodDeckSF + OpenPorchSF + 
              EnclosedPorch + PoolArea + SaleCondition,train.set)

summary(fit.1)
par(mfrow=c(1,4))
plot(fit.3)
predict.price <- exp(predict(fit.3,test.set))

## MAPE calculation
error.percent <- abs(predict.price-test.set$SalePrice)/test.set$SalePrice *100
mean(error.percent)
## rmsle calculation 
y_true<-test.set$SalePrice
y_pred<-predict.price
sqrt(mean((log1p(y_true)-log1p(y_pred))^2))

#Trying interactions to reduce MAPE
fit.4 <- lm(log(SalePrice)~MSSubClass+MSZoning+(log(MSSubClass)*MSZoning)+log(LotFrontage)+log(LotArea)+Street+Alley+LotShape
            +LandContour+LotConfig+LandSlope+Neighborhood+Condition1+BldgType+HouseStyle+OverallQual
            +OverallCond+(OverallCond*OverallQual)+YearBuilt+YearRemodAdd+MasVnrType+sqrt(MasVnrArea)
            +BsmtQual+BsmtCond+BsmtExposure+BsmtFinType1+sqrt(BsmtFinSF1)+BsmtFinType2+sqrt(TotalBsmtSF)
            +sqrt(BsmtFinSF2)+sqrt(BsmtUnfSF)+CentralAir+log(FirstFlrSF)+sqrt(SecondFlrSF)+log(GrLivArea)
            +sqrt(BsmtFullBath)+sqrt(BsmtHalfBath)+sqrt(FullBath)+sqrt(HalfBath)+KitchenQual
            +Fireplaces+GarageType+GarageCars+sqrt(GarageArea)+sqrt(WoodDeckSF)+sqrt(OpenPorchSF)
            +sqrt(EnclosedPorch)+sqrt(ScreenPorch)+PoolArea+sqrt(MiscVal)+sqrt(LotFrontage)
            +(log(LotFrontage)*LandContour)+(YearBuilt*OverallCond)+(YearBuilt*sqrt(LotFrontage))
            +(log(FirstFlrSF)*GrLivArea)+(sqrt(ScreenPorch)*GrLivArea)+(log(LotArea)*YearRemodAdd)
            +(YearRemodAdd*CentralAir)+(YearRemodAdd*sqrt(ScreenPorch))+(YearRemodAdd*MasVnrType)
            +(YearRemodAdd*OverallQual*OverallCond)+(YearRemodAdd*GrLivArea)+(YearRemodAdd*KitchenQual)
            +(YearBuilt*HouseStyle),train.set)

summary(fit.4)
par(mfrow=c(1,4))
plot(fit.4)
predict.price.4 <- exp(predict(fit.4,test.set))

## MAPE calculation
error.percent.4 <- abs(predict.price.4-test.set$SalePrice)/test.set$SalePrice *100
mean(error.percent.4)

#Using fit.3 for prediction
#Predicting Sale Price and writing the csv
predict.price.test.file <- exp(predict(fit.3,predict.set))
#write.table(cbind(predict.set$Id,lasso.testing.prediction),file = "Non-linear Predicted House Prices.csv",row.names = F,col.names = c("Id","SalePrice"))

###
#### Step 4:Regularizations (LASSO and ridge)####
###
#create the y variable and matrix (capital X) of x variables (will make the code below easier to read + will ensure that all interactoins exist)
y<-log(train.set$SalePrice)
X<-model.matrix(Id~MSZoning + log(LotFrontage) + log(LotArea) + 
                  Street + LotShape + LandContour + LotConfig + LandSlope + 
                  Neighborhood + Condition1 + Condition2 + BldgType + HouseStyle + log(OverallQual) + 
                  log(OverallCond) + log(YearBuilt) + log(YearRemodAdd) + RoofMatl + ExterQual + Foundation
                + BsmtQual + BsmtCond + BsmtExposure + BsmtFinSF1 + BsmtFinType2 + 
                  BsmtFinSF2 + BsmtUnfSF  + HeatingQC + CentralAir + FirstFlrSF + 
                  SecondFlrSF + log(GrLivArea) + BsmtFullBath + BsmtHalfBath + 
                  FullBath + HalfBath + BedroomAbvGr + KitchenAbvGr + TotRmsAbvGrd + 
                  Fireplaces + GarageCars + GarageArea + WoodDeckSF + OpenPorchSF + 
                  EnclosedPorch + PoolArea + SaleCondition, Housing.Price)[,-1]
X<-cbind(Housing.Price$Id,X)

# split X into testing, trainig/holdout and prediction as before
X.training<-subset(X,X[,1]<=1022)
X.testing<-subset(X, (X[,1]>1022 & X[,1]<=1460))
X.prediction<-subset(X,X[,1]>=1461)

#LASSO (alpha=1)
lasso.fit<-glmnet(x = X.training, y = y, alpha = 1)
par(mfrow=c(1,1))
plot(lasso.fit, xvar = "lambda")

#selecting the best penalty lambda
crossval <-  cv.glmnet(x = X.training, y = y, alpha = 1) #create cross-validation data
plot(crossval)
penalty.lasso <- crossval$lambda.min #determine optimal penalty parameter, lambda
log(penalty.lasso) #see where it was on the graph
#plot(crossval,xlim=c(-8.5,-6),ylim=c(0.006,0.008)) # lets zoom-in
lasso.opt.fit <-glmnet(x = X.training, y = y, alpha = 1, lambda = penalty.lasso) #estimate the model with the optimal penalty
coef(lasso.opt.fit) #resultant model coefficients

# predicting the performance on the testing set
lasso.testing <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx =X.testing))
percent.Lasso.error <- abs(lasso.testing-test.set$SalePrice)/test.set$SalePrice*100 #calculate and display MAPE
mean(percent.Lasso.error)

#Predicting Sale Price with Lasso and writing the csv
lasso.testing.prediction <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx =X.prediction))
#write.table(cbind(predict.set$Id,lasso.testing.prediction),file = "Lasso Predicted House Prices_file2",row.names = F,col.names = c("Id","SalePrice"))

##Ridge Model
#ridge (alpha=0)

ridge.fit<-glmnet(x = X.training, y = y, alpha = 0)
plot(ridge.fit, xvar = "lambda")

#selecting the best penalty lambda
crossval <-  cv.glmnet(x = X.training, y = y, alpha = 0)
plot(crossval)
penalty.ridge <- crossval$lambda.min 
log(penalty.ridge) 
ridge.opt.fit <-glmnet(x = X.training, y = y, alpha = 0, lambda = penalty.ridge) #estimate the model with that
coef(ridge.opt.fit)

ridge.testing <- exp(predict(ridge.opt.fit, s = penalty.ridge, newx =X.testing))
mean(abs(ridge.testing-test.set$SalePrice)/test.set$SalePrice*100)





