library(caret)
library(glmnet)
library(mltools)
library(plyr)
library(dplyr)
library(purrr)
library(mice)
library(ggplot2) 
set.seed(55)

#Import both train and test datasets and combine them into one dataset for applying the consistent operation on two datasets
dataraw<-read.csv("train.csv", stringsAsFactors = TRUE)
datapredict<-read.csv("test.csv", stringsAsFactors = TRUE)
datapredict$SalePrice = NA
datafull<-rbind(dataraw,datapredict)
dataforclean<-datafull
is.na(dataforclean)
count(is.na(dataforclean)=="TRUE")

## change NAs to cagegories that they are actually referring to
#Variables include:Alley, BsmtQual (basement height), BsmtCond (basement condition), BsmtExposure(basedment walls), BsmtFinType1 (basement rating), BsmtFinType2 (type 2 basement rating), FireplaceQu (fireplace quaility), GarageType (garage location), GarageFinish (interior garage), GarageQual(garage quality), GarageCond(garage condition), PoolQC (pool quality), Fence(fence quality), MiscFeature (miscellaneous feature)
levels(dataforclean$Alley)<-c(levels(dataforclean$Alley),"NoAlley") 
dataforclean$Alley[is.na(dataforclean$Alley)] <- "NoAlley"
levels(dataforclean$BsmtQual)<-c(levels(dataforclean$BsmtQual),"NoBase") 
dataforclean$BsmtQual[is.na(dataforclean$BsmtQual)] <- "NoBase"
levels(dataforclean$BsmtCond)<-c(levels(dataforclean$BsmtCond),"NoBase") 
dataforclean$BsmtCond[is.na(dataforclean$BsmtCond)] <- "NoBase"
levels(dataforclean$BsmtExposure)<-c(levels(dataforclean$BsmtExposure),"NoBase") 
dataforclean$BsmtExposure[is.na(dataforclean$BsmtExposure)] <- "NoBase"
levels(dataforclean$BsmtFinType1)<-c(levels(dataforclean$BsmtFinType1),"NoBase") 
dataforclean$BsmtFinType1[is.na(dataforclean$BsmtFinType1)] <- "NoBase"
levels(dataforclean$BsmtFinType2)<-c(levels(dataforclean$BsmtFinType2),"NoBase") 
dataforclean$BsmtFinType2[is.na(dataforclean$BsmtFinType2)] <- "NoBase"
levels(dataforclean$FireplaceQu)<-c(levels(dataforclean$FireplaceQu),"NoFire") 
dataforclean$FireplaceQu[is.na(dataforclean$FireplaceQu)] <- "NoFire"
levels(dataforclean$GarageType)<-c(levels(dataforclean$GarageType),"NoGarage") 
dataforclean$GarageType[is.na(dataforclean$GarageType)] <- "NoGarage"
levels(dataforclean$GarageFinish)<-c(levels(dataforclean$GarageFinish),"NoGarage") 
dataforclean$GarageFinish[is.na(dataforclean$GarageFinish)] <- "NoGarage"
levels(dataforclean$GarageQual)<-c(levels(dataforclean$GarageQual),"NoGarage") 
dataforclean$GarageQual[is.na(dataforclean$GarageQual)] <- "NoGarage"
levels(dataforclean$GarageCond)<-c(levels(dataforclean$GarageCond),"NoGarage") 
dataforclean$GarageCond[is.na(dataforclean$GarageCond)] <- "NoGarage"
levels(dataforclean$PoolQC)<-c(levels(dataforclean$PoolQC),"NoPool") 
dataforclean$PoolQC[is.na(dataforclean$PoolQC)] <- "NoPool"
levels(dataforclean$Fence)<-c(levels(dataforclean$Fence),"NoFence") 
dataforclean$Fence[is.na(dataforclean$Fence)] <- "NoFence"
levels(dataforclean$MiscFeature)<-c(levels(dataforclean$MiscFeature),"None") 
dataforclean$MiscFeature[is.na(dataforclean$MiscFeature)] <- "None"

##use Mode Imputation for missing values in categorical variables
#Variables include MSZoning, Utilities, Exterior1st, Exterior2nd, MasVnrType, Electrical, KitchenQual, Functional, SaleType
a<-unique(dataforclean$MSZoning[!is.na(dataforclean$MSZoning)])
mode<-a[which.max(tabulate(match(dataforclean$MSZoning, a)))]
dataforclean$MSZoning[is.na(dataforclean$MSZoning)]<-mode

a<-unique(dataforclean$Utilities[!is.na(dataforclean$Utilities)])
mode<-a[which.max(tabulate(match(dataforclean$Utilities, a)))]
dataforclean$Utilities[is.na(dataforclean$Utilities)]<-mode

a<-unique(dataforclean$Exterior1st[!is.na(dataforclean$Exterior1st)])
mode<-a[which.max(tabulate(match(dataforclean$Exterior1st, a)))]
dataforclean$Exterior1st[is.na(dataforclean$Exterior1st)]<-mode

a<-unique(dataforclean$Exterior2nd[!is.na(dataforclean$Exterior2nd)])
mode<-a[which.max(tabulate(match(dataforclean$Exterior2nd, a)))]
dataforclean$Exterior2nd[is.na(dataforclean$Exterior2nd)]<-mode

a<-unique(dataforclean$Electrical[!is.na(dataforclean$Electrical)])
mode<-a[which.max(tabulate(match(dataforclean$Electrical, a)))]
dataforclean$Electrical[is.na(dataforclean$Electrical)]<-mode

a<-unique(dataforclean$KitchenQual[!is.na(dataforclean$KitchenQual)])
mode<-a[which.max(tabulate(match(dataforclean$KitchenQual, a)))]
dataforclean$KitchenQual[is.na(dataforclean$KitchenQual)]<-mode

a<-unique(dataforclean$Functional[!is.na(dataforclean$Functional)])
mode<-a[which.max(tabulate(match(dataforclean$Functional, a)))]
dataforclean$Functional[is.na(dataforclean$Functional)]<-mode

a<-unique(dataforclean$SaleType[!is.na(dataforclean$SaleType)])
mode<-a[which.max(tabulate(match(dataforclean$SaleType, a)))]
dataforclean$SaleType[is.na(dataforclean$SaleType)]<-mode

a<-unique(dataforclean$MasVnrType[!is.na(dataforclean$MasVnrType)])
mode<-a[which.max(tabulate(match(dataforclean$MasVnrType, a)))]
dataforclean$MasVnrType[is.na(dataforclean$MasVnrType)]<-mode

#impute numerical missing values
data_numeric<-dplyr::select_if(dataforclean, is.numeric) %>% select(-SalePrice)
imputer<-preProcess(data_numeric, "medianImpute")
imputed_values<-predict(imputer, data_numeric)
data_categorical<-dplyr::select_if(dataforclean, negate(is.numeric))
data_cleanfull<-bind_cols(data_categorical,imputed_values,SalePrice=dataforclean$SalePrice)

is.na(data_cleanfull)
#check if there are still NAs except the second half of the sale variable,
#No NAs so the data cleaning process is competed

#re-separate dataset into train and test for model development
datapredict_cleanedfull<-subset(data_cleanfull, Id>1460)
dataraw_cleanedfull<-subset(data_cleanfull,Id<1461)

#Start the model by doing a simple linear regression 
reg1<-lm(SalePrice~MSZoning+Street+Alley+LotShape+LandContour+Utilities+LotConfig+LandSlope+Neighborhood+Condition1+Condition2+BldgType+HouseStyle+RoofStyle+RoofMatl+Exterior1st+Exterior2nd+MasVnrType+ExterQual+ExterCond+Foundation+BsmtQual+BsmtCond+BsmtExposure+BsmtFinType1+BsmtFinType2++Heating+HeatingQC+CentralAir+KitchenQual+Functional+FireplaceQu+GarageType+GarageFinish+GarageQual+GarageCond+PavedDrive+PoolQC+Fence+MiscFeature+SaleType+SaleCondition+MSSubClass+LotFrontage+LotArea+OverallQual+OverallCond+YearBuilt+YearRemodAdd+MasVnrArea+BsmtFinSF1+BsmtFinSF2+BsmtUnfSF+TotalBsmtSF+X1stFlrSF+X2ndFlrSF+LowQualFinSF +GrLivArea +BsmtFullBath +BsmtHalfBath +FullBath +HalfBath +BedroomAbvGr +KitchenAbvGr +TotRmsAbvGrd +Fireplaces +GarageYrBlt +GarageCars +GarageArea +WoodDeckSF +OpenPorchSF+EnclosedPorch+X3SsnPorch+ScreenPorch+PoolArea+MiscVal+MoSold+YrSold,dataraw_cleanedfull)
par(mfrow=c(2,2)) 
plot(reg1)
#based on the normal Q-Q plot, some of the numerical variables may not normally distributed
#as presented by the Cook's Distance plot, there are some influential outlives
predictnumber1<-predict(reg1, datapredict_cleanedfull)
predictnumber1
#write.csv(predictlasso,"try3.csv")
#result is 0.18099, this is the first submission try

par(mfrow=c(1,2)) 
hist(dataraw_cleanedfull$SalePrice)
hist(log(dataraw_cleanedfull$SalePrice))

#Try to use the Lasso, also apply log-transformation to the dependent variable
y<-log(dataraw_cleanedfull$SalePrice)
X<-model.matrix(Id~MSZoning+Street+Alley+LotShape+LandContour+Utilities+LotConfig+LandSlope+Neighborhood+Condition1+Condition2+BldgType+HouseStyle+RoofStyle+RoofMatl+Exterior1st+Exterior2nd+MasVnrType+ExterQual+ExterCond+Foundation+BsmtQual+BsmtCond+BsmtExposure+BsmtFinType1+BsmtFinType2++Heating+HeatingQC+CentralAir+KitchenQual+Functional+FireplaceQu+GarageType+GarageFinish+GarageQual+GarageCond+PavedDrive+PoolQC+Fence+MiscFeature+SaleType+SaleCondition+MSSubClass+LotFrontage+LotArea+OverallQual+OverallCond+YearBuilt+YearRemodAdd+MasVnrArea+BsmtFinSF1+BsmtFinSF2+BsmtUnfSF+TotalBsmtSF+X1stFlrSF+X2ndFlrSF+LowQualFinSF +GrLivArea +BsmtFullBath +BsmtHalfBath +FullBath +HalfBath +BedroomAbvGr +KitchenAbvGr +TotRmsAbvGrd +Fireplaces +GarageYrBlt +GarageCars +GarageArea +WoodDeckSF +OpenPorchSF+EnclosedPorch+X3SsnPorch+ScreenPorch+PoolArea+MiscVal+MoSold+YrSold,data_cleanfull)[,-1]
X<-cbind(data_cleanfull$Id,X)
datatrainlasso<-subset(X, X[,1]<=1460)
datatestlasso<-subset(X,X[,1]>1460)
lasso.fit<-glmnet(x = datatrainlasso, y = y, alpha = 1)
crossval <-  cv.glmnet(x = datatrainlasso, y = y, alpha = 1) 
penalty.lasso <- crossval$lambda.min
lasso.fit <-glmnet(x = datatrainlasso, y = y, alpha = 1, lambda = penalty.lasso)  
predictlasso <- exp(predict(lasso.fit, s = penalty.lasso, newx =datatestlasso))
#write.csv(predictlasso,"try4.csv")
###The results is 0.13752, this is the second submission try

##By reading the data dictionary and logically think there are some variables might work better as interaction terms
##sale condition*miscellaneous feature value
##pool area*pool condition
##GarageType,GarageYrBlt,GarageFinish,GarageCars,GarageArea,GarageQual,GarageCond
y<-log(dataraw_cleanedfull$SalePrice)
X<-model.matrix(Id~MSZoning+Street+Alley+LotShape+LandContour+Utilities+LotConfig+LandSlope+Neighborhood+Condition1+Condition2+BldgType+HouseStyle+RoofStyle+RoofMatl+Exterior1st+Exterior2nd+MasVnrType+ExterQual+ExterCond+Foundation+BsmtQual+BsmtCond+BsmtExposure+BsmtFinType1+BsmtFinType2++Heating+HeatingQC+CentralAir+KitchenQual+Functional+FireplaceQu+GarageType*GarageFinish*GarageQual*GarageCond*GarageYrBlt*GarageCars*GarageArea+PavedDrive+Fence+MiscFeature+SaleType+MSSubClass+LotFrontage+LotArea+OverallQual+OverallCond+YearBuilt+YearRemodAdd+MasVnrArea+BsmtFinSF1+BsmtFinSF2+BsmtUnfSF+TotalBsmtSF+X1stFlrSF+X2ndFlrSF+LowQualFinSF +GrLivArea +BsmtFullBath +BsmtHalfBath +FullBath +HalfBath +BedroomAbvGr +KitchenAbvGr +TotRmsAbvGrd +Fireplaces +WoodDeckSF +OpenPorchSF+EnclosedPorch+X3SsnPorch+ScreenPorch+PoolArea*PoolQC+MiscVal*SaleCondition+MoSold+YrSold,data_cleanfull)[,-1]
X<-cbind(data_cleanfull$Id,X)

datatrainlasso<-subset(X, X[,1]<=1460)
datatestlasso<-subset(X,X[,1]>1460)

lasso.fit<-glmnet(x = datatrainlasso, y = y, alpha = 1)
crossval <-  cv.glmnet(x = datatrainlasso, y = y, alpha = 1) 
penalty.lasso <- crossval$lambda.min
lasso.fit <-glmnet(x = datatrainlasso, y = y, alpha = 1, lambda = penalty.lasso)  
predictlasso <- exp(predict(lasso.fit, s = penalty.lasso, newx =datatestlasso))
write.csv(predictlasso,"try5.csv")
###The results is 0.14363, this is the third submission try

#try to do feature engineering by adding transformation and interaction terms.
ggplot(dataraw_cleanedfull, aes(x=MiscVal, y=SalePrice, shape=SaleCondition, color=SaleType)) +  geom_point(size=3)
hist(datapredict_cleanedfull$MiscVal) #MiscVal has outliers
ggplot(dataraw_cleanedfull, aes(x=GarageArea, y=SalePrice, shape=GarageFinish, color=GarageCond)) +  geom_point(size=3)
ggplot(dataraw_cleanedfull, aes(x=GarageArea, y=SalePrice, shape=GarageFinish, color=GarageQual)) +  geom_point(size=3)
ggplot(dataraw_cleanedfull, aes(x=YrSold, y=SalePrice, color=SaleType)) +  geom_point(size=3) #no finding
ggplot(dataraw_cleanedfull, aes(x=PoolArea, y=log(SalePrice), color=PoolQC)) +  geom_point(size=3)
#remove PoolQC and Pool Area as only a few of them are none zeros
ggplot(dataraw_cleanedfull, aes(x=Fireplaces, y=log(SalePrice), color=FireplaceQu)) +  geom_point(size=3)
#create interaction of number of fireplaces and fileplace quality
ggplot(dataraw_cleanedfull, aes(x=LotArea, y=log(SalePrice), color=SaleType)) +  geom_point(size=3)
max(data_cleanfull$LotArea)
max(dataraw_cleanedfull$LotArea) #also noted that there are a few outliers for LotArea
#add interaction term between sale type and lot area

#take another try
y<-log(dataraw_cleanedfull$SalePrice)
X<-model.matrix(Id~MSZoning+Street+Alley+LotShape+LandContour+Utilities+LotConfig+LandSlope+Neighborhood+Condition1+Condition2+BldgType+HouseStyle+RoofStyle+RoofMatl+Exterior1st+Exterior2nd+MasVnrType+ExterQual+ExterCond+Foundation+BsmtQual+BsmtCond+BsmtExposure+BsmtFinType1+BsmtFinType2++Heating+HeatingQC+CentralAir+KitchenQual+Functional+PavedDrive+Fence+MiscFeature+SaleType*LotArea+MSSubClass+LotFrontage+OverallQual+OverallCond+YearBuilt+YearRemodAdd+MasVnrArea+BsmtFinSF1+BsmtFinSF2+BsmtUnfSF+TotalBsmtSF+X1stFlrSF+X2ndFlrSF+LowQualFinSF+GrLivArea +BsmtFullBath +BsmtHalfBath +FullBath +HalfBath +BedroomAbvGr +KitchenAbvGr +TotRmsAbvGrd +Fireplaces*FireplaceQu +WoodDeckSF +OpenPorchSF+EnclosedPorch+X3SsnPorch+ScreenPorch+MiscVal+SaleCondition+MoSold+YrSold+GarageType*GarageFinish*GarageQual*GarageCond*GarageYrBlt+GarageType*GarageFinish*GarageQual*GarageCond*GarageCars+GarageType*GarageFinish*GarageQual*GarageCond*GarageArea,data_cleanfull)[,-1]
X<-cbind(data_cleanfull$Id,X)

datatrainlasso<-subset(X, X[,1]<=1460)
datatestlasso<-subset(X,X[,1]>1460)

lasso.fit<-glmnet(x = datatrainlasso, y = y, alpha = 1)
crossval <-  cv.glmnet(x = datatrainlasso, y = y, alpha = 1) 
penalty.lasso <- crossval$lambda.min
lasso.fit <-glmnet(x = datatrainlasso, y = y, alpha = 1, lambda = penalty.lasso)  
predictlasso <- exp(predict(lasso.fit, s = penalty.lasso, newx =datatestlasso))
#write.csv(predictlasso,"try6.csv")
###The results is 0.13328, this is the forth submission try

#work on outliers for categorical varaibles
aa<-unique(data_cleanfull$MSZoning)
tabulate(match(data_cleanfull$MSZoning,aa))
aa<-unique(data_cleanfull$Street)
tabulate(match(data_cleanfull$Street,aa))
#remove Street
aa<-unique(data_cleanfull$Alley)
tabulate(match(data_cleanfull$Alley,aa))
aa<-unique(data_cleanfull$LotShape)
tabulate(match(data_cleanfull$LotShape,aa))
aa<-unique(data_cleanfull$LandContour)
tabulate(match(data_cleanfull$LandContour,aa))
aa<-unique(data_cleanfull$Utilities)
tabulate(match(data_cleanfull$Utilities,aa))
#remove Utilities
aa<-unique(data_cleanfull$LotConfig)
tabulate(match(data_cleanfull$LotConfig,aa))
aa<-unique(data_cleanfull$LandSlope)
tabulate(match(data_cleanfull$LandSlope,aa))
aa<-unique(data_cleanfull$Neighborhood)
tabulate(match(data_cleanfull$Neighborhood,aa))
aa<-unique(data_cleanfull$Condition1)
tabulate(match(data_cleanfull$Condition1,aa))
aa<-unique(data_cleanfull$Condition2)
tabulate(match(data_cleanfull$Condition2,aa))
#remove condition2
aa<-unique(data_cleanfull$BldgType)
tabulate(match(data_cleanfull$BldgType,aa))
aa<-unique(data_cleanfull$HouseStyle)
tabulate(match(data_cleanfull$HouseStyle,aa))
aa<-unique(data_cleanfull$RoofStyle)
tabulate(match(data_cleanfull$RoofStyle,aa))
aa<-unique(data_cleanfull$Exterior1st)
tabulate(match(data_cleanfull$Exterior1st,aa))
aa<-unique(data_cleanfull$Exterior2nd)
tabulate(match(data_cleanfull$Exterior2nd,aa))
aa<-unique(data_cleanfull$RoofMatl)
tabulate(match(data_cleanfull$RoofMatl,aa))
#remove RoofMatl
aa<-unique(data_cleanfull$MasVnrType)
tabulate(match(data_cleanfull$MasVnrType,aa))
aa<-unique(data_cleanfull$ExterQual)
tabulate(match(data_cleanfull$ExterQual,aa))
aa<-unique(data_cleanfull$ExterCond)
tabulate(match(data_cleanfull$ExterCond,aa))
aa<-unique(data_cleanfull$Foundation)
tabulate(match(data_cleanfull$Foundation,aa))
aa<-unique(data_cleanfull$BsmtQual)
tabulate(match(data_cleanfull$BsmtQual,aa))
aa<-unique(data_cleanfull$BsmtCond)
tabulate(match(data_cleanfull$BsmtCond,aa))
aa<-unique(data_cleanfull$BsmtExposure)
tabulate(match(data_cleanfull$BsmtExposure,aa))
aa<-unique(data_cleanfull$BsmtFinType1)
tabulate(match(data_cleanfull$BsmtFinType1,aa))
aa<-unique(data_cleanfull$BsmtFinType2)
tabulate(match(data_cleanfull$BsmtFinType2,aa))
aa<-unique(data_cleanfull$Heating)
tabulate(match(data_cleanfull$Heating,aa))
#remove Heating
aa<-unique(data_cleanfull$HeatingQC)
tabulate(match(data_cleanfull$HeatingQC,aa))
aa<-unique(data_cleanfull$CentralAir)
tabulate(match(data_cleanfull$CentralAir,aa))#PENDING
aa<-unique(data_cleanfull$Electrical)
tabulate(match(data_cleanfull$Electrical,aa))#consider to add back
aa<-unique(data_cleanfull$KitchenQual)
tabulate(match(data_cleanfull$KitchenQual,aa))
aa<-unique(data_cleanfull$Functional)
tabulate(match(data_cleanfull$Functional,aa))#PENDING
aa<-unique(data_cleanfull$FireplaceQu)
tabulate(match(data_cleanfull$FireplaceQu,aa))
aa<-unique(data_cleanfull$GarageType)
tabulate(match(data_cleanfull$GarageType,aa))
aa<-unique(data_cleanfull$GarageFinish)
tabulate(match(data_cleanfull$GarageFinish,aa))
aa<-unique(data_cleanfull$GarageQual)
tabulate(match(data_cleanfull$GarageQual,aa))#pending
aa<-unique(data_cleanfull$GarageCond)
tabulate(match(data_cleanfull$GarageCond,aa))#pending
aa<-unique(data_cleanfull$PavedDrive)
tabulate(match(data_cleanfull$PavedDrive,aa))#pending
aa<-unique(data_cleanfull$PoolQC)
tabulate(match(data_cleanfull$PoolQC,aa))
#remove PoolQC
aa<-unique(data_cleanfull$Fence)
tabulate(match(data_cleanfull$Fence,aa))
aa<-unique(data_cleanfull$MiscFeature)
tabulate(match(data_cleanfull$MiscFeature,aa))
#remove MiscFeature
aa<-unique(data_cleanfull$SaleType)
tabulate(match(data_cleanfull$SaleType,aa))
aa<-unique(data_cleanfull$SaleCondition)
tabulate(match(data_cleanfull$SaleCondition,aa))

#take another try
y<-log(dataraw_cleanedfull$SalePrice)
X<-model.matrix(Id~MSZoning+Alley+LotShape+LandContour+LotConfig+LandSlope+Neighborhood+Condition1+BldgType+HouseStyle+RoofStyle+Exterior1st+Exterior2nd+MasVnrType+ExterQual+ExterCond+Foundation+BsmtQual+BsmtCond+BsmtExposure+BsmtFinType1+BsmtFinType2+HeatingQC+CentralAir+KitchenQual+Functional+PavedDrive+Fence+MiscFeature+SaleType*LotArea+MSSubClass+LotFrontage+OverallQual+OverallCond+YearBuilt+YearRemodAdd+MasVnrArea+BsmtFinSF1+BsmtFinSF2+BsmtUnfSF+TotalBsmtSF+X1stFlrSF+X2ndFlrSF+LowQualFinSF +GrLivArea +BsmtFullBath +BsmtHalfBath +FullBath +HalfBath +BedroomAbvGr +KitchenAbvGr +TotRmsAbvGrd +Fireplaces*FireplaceQu +WoodDeckSF +OpenPorchSF+EnclosedPorch+X3SsnPorch+ScreenPorch+MiscVal+SaleCondition+MoSold+YrSold+GarageType*GarageFinish*GarageQual*GarageCond*GarageYrBlt+GarageType*GarageFinish*GarageQual*GarageCond*GarageCars+GarageType*GarageFinish*GarageQual*GarageCond*GarageArea,data_cleanfull)[,-1]
X<-cbind(data_cleanfull$Id,X)
datatrainlasso<-subset(X, X[,1]<=1460)
datatestlasso<-subset(X,X[,1]>1460)

lasso.fit<-glmnet(x = datatrainlasso, y = y, alpha = 1)
crossval <-  cv.glmnet(x = datatrainlasso, y = y, alpha = 1) 
penalty.lasso <- crossval$lambda.min
lasso.fit <-glmnet(x = datatrainlasso, y = y, alpha = 1, lambda = penalty.lasso)  
predictlasso <- exp(predict(lasso.fit, s = penalty.lasso, newx =datatestlasso))
#write.csv(predictlasso,"try7.csv")
###The results is 0.13285, this is the fifth submission try


##try to deal with numerical varaibles outliers
par(mfrow=c(2,2)) 
plot(data_cleanfull$MSSubClass)
plot(data_cleanfull$LotFrontage) #two outliers
data_cleanfull$LotFrontage[data_cleanfull$LotFrontage>250]<-mean(data_cleanfull$LotFrontage[data_cleanfull$LotFrontage<250])
plot(data_cleanfull$LotArea)
data_cleanfull$LotArea[data_cleanfull$LotArea>100000]<-mean(data_cleanfull$LotArea[data_cleanfull$LotArea<100000])
plot(data_cleanfull$OverallQual)
plot(data_cleanfull$OverallCond)
plot(data_cleanfull$YearBuilt) 
plot(data_cleanfull$YearRemodAdd)
plot(data_cleanfull$MasVnrArea)
data_cleanfull$MasVnrArea[data_cleanfull$MasVnrArea>1500]<-mean(data_cleanfull$MasVnrArea[data_cleanfull$MasVnrArea<1500])
plot(data_cleanfull$BsmtFinSF1)
data_cleanfull$BsmtFinSF1[data_cleanfull$BsmtFinSF1>5000]<-mean(data_cleanfull$BsmtFinSF1[data_cleanfull$BsmtFinSF1<5000])
plot(data_cleanfull$BsmtFinSF2)
plot(data_cleanfull$BsmtUnfSF)
plot(data_cleanfull$TotalBsmtSF)
data_cleanfull$TotalBsmtSF[data_cleanfull$TotalBsmtSF>5500]<-mean(data_cleanfull$TotalBsmtSF[data_cleanfull$TotalBsmtSF<5500])
plot(data_cleanfull$X1stFlrSF)
data_cleanfull$X1stFlrSF[data_cleanfull$X1stFlrSF>3500]<-mean(data_cleanfull$X1stFlrSF[data_cleanfull$X1stFlrSF<3500])
plot(data_cleanfull$X2ndFlrSF)
plot(data_cleanfull$LowQualFinSF)
data_cleanfull$LowQualFinSF[data_cleanfull$LowQualFinSF>800]<-mean(data_cleanfull$LowQualFinSF[data_cleanfull$LowQualFinSF<800])
plot(data_cleanfull$GrLivArea)
plot(data_cleanfull$BsmtFullBath)
data_cleanfull$BsmtFullBath[data_cleanfull$BsmtFullBath>2]<-2
plot(data_cleanfull$BsmtHalfBath)
data_cleanfull$BsmtHalfBath[data_cleanfull$BsmtHalfBath>1]<-1
plot(data_cleanfull$BedroomAbvGr)
data_cleanfull$BedroomAbvGr[data_cleanfull$BedroomAbvGr>6]<-6
plot(data_cleanfull$KitchenAbvGr)
data_cleanfull$KitchenAbvGr[data_cleanfull$KitchenAbvGr>2]<-2
plot(data_cleanfull$TotalBsmtSF)
plot(data_cleanfull$Fireplaces)
data_cleanfull$Fireplaces[data_cleanfull$Fireplaces>3]<-3
plot(data_cleanfull$GarageYrBlt)
data_cleanfull$GarageYrBlt[data_cleanfull$GarageYrBlt>2100]<-2007
plot(data_cleanfull$GarageCars)
data_cleanfull$GarageCars[data_cleanfull$GarageCars>4]<-4
plot(data_cleanfull$GarageArea)
plot(data_cleanfull$WoodDeckSF)
plot(data_cleanfull$OpenPorchSF)
plot(data_cleanfull$X3SsnPorch)#remove X3SsnPorch, as majority of entries are zero
plot(data_cleanfull$ScreenPorch)
plot(data_cleanfull$PoolArea)#removed previously (majority of entries are zero)
plot(data_cleanfull$MiscVal)
data_cleanfull$MiscVal[data_cleanfull$MiscVal>5000]<-mean(data_cleanfull$MiscVal[data_cleanfull$MiscVal<5000])
plot(data_cleanfull$YrSold)
plot(data_cleanfull$MoSold)

datapredict_cleanedfull<-subset(data_cleanfull, Id>1460)
dataraw_cleanedfull<-subset(data_cleanfull,Id<1461)

y<-log(dataraw_cleanedfull$SalePrice)
X<-model.matrix(Id~MSZoning+Alley+LotShape+LandContour+LotConfig+LandSlope+Neighborhood+Condition1+BldgType+HouseStyle+RoofStyle+Exterior1st+Exterior2nd+MasVnrType+ExterQual+ExterCond+Foundation+BsmtQual+BsmtCond+BsmtExposure+BsmtFinType1+BsmtFinType2+HeatingQC+CentralAir+KitchenQual+Functional+PavedDrive+Fence+MiscFeature+SaleType*LotArea+MSSubClass+LotFrontage+OverallQual+OverallCond+YearBuilt+YearRemodAdd+MasVnrArea+BsmtFinSF1+BsmtFinSF2+BsmtUnfSF+TotalBsmtSF+X1stFlrSF+X2ndFlrSF+LowQualFinSF +GrLivArea +BsmtFullBath +BsmtHalfBath +FullBath +HalfBath +BedroomAbvGr +KitchenAbvGr +TotRmsAbvGrd +Fireplaces*FireplaceQu +WoodDeckSF +OpenPorchSF+EnclosedPorch+ScreenPorch+MiscVal+SaleCondition+MoSold+YrSold+GarageType*GarageFinish*GarageQual*GarageCond*GarageYrBlt+GarageType*GarageFinish*GarageQual*GarageCond*GarageCars+GarageType*GarageFinish*GarageQual*GarageCond*GarageArea,data_cleanfull)[,-1]
X<-cbind(data_cleanfull$Id,X)
datatrainlasso<-subset(X, X[,1]<=1460)
datatestlasso<-subset(X,X[,1]>1460)

lasso.fit<-glmnet(x = datatrainlasso, y = y, alpha = 1)
crossval <-  cv.glmnet(x = datatrainlasso, y = y, alpha = 1) 
penalty.lasso <- crossval$lambda.min
lasso.fit <-glmnet(x = datatrainlasso, y = y, alpha = 1, lambda = penalty.lasso)  
predictlasso <- exp(predict(lasso.fit, s = penalty.lasso, newx =datatestlasso))

#write.csv(predictlasso,"try8.csv")
###The results is 0.12879, this is the sixth submission try

ridge.fit<-glmnet(x = datatrainlasso, y = y, alpha = 0)
crossval1 <-  cv.glmnet(x = datatrainlasso, y = y, alpha = 0)
penalty.ridge <- crossval1$lambda.min 
ridge.opt.fit <-glmnet(x =datatrainlasso, y = y, alpha = 0, lambda = penalty.ridge) 
predictridge<- exp(predict(ridge.opt.fit, s = penalty.ridge, newx =datatestlasso))
#write.csv(predictridge,"try9.csv")
###The results is 0.20555, this is the seventh submission try, ridge doesn't work well


#remove Functional, centralair, paveddrive
#try to do plots to add more interaction terms
ggplot(dataraw_cleanedfull, aes(x=X1stFlrSF, y=log(SalePrice), shape=Condition1, color=Neighborhood)) +  geom_point(size=3)
#Neighborhood*Condition1*X1stFlrSF + X2ndFlrSF*Neighborhood*Condition1
y<-log(dataraw_cleanedfull$SalePrice)
X<-model.matrix(Id~MSZoning+Alley+LotShape+LandContour+LotConfig+LandSlope+Neighborhood*Condition1*log(X1stFlrSF)+X2ndFlrSF*Neighborhood*Condition1+BldgType+HouseStyle+RoofStyle+Exterior1st+Exterior2nd+MasVnrType+ExterQual+ExterCond+Foundation+BsmtQual+BsmtCond+BsmtExposure+BsmtFinType1+BsmtFinType2+HeatingQC+KitchenQual+Fence+MiscFeature+SaleType*LotArea+MSSubClass+LotFrontage+OverallQual+OverallCond+YearBuilt+YearRemodAdd+MasVnrArea+BsmtFinSF1+BsmtFinSF2+BsmtUnfSF+TotalBsmtSF+LowQualFinSF +log(GrLivArea) +BsmtFullBath +BsmtHalfBath +FullBath +HalfBath +BedroomAbvGr +KitchenAbvGr +TotRmsAbvGrd +Fireplaces*FireplaceQu +WoodDeckSF +OpenPorchSF+EnclosedPorch+ScreenPorch+MiscVal+SaleCondition+MoSold+YrSold+GarageType*GarageFinish*GarageQual*GarageCond*GarageYrBlt+GarageType*GarageFinish*GarageQual*GarageCond*GarageCars+GarageType*GarageFinish*GarageQual*GarageCond*GarageArea,data_cleanfull)[,-1]
X<-cbind(data_cleanfull$Id,X)
lasso.fit<-glmnet(x = datatrainlasso, y = y, alpha = 1)
crossval <-  cv.glmnet(x = datatrainlasso, y = y, alpha = 1) 
penalty.lasso <- crossval$lambda.min
lasso.fit <-glmnet(x = datatrainlasso, y = y, alpha = 1, lambda = penalty.lasso)  
predictlasso <- exp(predict(lasso.fit, s = penalty.lasso, newx =datatestlasso))

#write.csv(predictlasso,"try10.csv")
###The results is 0.12880, this is the eighth submission try.

#addback Functional, centralair, paveddrive
#change mean to median
#data_cleanfull$LotFrontage[data_cleanfull$LotFrontage>250]<-median(data_cleanfull$LotFrontage[data_cleanfull$LotFrontage<250])
#data_cleanfull$LotArea[data_cleanfull$LotArea>100000]<-median(data_cleanfull$LotArea[data_cleanfull$LotArea<100000])
#data_cleanfull$MasVnrArea[data_cleanfull$MasVnrArea>1500]<-median(data_cleanfull$MasVnrArea[data_cleanfull$MasVnrArea<1500])
#data_cleanfull$BsmtFinSF1[data_cleanfull$BsmtFinSF1>5000]<-median(data_cleanfull$BsmtFinSF1[data_cleanfull$BsmtFinSF1<5000])
#data_cleanfull$TotalBsmtSF[data_cleanfull$TotalBsmtSF>5500]<-median(data_cleanfull$TotalBsmtSF[data_cleanfull$TotalBsmtSF<5500])
#data_cleanfull$X1stFlrSF[data_cleanfull$X1stFlrSF>3500]<-median(data_cleanfull$X1stFlrSF[data_cleanfull$X1stFlrSF<3500])
#data_cleanfull$LowQualFinSF[data_cleanfull$LowQualFinSF>800]<-median(data_cleanfull$LowQualFinSF[data_cleanfull$LowQualFinSF<800])

#datapredict_cleanedfull<-subset(data_cleanfull, Id>1460)
#dataraw_cleanedfull<-subset(data_cleanfull,Id<1461)

#y<-log(dataraw_cleanedfull$SalePrice)
#X<-model.matrix(Id~MSZoning+Alley+LotShape+LandContour+LotConfig+LandSlope+Neighborhood*Condition1*log(X1stFlrSF)+X2ndFlrSF*Neighborhood*Condition1+BldgType+HouseStyle+RoofStyle+Exterior1st+Exterior2nd+MasVnrType+ExterQual+ExterCond+Foundation+BsmtQual+BsmtCond+BsmtExposure+BsmtFinType1+BsmtFinType2+HeatingQC+KitchenQual+Fence+MiscFeature+SaleType*LotArea+MSSubClass+LotFrontage+OverallQual+OverallCond+YearBuilt+YearRemodAdd+MasVnrArea+BsmtFinSF1+BsmtFinSF2+BsmtUnfSF+TotalBsmtSF+LowQualFinSF +log(GrLivArea) +BsmtFullBath +BsmtHalfBath +FullBath +HalfBath +BedroomAbvGr +KitchenAbvGr +TotRmsAbvGrd +Fireplaces*FireplaceQu +WoodDeckSF +OpenPorchSF+EnclosedPorch+ScreenPorch+MiscVal+SaleCondition+MoSold+YrSold+GarageType*GarageFinish*GarageQual*GarageCond*GarageYrBlt+GarageType*GarageFinish*GarageQual*GarageCond*GarageCars+GarageType*GarageFinish*GarageQual*GarageCond*GarageArea +Functional+CentralAir+PavedDrive,data_cleanfull)[,-1]
#X<-cbind(data_cleanfull$Id,X)
#lasso.fit<-glmnet(x = datatrainlasso, y = y, alpha = 1)
#crossval <-  cv.glmnet(x = datatrainlasso, y = y, alpha = 1) 
#penalty.lasso <- crossval$lambda.min
#lasso.fit <-glmnet(x = datatrainlasso, y = y, alpha = 1, lambda = penalty.lasso)  
#predictlasso <- exp(predict(lasso.fit, s = penalty.lasso, newx =datatestlasso))
#write.csv(predictlasso,"try11.csv")
#The results is 0.13412, this the ninth submission try.

#back to mean and submit
#write.csv(predictlasso,"try12.csv")

#remove interaction on garage
datapredict_cleanedfull<-subset(data_cleanfull, Id>1460)
dataraw_cleanedfull<-subset(data_cleanfull,Id<1461)

y<-log(dataraw_cleanedfull$SalePrice)
X<-model.matrix(Id~MSZoning+Alley+LotShape+LandContour+LotConfig+LandSlope+Neighborhood+Condition1+BldgType+HouseStyle+RoofStyle+Exterior1st+Exterior2nd+MasVnrType+ExterQual+ExterCond+Foundation+BsmtQual+BsmtCond+BsmtExposure+BsmtFinType1+BsmtFinType2+HeatingQC+CentralAir+KitchenQual+Functional+PavedDrive+Fence+MiscFeature+SaleType*LotArea+MSSubClass+LotFrontage+OverallQual+OverallCond+YearBuilt+YearRemodAdd+MasVnrArea+BsmtFinSF1+BsmtFinSF2+BsmtUnfSF+TotalBsmtSF+X1stFlrSF+X2ndFlrSF+LowQualFinSF +GrLivArea +BsmtFullBath +BsmtHalfBath +FullBath +HalfBath +BedroomAbvGr +KitchenAbvGr +TotRmsAbvGrd +Fireplaces*FireplaceQu +WoodDeckSF +OpenPorchSF+EnclosedPorch+ScreenPorch+MiscVal+SaleCondition+MoSold+YrSold+GarageType+GarageFinish+GarageQual+GarageCond+GarageYrBlt+GarageCars+GarageArea,data_cleanfull)[,-1]
X<-cbind(data_cleanfull$Id,X)
datatrainlasso<-subset(X, X[,1]<=1460)
datatestlasso<-subset(X,X[,1]>1460)
lasso.fit<-glmnet(x = datatrainlasso, y = y, alpha = 1)
crossval <-  cv.glmnet(x = datatrainlasso, y = y, alpha = 1) 
penalty.lasso <- crossval$lambda.min
lasso.fit <-glmnet(x = datatrainlasso, y = y, alpha = 1, lambda = penalty.lasso)  
predictlasso <- exp(predict(lasso.fit, s = penalty.lasso, newx =datatestlasso))

#write.csv(predictlasso,"try13.csv")
#0.12832 

##
mycontrol<-trainControl(method="cv")
glm_model<-train(log(SalePrice)~MSZoning+Alley+LotShape+LandContour+LotConfig+LandSlope+Neighborhood+Condition1+BldgType+HouseStyle+RoofStyle+Exterior1st+Exterior2nd+MasVnrType+ExterQual+ExterCond+Foundation+BsmtQual+BsmtCond+BsmtExposure+BsmtFinType1+BsmtFinType2+HeatingQC+CentralAir+KitchenQual+Functional+PavedDrive+Fence+MiscFeature+SaleType*LotArea+MSSubClass+LotFrontage+OverallQual+OverallCond+YearBuilt+YearRemodAdd+MasVnrArea+BsmtFinSF1+BsmtFinSF2+BsmtUnfSF+TotalBsmtSF+X1stFlrSF+X2ndFlrSF+LowQualFinSF +GrLivArea +BsmtFullBath +BsmtHalfBath +FullBath +HalfBath +BedroomAbvGr +KitchenAbvGr +TotRmsAbvGrd +Fireplaces*FireplaceQu +WoodDeckSF +OpenPorchSF+EnclosedPorch+ScreenPorch+MiscVal+SaleCondition+MoSold+YrSold+GarageType+GarageFinish+GarageQual+GarageCond+GarageYrBlt+GarageCars+GarageArea,data=dataraw_cleanedfull,method="glm",trControl = mycontrol)
glm_model
predictglm<-exp(predict(glm_model,datapredict_cleanedfull))
#write.csv(predictglm,"try14.csv")
#0.13538

##
datapredict_cleanedfull<-subset(data_cleanfull, Id>1460)
dataraw_cleanedfull<-subset(data_cleanfull,Id<1461)
par(mfrow=c(3,2)) 
hist(data_cleanfull$LotArea)
hist(log(data_cleanfull$LotArea))
hist(data_cleanfull$X1stFlrSF)
hist(log(data_cleanfull$X1stFlrSF))
hist(data_cleanfull$GrLivArea)
hist(log(data_cleanfull$GrLivArea))
#log on lotarea, X1stFlrSF, GrLivArea

y<-log(dataraw_cleanedfull$SalePrice)
X<-model.matrix(Id~MSZoning+Alley+LotShape+LandContour+LotConfig+LandSlope+Neighborhood+Condition1+BldgType+HouseStyle+RoofStyle+Exterior1st+Exterior2nd+MasVnrType+ExterQual+ExterCond+Foundation+BsmtQual+BsmtCond+BsmtExposure+BsmtFinType1+BsmtFinType2+HeatingQC+CentralAir+KitchenQual+Functional+PavedDrive+Fence+MiscFeature+SaleType*log(LotArea)+MSSubClass+LotFrontage+OverallQual+OverallCond+YearBuilt+YearRemodAdd+MasVnrArea+BsmtFinSF1+BsmtFinSF2+BsmtUnfSF+TotalBsmtSF+log(X1stFlrSF)+X2ndFlrSF+LowQualFinSF +log(GrLivArea) +BsmtFullBath +BsmtHalfBath +FullBath +HalfBath +BedroomAbvGr +KitchenAbvGr +TotRmsAbvGrd +Fireplaces*FireplaceQu +WoodDeckSF+OpenPorchSF+EnclosedPorch+ScreenPorch+MiscVal+SaleCondition+MoSold+YrSold+GarageType+GarageFinish+GarageQual+GarageCond+GarageYrBlt+GarageCars+GarageArea,data_cleanfull)[,-1]
X<-cbind(data_cleanfull$Id,X)
datatrainlasso<-subset(X, X[,1]<=1460)
datatestlasso<-subset(X,X[,1]>1460)
lasso.fit<-glmnet(x = datatrainlasso, y = y, alpha = 1)
crossval <-  cv.glmnet(x = datatrainlasso, y = y, alpha = 1) 
penalty.lasso <- crossval$lambda.min
lasso.fit <-glmnet(x = datatrainlasso, y = y, alpha = 1, lambda = penalty.lasso)  
predictlasso <- exp(predict(lasso.fit, s = penalty.lasso, newx =datatestlasso))
write.csv(predictlasso,"try15.csv")
#0.12596 12th try

par(mfrow=c(1,1)) 

plot(lasso.fit, xvar = "lambda")
plot(crossval)
log(penalty.lasso) #-5.585515
plot(crossval,xlim=c(-6,-4),ylim=c(0.006,0.04))
coef(lasso.fit) #86 variables have estimated coefficient 
