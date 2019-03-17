# Predict if a loan will get funded

library(rgeos)
library(rgdal)
library(ggplot2)
library(ggmap)
library(dplyr)
library(sp)
library(spdep)
library(raster)
library(RColorBrewer)
library(maptools)
library(classInt)
library(broom)
library(quantmod)

source("scripts/kiva_loan_cleaning.R")
#source("scripts/gis.R")

head(loans)

loans_map <- feather::read_feather("data/enriched_map_data.feather")
head(loans_map)
# Strip the GIS off it
loans_map <- unique(loans_map[, -which(names(loans_map) %in% c('lat', 'long', 'order'))])

full_df <- merge(loans, loans_map, by=c("country"), all.x = T)

### Format data for prediction

head(full_df)

pred_df <- full_df[, c("country", "funded_amount", "loan_amount", "activity"
                       , "sector", "posted_time", "funded_time", "term_in_months"
                       , "lender_count", "repayment_interval", "date", "any_female"
                       , "usd_amt", "total_loans", "mean_term", "mean_lenders"
                       , "mean_amt", "median_amt", "gender_ratio_mean_amt"
                       , "gender_ratio_median_amt", "POP_EST", "POP_RANK"
                       , "GDP_MD_EST", "ECONOMY", "INCOME_GRP", "SUBREGION", "REGION_WB"
                       , "CONTINENT")]

head(pred_df)

# Get rid of NA values
for(i in 1:length(names(pred_df))){
  pred_df[i,][is.na(pred_df[i,])] <- 99999
}

# Convert strings to factor
for(i in 1:length(names(pred_df))){
  if(is.character(pred_df[,i])){
    pred_df[,i] <- as.numeric(as.factor(pred_df[,i]))
  }
}

pred_df$INCOME_GRP <- as.numeric(pred_df$INCOME_GRP)
pred_df$ECONOMY <- as.numeric(pred_df$ECONOMY)
pred_df$SUBREGION <- as.numeric(pred_df$SUBREGION)
pred_df$REGION_WB <- as.numeric(pred_df$REGION_WB)
pred_df$CONTINENT <- as.numeric(pred_df$CONTINENT)


library(caret)
library(xgboost)
library(randomForest)
library(pROC)
library(mlbench)

# Create outcome- diff between funded and loan amt
pred_df$amt_diff <- (pred_df$loan_amount - pred_df$funded_amount)/pred_df$loan_amount



set.seed(1234)
outcome <- pred_df$amt_diff

partition <- createDataPartition(y=outcome,
                                 p=.8,
                                 list=F)
training <- pred_df[partition,]
testing <- pred_df[-partition,]

#Create matrices from the data frames
trainData<- as.matrix(training, rownames.force=NA)
testData<- as.matrix(testing, rownames.force=NA)

#Turn the matrices into sparse matrices
train2 <- as(trainData, "sparseMatrix")
test2 <- as(testData, "sparseMatrix")

vars <- c(1, 4:6, 8, 11:28) #choose the columns we want to use in the prediction matrix

trainD <- xgb.DMatrix(data = train2[,vars], label = train2[,"amt_diff"]) #Convert to xgb.DMatrix format

params_try <- expand.grid(nrounds = c(100, 150),
                          max_depth = c(3,5),
                          eta = c(0.05, 0.1)
                          , colsample_bytree = c(1, .5))

#Cross validate the model 
cvfunction <- function(grid){
  results <- list()
  
  for(i in 1:nrow(grid)){
    cv.sparse <- xgb.cv(data = trainD,
                        nrounds = grid[i, "nrounds"],
                        min_child_weight = .1,
                        max_depth = grid[i, "max_depth"],
                        eta = grid[i, "eta"],
                        subsample = 1,
                        colsample_bytree = grid[i, "colsample_bytree"],
                        booster = "gbtree",
                        eval_metric = "rmse",
                        verbose = TRUE,
                        print_every_n = 50,
                        nfold = 5,
                        nthread = 2,
                        objective="reg:linear")
    
    
    results[[i]] <- data.frame(cbind(grid[i, "nrounds"],  
                                     grid[i, "max_depth"],
                                     grid[i, "colsample_bytree"],
                                     grid[i, "eta"]),
                               cv.sparse$evaluation_log[grid[i, "nrounds"]])
    
  }
  results
}



resultstime <- cvfunction(params_try)
resultstime_df <- resultstime %>%
  bind_rows(resultstime) %>%
  dplyr::rename(trees = X1, depth = X2, eta = X3)



#### TRAINING ####

#Choose the parameters for the model
param <- list(min_child_weight = 0,
              max_depth = 5,
              eta = .05,
              subsample = 1,
              colsample_bytree = 1,
              booster = "gbtree",
              eval_metric = "rmse",
              objective="reg:linear")



#Train the model using those parameters
bstSparse <-
  xgb.train(params = param,
            data = trainD,
            nrounds = 125,
            watchlist = list(train = trainD),
            verbose = TRUE,
            print_every_n = 25,
            nthread = 2)

xgb.save(bstSparse, paste0("xgboost_profit.model"))

testD <- xgb.DMatrix(data = test2[,vars])
#Column names must match the inputs EXACTLY
prediction <- predict(bstSparse, testD) #Make the prediction based on the half of the training data set aside

#Put testing prediction and test dataset all together
test3 <- as.data.frame(as.matrix(test2))
prediction <- as.data.frame(as.matrix(prediction))
colnames(prediction) <- "prediction"
model_output <- cbind(test3, prediction)


write.csv(model_output, paste0("funding_predictions.csv"))

# =================== Evaluate Predictions ======================== ####

# RMSE
rmse_test <- RMSE(model_output[,"amt_diff"], model_output[,"prediction"])
rmse_test

# Feature Importance
importance <- xgb.importance(feature_names = colnames(train2[,vars]), model = bstSparse)

ggplot(importance[importance$Gain >= .01], aes(x=Feature, y=Gain))+
  geom_bar(stat="identity")+
  coord_flip()

# Predicted x Truth Sample Test
plotset <- model_output[sample(nrow(model_output), 150),]

ggplot(plotset, aes(y=amt_diff, x=prediction))+
  theme_bw()+
  geom_jitter()+
  geom_smooth(method="loess")+
  labs(title = "Testing the Predictions - Top 100", x="Predicted Funding", y="Ground Truth")



testglm <- lm(data =pred_df[, c(vars, 29)], formula = amt_diff ~ .)

testglm <- lm(data =pred_df[, c("country", "amt_diff", "sector"
                                , "ECONOMY", "CONTINENT")], formula = amt_diff ~ .)

summary(testglm)
