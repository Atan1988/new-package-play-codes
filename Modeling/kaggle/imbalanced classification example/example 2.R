library(DMwR)
library(caret)
library(pROC)

library(tidyverse)
library(dplyr)

hyper <-read.csv('http://archive.ics.uci.edu/ml/machine-learning-databases/thyroid-disease/hypothyroid.data', header=F)
names <- read.csv('http://archive.ics.uci.edu/ml/machine-learning-databases/thyroid-disease/hypothyroid.names', header=F, sep='\t')[[1]]
names <- gsub(pattern =":|[.]",x = names, replacement="")
colnames(hyper) <- names

colnames(hyper) <-c("target", "age", "sex", "on_thyroxine", "query_on_thyroxine",
                    "on_antithyroid_medication", "thyroid_surgery", "query_hypothyroid",
                    "query_hyperthyroid", "pregnant", "sick", "tumor", "lithium",
                    "goitre", "TSH_measured", "TSH", "T3_measured", "T3", "TT4_measured",
                    "TT4", "T4U_measured", "T4U", "FTI_measured", "FTI", "TBG_measured",
                    "TBG")
hyper$target <- ifelse(hyper$target=='negative',0,1)

prop.table(table(hyper$target))

head(hyper,2)


ind <- sapply(hyper, is.factor)
hyper[ind] <- lapply(hyper[ind], as.character)

hyper[ hyper == "?" ] = NA
hyper[ hyper == "f" ] = 0
hyper[ hyper == "t" ] = 1
hyper[ hyper == "n" ] = 0
hyper[ hyper == "y" ] = 1
hyper[ hyper == "M" ] = 0
hyper[ hyper == "F" ] = 1

hyper[ind] <- lapply(hyper[ind], as.numeric)

repalceNAsWithMean <- function(x) {replace(x, is.na(x), mean(x[!is.na(x)]))}
hyper <- repalceNAsWithMean(hyper)

set.seed(1234)
splitIndex <- createDataPartition(hyper$target, p = .50,
                                  list = FALSE,
                                  times = 1)
trainSplit <- hyper[ splitIndex,]
testSplit <- hyper[-splitIndex,]

prop.table(table(trainSplit$target))

prop.table(table(testSplit$target))

ctrl <- trainControl(method = "cv", number = 5)
tbmodel <- train(target ~ ., data = trainSplit, method = "treebag",
                 trControl = ctrl)

predictors <- names(trainSplit)[names(trainSplit) != 'target']
pred <- predict(tbmodel$finalModel, testSplit[,predictors])

auc <- roc(testSplit$target, pred)
print(auc)

plot(auc, ylim=c(0,1), print.thres=TRUE, main=paste('AUC:',round(auc$auc[[1]],2)))
abline(h=1,col='blue',lwd=2)
abline(h=0,col='red',lwd=2)

####
trainSplit$target <- as.factor(trainSplit$target)
trainSplit_SMOTE <- SMOTE(target ~ ., trainSplit, perc.over = 100, perc.under=200)
trainSplit_SMOTE <- trainSplit_SMOTE %>% 
                        mutate(target = as.numeric(target))

prop.table(table(trainSplit_SMOTE$target))

tbmodel_SMOTE <- train(target ~ ., data = trainSplit_SMOTE, method = "treebag",
                 trControl = ctrl)

predictors <- names(trainSplit)[names(trainSplit) != 'target']
pred_SMOTE  <- predict(tbmodel_SMOTE$finalModel, testSplit[,predictors])

auc_SMOTE  <- roc(testSplit$target, pred_SMOTE )
print(auc_SMOTE )

####
library(ROSE)
trainSplit_rose <- 

