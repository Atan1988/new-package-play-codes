library(tidyverse)
library(reshape2)
library(readr)
library(ROSE)
library(DMwR)
library(caret)




setwd("~/GitHub/new package play codes/Modeling/kaggle")

train_df <- read_csv("train.zip")
test_df <- read_csv("test.zip")

train_df <- train_df %>% mutate(target = as.factor(target))
set.seed(1234)
splitIndex <- createDataPartition(train_df$target, p = .50,
                                  list = FALSE,
                                  times = 1)
train_df1  <- train_df[ splitIndex,]
valid_df <- train_df[-splitIndex,]



system.time(data.rose <- ROSE(target ~ ., data = train_df1 %>% select(-id), seed = 12345)$data)
system.time(data.rose1 <- ROSE(target ~ ., data = train_df1 %>% select(-id), N = 50000,  seed = 12345)$data)
system.time(data.under <- ovun.sample(target ~ ., data = train_df1 %>% select(-id), method = "under", 
                          N = 2 * train_df1 %>% filter(target == 1) %>% nrow(), seed = 1)$data)


system.time(tree.rose <- rpart(target ~ ., data = data.rose))
system.time(tree.under <- rpart(target ~ ., data = data.under))
system.time(tree.rose1 <- rpart(target ~ ., data = data.rose1))

#make predictions on unseen data
system.time(pred.tree.rose <- predict(tree.rose, newdata = valid_df))
system.time(pred.tree.under <- predict(tree.under, newdata = valid_df))
system.time(pred.tree.rose1 <- predict(tree.rose1, newdata = valid_df))

#AUC ROSE
roc.curve(valid_df$target, pred.tree.rose[,2])


#AUC ROSE1
roc.curve(valid_df$target, pred.tree.rose1[,2])


#AUC Undersampling
roc.curve(valid_df$target, pred.tree.under[,2])

