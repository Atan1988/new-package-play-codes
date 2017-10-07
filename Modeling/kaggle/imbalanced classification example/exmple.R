setwd("~/GitHub/new package play codes/Modeling/kaggle/imbalanced classification example")


library(ROSE)
data(hacide)
str(hacide.train)

table(hacide.train$cls)

prop.table(table(hacide.train$cls))

library(rpart)
treeimb <- rpart(cls ~ ., data = hacide.train)
pred.treeimb <- predict(treeimb, newdata = hacide.test)

accuracy.meas(hacide.test$cls, pred.treeimb[,2])

roc.curve(hacide.test$cls, pred.treeimb[,2], plotit = F)

#over sampling
data_balanced_over <- ovun.sample(cls ~ ., data = hacide.train, method = "over",N = 1960)$data

#under sampling
data_balanced_under <- ovun.sample(cls ~ ., data = hacide.train, method = "under", N = 40, seed = 1)$data

###both
data_balanced_both <- ovun.sample(cls ~ ., data = hacide.train, method = "both", p=0.5,                             
                                  N=1000, seed = 1)$data

####sythetic
data.rose <- ROSE(cls ~ ., data = hacide.train, seed = 1)$data

####SMOTE
library(DMwR)
library(caret)

data.SMOTE <- SMOTE(cls ~ ., hacide.train, perc.over = 200, perc.under=100)
prop.table(table(train.SMOTE$cls))

#build decision tree models
tree.rose <- rpart(cls ~ ., data = data.rose)
tree.over <- rpart(cls ~ ., data = data_balanced_over)
tree.under <- rpart(cls ~ ., data = data_balanced_under)
tree.both <- rpart(cls ~ ., data = data_balanced_both)
tree.SMOTE <- rpart(cls ~., data = data.SMOTE)

#make predictions on unseen data
pred.tree.rose <- predict(tree.rose, newdata = hacide.test)
pred.tree.over <- predict(tree.over, newdata = hacide.test)
pred.tree.under <- predict(tree.under, newdata = hacide.test)
pred.tree.both <- predict(tree.both, newdata = hacide.test)
pred.tree.SMOTE <- predict(tree.SMOTE, newdata = hacide.test)

#AUC ROSE
roc.curve(hacide.test$cls, pred.tree.rose[,2])


#AUC Oversampling
roc.curve(hacide.test$cls, pred.tree.over[,2])


#AUC Undersampling
roc.curve(hacide.test$cls, pred.tree.under[,2])


#AUC Both
roc.curve(hacide.test$cls, pred.tree.both[,2])

#AUC SMOTE
roc.curve(hacide.test$cls, pred.tree.SMOTE[,2])

(ROSE.holdout <- ROSE.eval(cls ~ ., data = hacide.train, 
                          learner = rpart, method.assess = "holdout", 
                          extr.pred = function(obj)obj[,2], seed = 1))

(ROSE.BOOT <- ROSE.eval(cls ~ ., data = hacide.train, 
                           learner = rpart, method.assess = "BOOT", 
                           extr.pred = function(obj)obj[,2], seed = 1))


