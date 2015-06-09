library(caret)
library(kernlab)
# library(magrittr)

data(spam)

# smap <- dplyr::tbl_df(spam)

inTrain <- createDataPartition(y = spam$type,
                               p = 0.75,
                               list = FALSE)

training <- spam[inTrain,]# %>%
#   dplyr::tbl_df()
testing <- spam[-inTrain,]# %>%
#   dplyr::tbl_df()
dim(spam)

dim(training)
dim(testing)

# fit a model
set.seed(32343)

modelFit <- train(type ~ ., data = training, method = 'glm')
modelFit

modelFit$finalModel


predictions <- predict(modelFit, newdata = testing)
predictions

confusionMatrix(predictions, testing$type)


# create folds for cross-validation

folds <- createFolds(y = spam$type,
                     k = 10,
                     list = TRUE,
                     returnTrain = TRUE)
sapply(folds, length)
str(folds,1)


folds <- createFolds(y = spam$type,
                     k = 10,
                     list = TRUE,
                     returnTrain = FALSE)
sapply(folds, length)
str(folds,1)



folds <- createResample(y = spam$type,
                     times = 10,
                     list = TRUE)
sapply(folds, length)
str(folds,1)


tme <- 1:1000
folds <- createTimeSlices(y = tme,
                        initialWindow = 20,
                        horizon = 10)
names(folds)
str(folds,1)
folds$train[[1]]
folds$test[[1]]



modelFit <- train(type ~ ., data = training, method = 'glm')
modelFit

args(train.default)
args(trainControl)
