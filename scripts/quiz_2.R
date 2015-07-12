# Question 2
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]



library(ggplot2)
ggplot(concrete, aes(x = SuperPlasticizer)) + 
  geom_histogram()# + 
    # scale_x_log10()
hist(log10(concrete$Superplasticizer + 1))


# Question 3
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

library(magrittr)
vars <- names(training) %>% 
  .[stringr::str_sub(.,1,2) == 'IL']
predata <- training[vars]

caret::preProcess(predata, method = 'pca', thresh = 0.80)



# Question 4
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

library(magrittr)
vars <- names(training) %>% 
  .[stringr::str_sub(.,1,2) == 'IL']
predata <- training[c('diagnosis', vars)]

m1 <- caret::train(diagnosis ~ ., data = predata, method = 'glm')
m2 <- caret::train(diagnosis ~ ., data = predata, method = 'glm', preProcess = 'pca', trControl = trainControl(preProcOptions = list(thresh = 0.80)))

out1 <- predict(m1, testing[c('diagnosis', vars)])
out2 <- predict(m2, testing[c('diagnosis', vars)])
caret::confusionMatrix(testing$diagnosis, out1)
caret::confusionMatrix(testing$diagnosis, out2)

