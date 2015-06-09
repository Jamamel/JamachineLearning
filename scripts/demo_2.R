library(caret)
library(kernlab)
library(ggplot2)

data(spam)

inTrain <- createDataPartition(y = spam$type,
                               p = 0.75,
                               list = FALSE)

training <- spam[inTrain,]
testing <- spam[-inTrain,]
dim(spam)

dim(training)
dim(testing)

ggplot(data = training, aes(x = capitalAve)) +
  geom_histogram() +
  xlab('ave. capital run length')

mean(training$capitalAve)
sd(training$capitalAve)

trainCapAve <- training$capitalAve
trainCapAveS <- scale(trainCapAve)
mean(trainCapAveS)
sd(trainCapAveS)

preObj <- preProcess(training[,-58], method = c('center', 'scale'))
trainCapAveS <- predict(preObj, training[,-58])$capitalAve
mean(trainCapAveS)
sd(trainCapAveS)

set.seed(32343)
modelFit <- train(type ~ ., data = training, preProcess = c('center', 'scale'), method = 'glm')
modelFit

preObj <- preProcess(training[,-58], method = c('BoxCox'))
capave <- predict(preObj, training[-58])$capitalAve
trainCapAveS <- data.frame(mean = capave)

ggplot(data = trainCapAveS, aes(x = mean)) + geom_histogram()
qqnorm(capave)

