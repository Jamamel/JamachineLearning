# Question 1
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
d <- segmentationOriginal

# trainsample <- caret::createDataPartition(d$Case, p = 0.6)
trainsample <- which(d$Case == 'Train')

training <- d[trainsample, ]
testing <- d[-trainsample, ]
set.seed(125)


mod <- caret::train(Class ~ ., data = training, method = 'rpart')
mod
mod$finalModel

predict(mod)
View(training)
predict(mod, newdata = data.frame(TotalIntench2 = 23000))



# Question 3
library(pgmm)
data(olive)
olive = olive[,-1]

mod <- train(Area ~ ., data = olive, method = 'rpart')
predict(mod, newdata = as.data.frame(t(colMeans(olive))))



# Question 4
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

set.seed(13234)
mod <- caret::train(chd ~ age + alcohol + obesity + tobacco + typea + ldl, data = trainSA, method = 'glm', family = 'binomial')
mod
summary(mod)

missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
missClass(trainSA$chd, predict(mod))
missClass(testSA$chd, predict(mod, newdata = testSA))



# Question 5
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)

vowel.train$y <- factor(vowel.train$y)
vowel.test$y <- factor(vowel.test$y)

set.seed(33833)

mod <- caret::train(y ~ ., data = vowel.train, method = 'rf')
mod
caret::varImp(mod)

