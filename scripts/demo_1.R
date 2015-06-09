library(ISLR)
library(ggplot2)
library(caret)

data(Wage)
summary(Wage)

inTrain <- createDataPartition(y = Wage$wage,
                               p = 0.70,
                               list = FALSE)

training <- Wage[inTrain,]
testing <- Wage[-inTrain,]

dim(training)
dim(testing)

featurePlot(x = training[c('age', 'education', 'jobclass')],
            y = training$wage,
            plot = 'pairs')

ggplot(data = training, aes(x = age, y = wage, colour = education)) + 
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x)

cutWage <- Hmisc::cut2(training$wage, g = 3)
table(cutWage)

p1 <- ggplot(data = training, aes(x = cutWage, y = age, fill = cutWage)) +
  geom_boxplot()
p1


p2 <- ggplot(data = training, aes(x = cutWage, y = age, fill = cutWage)) +
  geom_boxplot() +
  geom_point(position = 'jitter')
p2

gridExtra::grid.arrange(p1, p2, ncol = 2)



t1 <- table(cutWage, training$jobclass)
t1


prop.table(t1,1)


ggplot(data = training, aes(x = wage, colour = education)) + 
  geom_density()
