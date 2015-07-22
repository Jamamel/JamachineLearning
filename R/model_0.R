library(magrittr)
library(data.table)

dlist <- JamachineLearning::loadJML('./csvs/pml-training.csv', './csvs/pml-testing.csv')
d1 <- data.table::copy(dlist[[1]])
d1[, c(1:7) := NULL, with = F]
for(i in c('classe')) data.table::set(d1, j = i, value = factor(d1[[i]]))

trainpart <- caret::createDataPartition(d1$classe, p = 0.6, list = FALSE)[,1]

training <- d1[trainpart, ]
testing <- d1[-trainpart, ]

varcheck <- caret::nearZeroVar(training, saveMetrics = TRUE)
outvars <- rownames(varcheck)[varcheck$nzv]
if(isTRUE(length(outvars) > 0)) training[, outvars := NULL, with = F]


nacount <- as.matrix(training[, lapply(.SD, function(x) sum(is.na(x)))])
table(nacount)
nacutoff <- min(nacount[nacount != 0])


if(isTRUE(nacutoff/nrow(training) > 0.97)) {

  outvars <- which(nacount >= nacutoff)
  training[, outvars := NULL, with = F]

}

vars <- unlist(sapply(training, function(x) class(x)[1]))
numvars <- names(vars[vars %in% c('numeric', 'integer')])
for(i in numvars) data.table::set(training, j = i, value = as.numeric(training[[i]]))

set.seed(1000)
moddata <- data.table::copy(training)
mod <- randomForest::randomForest(classe ~ ., data = moddata)
mod

confmat <- caret::confusionMatrix(predict(mod, testing, type = 'class'), testing$classe)
scales::percent(1 - confmat$overall['Accuracy'])



validating <- data.table::copy(dlist[[2]])
validating[, 'V1' := as.numeric(V1)]
data.table::setkey(validating, V1)
for(i in numvars) data.table::set(validating, j = i, value = as.numeric(validating[[i]]))

predict(mod, newdata = validating[,colnames(moddata)[-53], with = F], type = 'class')


