# JamachineLearning Project Writeup
Jamamel  
22 July 2015  

# Introduction

This document is the final submission that describes construction of a machine learning algorithm to predict the manner of excercise being performed by several project participants.

## Background

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website [here](http://groupware.les.inf.puc-rio.br/har]) (see the section on the Weight Lifting Exercise Dataset). 

## Data 

The training data for this project are available [here](https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv).

The test data are available [here](https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv).

The data for this project come from this [source](http://groupware.les.inf.puc-rio.br/har).

The aim of the project is to predict the manner in which they did the exercise using a machine learning algorithm to predit it (variable *classe*).


# Data Loading and Pre-Processing

Data was doawnloaded to R working directory and loaded ([loadJML code](https://github.com/Jamamel/JamachineLearning/blob/master/R/load_0.R)).


```r
library(magrittr)
library(data.table)
dlist <- JamachineLearning::loadJML('./csvs/pml-training.csv', './csvs/pml-testing.csv')
```

Remove first 7 variables (id, window, timestamp) given they are considered redundant in the analysis. Transform *classe* to factor.


```r
d1 <- data.table::copy(dlist[[1]])
d1[, c(1:7) := NULL, with = F]
for(i in c('classe')) data.table::set(d1, j = i, value = factor(d1[[i]]))
```

Create data partition (60% training, 40% testing) of original training dataset.Remove variables with (up to nearly) no variance and any variables that have at least 97% of their values missing.


```r
# partition dataset
trainpart <- caret::createDataPartition(d1$classe, p = 0.6, list = FALSE)[,1]

training <- d1[trainpart, ]
testing <- d1[-trainpart, ]

# check for near zero variance
varcheck <- caret::nearZeroVar(training, saveMetrics = TRUE)
outvars <- rownames(varcheck)[varcheck$nzv]
if(isTRUE(length(outvars) > 0)) training[, outvars := NULL, with = F]


# check for proportion of missing values
nacount <- as.matrix(training[, lapply(.SD, function(x) sum(is.na(x)))])
table(nacount)
nacutoff <- min(nacount[nacount != 0])

if(isTRUE(nacutoff/nrow(training) > 0.97)) {

  outvars <- which(nacount >= nacutoff)
  training[, outvars := NULL, with = F]

}

# apply common class to all numeric variables remaining
vars <- unlist(sapply(training, function(x) class(x)[1]))
numvars <- names(vars[vars %in% c('numeric', 'integer')])
for(i in numvars) data.table::set(training, j = i, value = as.numeric(training[[i]]))
```


# Modelling and Cross-Validation

A random forest algorithm is applied to identify the best prediction model using all *randomForest* default settings.


```r
set.seed(1000)
moddata <- data.table::copy(training)
mod <- randomForest::randomForest(classe ~ ., data = moddata)
```

Generate confusino matrix to measure accuracies.


```r
confmat <- caret::confusionMatrix(predict(mod, testing, type = 'class'), testing$classe)
confmat
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 2231    9    0    0    0
##          B    1 1501    7    0    0
##          C    0    7 1358   14    3
##          D    0    1    3 1270    4
##          E    0    0    0    2 1435
## 
## Overall Statistics
##                                           
##                Accuracy : 0.9935          
##                  95% CI : (0.9915, 0.9952)
##     No Information Rate : 0.2845          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.9918          
##  Mcnemar's Test P-Value : NA              
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.9996   0.9888   0.9927   0.9876   0.9951
## Specificity            0.9984   0.9987   0.9963   0.9988   0.9997
## Pos Pred Value         0.9960   0.9947   0.9826   0.9937   0.9986
## Neg Pred Value         0.9998   0.9973   0.9985   0.9976   0.9989
## Prevalence             0.2845   0.1935   0.1744   0.1639   0.1838
## Detection Rate         0.2843   0.1913   0.1731   0.1619   0.1829
## Detection Prevalence   0.2855   0.1923   0.1761   0.1629   0.1832
## Balanced Accuracy      0.9990   0.9938   0.9945   0.9932   0.9974
```

The confusion matrix confirms a highly accurate model (*Accuracy* = 99.3%) with acceptably low out-of-bag error (*OOB* = 0.65%).


# Validation and Submission

The model was then validated against the project's training set.


```r
validating <- data.table::copy(dlist[[2]])
validating[, 'V1' := as.numeric(V1)]
data.table::setkey(validating, V1)
for(i in numvars) data.table::set(validating, j = i, value = as.numeric(validating[[i]]))
```

The predicted values were then submitted for the project's submission.


```r
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(predict(mod, newdata = validating[,colnames(moddata)[-53], with = F], type = 'class'))
```


# Appendix

This is t he R setup required to run all code displayed.


```r
sessionInfo()
```

```
## R version 3.2.1 (2015-06-18)
## Platform: x86_64-w64-mingw32/x64 (64-bit)
## Running under: Windows 7 x64 (build 7601) Service Pack 1
## 
## locale:
## [1] LC_COLLATE=English_United Kingdom.1252 
## [2] LC_CTYPE=English_United Kingdom.1252   
## [3] LC_MONETARY=English_United Kingdom.1252
## [4] LC_NUMERIC=C                           
## [5] LC_TIME=English_United Kingdom.1252    
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] data.table_1.9.4 magrittr_1.5    
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_0.11.6           formatR_1.2           nloptr_1.0.4         
##  [4] plyr_1.8.3            class_7.3-13          iterators_1.0.7      
##  [7] tools_3.2.1           digest_0.6.8          lme4_1.1-8           
## [10] lubridate_1.3.3       evaluate_0.7          memoise_0.2.1        
## [13] gtable_0.1.2          nlme_3.1-121          lattice_0.20-31      
## [16] mgcv_1.8-6            Matrix_1.2-2          foreach_1.4.2        
## [19] parallel_3.2.1        yaml_2.1.13           SparseM_1.6          
## [22] brglm_0.5-9           proto_0.3-10          e1071_1.6-6          
## [25] BradleyTerry2_1.0-6   stringr_1.0.0         knitr_1.10.5         
## [28] gtools_3.5.0          stats4_3.2.1          nnet_7.3-9           
## [31] grid_3.2.1            caret_6.0-52          rmarkdown_0.7        
## [34] minqa_1.2.4           car_2.0-25            ggplot2_1.0.1        
## [37] reshape2_1.4.1        scales_0.2.5          codetools_0.2-14     
## [40] htmltools_0.2.6       MASS_7.3-40           splines_3.2.1        
## [43] pbkrtest_0.4-2        randomForest_4.6-10   colorspace_1.2-6     
## [46] quantreg_5.11         JamachineLearning_0.1 stringi_0.5-5        
## [49] munsell_0.4.2         chron_2.3-47
```

