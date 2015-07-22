library(magrittr)
# library(JamachineLearning)
dlist <- JamachineLearning::loadJML('./csvs/pml-training.csv', './csvs/pml-testing.csv')
d1 <- data.table::copy(dlist[[1]])
d1[, c(1,2) := NULL, with = F]

nacount <- sapply(d1, function(x) sum(is.na(x)))
prop.table(table(nacount))
nacutoff <- min(nacount[nacount != 0])
nacutoff/nrow(d1)

outvars <- which(nacount >= nacutoff)
d1[, outvars := NULL, with = F]

varcheck <- caret::nearZeroVar(d1, saveMetrics = TRUE)
outvars <- rownames(varcheck)[varcheck$zeroVar]
if(isTRUE(length(outvars) > 0)) d1[, outvars := NULL, with = F]

vars <- unlist(sapply(d1, function(x) class(x)[1]))
numvars <- names(vars[vars %in% c('numeric', 'integer')])

x <- caret::findCorrelation(d1[, numvars, with = F], names = TRUE, cutoff = 0.90, exact = FALSE)

if(isTRUE(length(x) > 0)) d1[, x := NULL, with = F]


# View(d1)


vars <- d1 %>%
  sapply(function(x) class(x)[1]) %>%
  unlist()
mvars <- names(vars)[vars %in% c('numeric', 'integer')]
idvars <- c(names(d1)[!names(d1) %in% mvars], mvars[1:3])
mvars <- mvars[-c(1:3)]

d1[, hour := lubridate::hour(cvtd_timestamp)]
d1[, day := lubridate::day(cvtd_timestamp)]


plot1d <- d1[, c('classe', mvars), with = F] %>%
  reshape2::melt(id.vars = 'classe')

plot2d <- d1[, c('classe', 'day', 'hour'), with = F]


library(ggplot2)
pd <- plot1d[grepl('arm', variable),]
ggplot(pd, aes(x = value, colour = classe)) +
  geom_density() +
  facet_wrap( ~ variable, scales = 'free')
plot(d1$var_yaw_belt)

varvars <- d1 %>%
  colnames() %>%
  .[grepl('var', .)]

pd <- plot1d[grepl('var', variable),]
ggplot(pd, aes(x = value, colour = classe)) +
  geom_density() +
  facet_wrap( ~ variable, scales = 'free')

if(isTRUE(length(varvars) > 0)) d1[, varvars := NULL, with = F]

numvars <- numvars[numvars %in% names(d1)]

# varcols0 <- caret::preProcess(d1[, numvars, with = F], method = c('center', 'scale'))
varcols1 <- caret::preProcess(d1[, numvars, with = F], method = 'knnImpute')
# varcols2 <- caret::preProcess(d1[, numvars[-c(83:84)], with = F], method = 'pca')
# trainvarcols <- data.table::data.table(predict(varcols0, newdata = d1[, numvars, with = F]))
trainvarcols <- data.table::data.table(predict(varcols1, newdata = d1[, numvars, with = F]))
# trainvarcols <- data.table::data.table(predict(varcols2, newdata = trainvarcols[, -c(83:84), with = F]))
d1[, names(trainvarcols) := trainvarcols]
plottvc <- trainvarcols[, classe := d1$classe] %>%
  reshape2::melt(id.vars = 'classe')

ggplot(plottvc, aes(x = value, colour = classe)) +
  geom_density() +
  facet_wrap( ~ variable, scales = 'free')

library(randomForest)
library(caret)
d1[, classe := factor(classe)]
moddata <- d1[, -c(1,2,11,12), with = F]

library(foreach)
rfparams <- expand.grid(mtry = 10, importance = TRUE)

mod <- caret::train(classe ~ ., data = moddata, method = 'rf', ntree = 5, importance = TRUE)
mod

warnings()
View(trainvarcols)

library(dplyr)
x <- d1[, -c(1,2,11,12), with = F] %>%
  dplyr::tbl_df() %>%
  dplyr::group_by(classe) %>%
  dplyr::summarise_each_(funs(sd), names(.))
View(x)

summary(plottvc$value)

auxvars <- as.character(unique(pd$variable))
pd.1 <- d1[, c('classe', auxvars), with = F]
library(GGally)
ggpairs(data = pd.1, # data.frame with variables
        # columns = match(mvars[1:20], names(d1)), # columns to plot, default to all.
        title = "tips data"#, # title of the plot
        # upper = list(combo = "density",
        # continuous = 'density'),
        # colour = "classe"
)



pd <- plot1d[grepl('belt', variable),]
ggplot(pd, aes(x = value, colour = classe)) +
  geom_density() +
  facet_wrap( ~ variable, scales = 'free')



pd <- plot2d
ggplot(pd, aes(x = as.factor(hour), fill = classe)) +
  geom_bar(position = 'fill')


pd <- plot3d
ggplot(pd, aes(x = as.factor(day), fill = classe)) +
  geom_bar(position = 'fill')



library(GGally)
ggpairs(data = d1, # data.frame with variables
        columns = match(mvars[1:20], names(d1)), # columns to plot, default to all.
        title = "tips data", # title of the plot
        # upper = list(combo = "density",
        # continuous = 'density'),
        colour = "classe"
)



preProc <- caret::preProcess()






d2 <- d1 %>%
  reshape2::melt(id.vars = idvars, measure.vars = mvars) %>%
  .[!is.na(value),]

vars <- d2$variable %>%
  levels()

devicecol <- c('_belt', '_arm', '_dumbbell', '_forearm')
measurecol <- c('roll', 'pitch', 'picth','yaw', 'accel', 'gyros', 'magnet')
statscol <- c('kurtosis', 'skewness', 'max', 'min', 'amplitude', 'total', 'var', 'avg', 'stddev')
aggrcol <- c('roll_belt', 'pitch_belt', 'yaw_belt')

mark <- sapply(devicecol, grepl, d2$variable, ignore.case=TRUE)
d2[, 'device' := stringr::str_replace(devicecol[which(mark, arr.ind = T)[,2]], '_', '')]

mark <- sapply(measurecol, grepl, d2$variable, ignore.case=TRUE)
d2[, 'measure' := stringr::str_replace(measurecol[which(mark, arr.ind = T)[,2]], '_', '')]
d2[measure == 'picth', measure := 'pitch']

mark <- sapply(statscol, grepl, d2$variable, ignore.case=TRUE)
mask <- which(mark, arr.ind = T)
d2[, 'stat' := NA_character_]
d2[mask[,1], stat := stringr::str_replace(statscol[mask[,2]], '_', '')]
d2[is.na(stat) & variable %in% aggrcol, stat := 'aggr']
d2[is.na(stat), stat := 'coord']
d2[stat != 'coord',]

d2[, variable := NULL]

d3 <- d2 %>%
  data.table::dcast.data.table(user_name + cvtd_timestamp + new_window + classe + raw_timestamp_part_1 + raw_timestamp_part_2 + num_window + device + measure ~ stat, fun.aggregate = mean, na.rm = T, drop = TRUE)

set.seed(1000)
inTrain <- caret::createDataPartition(d3$classe, p = 0.60)[[1]]
training <- as.data.frame(d3[ inTrain, c(4, 8, 9, 11:12), with = F])
testing <- as.data.frame(d3[-inTrain, -c(1:3, 5:7, 13,14), with = F])

dim(training)
dim(testing)

names(d3)
library(GGally)
ggpairs(data = training, # data.frame with variables
        # columns = , # columns to plot, default to all.
        title = "tips data", # title of the plot
        # upper = list(combo = "density",
                     # continuous = 'density'),
        colour = "classe"
        )

a <- plyr::dlply(training, device, function(x) caret::nearZeroVar(x, saveMetrics = TRUE))

modFit <- caret::train(classe ~ ., data = training, method = 'rf')
dim(na.omit(training[,-5]))
View(training)
