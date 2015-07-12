dlist <- loadJML('./csvs/pml-training.csv', './csvs/pml-training.csv')
d1 <- dlist[[1]]

varcheck <- caret::nearZeroVar(d1, saveMetrics = TRUE)
outvars <- rownames(varcheck)[varcheck$zeroVar]
if(isTRUE(length(outvars) > 0)) d1[, outvars := NULL, with= F]

vars <- unlist(sapply(d1, function(x) class(x)[1]))
numvars <- names(vars[vars %in% c('numeric', 'integer')])

cormat <- cor(d1[, numvars, with = F], use = 'pairwise.complete.obs')
diag(cormat) <- 0

outvars <- which(cormat == 1, arr.ind = T)
if(isTRUE(nrow(outvars) > 0)) {

  outvars <- rownames(outvars[seq(1 , nrow(outvars), by = 2),])
  d1[, outvars := NULL, with= F]
  vars <- unlist(sapply(d1, function(x) class(x)[1]))
  numvars <- names(vars[vars %in% c('numeric', 'integer')])

}

GGally::ggpairs(data = d1, # data.frame with variables
        columns = 20:30, # columns to plot, default to all.
        title = "tips data"#, # title of the plot
        #colour = "sex"
        )

caret::featurePlot(d1[, numvars], d1$classe, plot = 'pairs')

