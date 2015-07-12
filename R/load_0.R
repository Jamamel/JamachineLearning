#' Load training and test datasets
#'
#' This function loads both csv's corresponding to the training and test dataset.
#'
#' @param training path to training csv.
#' @param testing path to test csv.
#'
#' @details Each of the two datasets are loaded from csv's as data.tables. Column "user_name" is defined as key in each. Variable "cvtd_timestamp" is converted to class POSIXct. All relevant variables are classed as numericals, interpreting emtpy cells "" and "#DIV/0!" as NA.
#'
#' @return list with 2 data.tables 'training' and 'testing'
#' \item{training}{training data.table}
#' \item{testing}{testing data.table}
#'
#' @examples
#' # load with folder csv's in working directory containing both files.
#' # dlist <- loadJML('./csvs/pml-testing.csv', './csvs/pml-testing.csv')
#'
#' @import data.table
#' @import magrittr
#'
#' @export

loadJML <- function(training, testing) {

  training <- data.table::fread(training, na.strings = c('NA', '', '#DIV/0!'))
  testing <- data.table::fread(testing, na.strings = c('NA', '', '#DIV/0!'))

  cleanDT <- function(DT){

    setkey(DT, user_name)
    DT[, 'cvtd_timestamp' := lubridate::dmy_hm(cvtd_timestamp), with = F]
    vars <- DT %>%
      sapply(class) %>%
      unlist() %>%
      .[. == 'character'] %>%
      names() %>%
      .[!. %in% c('V1', 'classe', 'new_window', 'user_name')]

    for(i in vars) data.table::set(DT, j = i, value = as.numeric(DT[[i]]))

    return(DT)

  }

  dlist <- list(training = cleanDT(training), testing = cleanDT(testing))

}



