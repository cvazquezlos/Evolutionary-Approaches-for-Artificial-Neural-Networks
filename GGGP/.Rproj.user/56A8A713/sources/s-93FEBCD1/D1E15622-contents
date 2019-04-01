library("xlsx")
library("ggplot2")
library("sets")
library("keras")
library("stringr")

# setwd("D:/Usuarios/cvazquezlos/GitHub/Genetic-programming-for-Artificial-Neural-Networks/results")
setwd("~/GitHub/Evolutionary-Approaches-for-Artificial-Neural-Networks/results")

TARGET_FOLDER <- "./classification/car/total"

BASE_DATA_FRAME <- data.frame(architecture = character(),
                              saved_model = character(),
                              acc_train = double(),
                              acc_validation = double(),
                              acc_test = double(),
                              stringsAsFactors = FALSE)
executions <- list.files(TARGET_FOLDER)

bad_executions <- list()
executions_results <- BASE_DATA_FRAME
y <- lapply(executions, function (x) {
  tryCatch({
    df = readRDS(paste0(TARGET_FOLDER, "/", x, "/final_population.rds"))
    executions_results <<- rbind(executions_results, df)
  }, error = function(cond) {
    bad_executions <<- c(bad_executions, x)
  }, warning = function(cond) {
    bad_executions <<- c(bad_executions, x)
  })
})
rm("y")

bad_executions <- unlist(bad_executions)
last_index = 20
for (bad_execution in bad_executions) {
  
}