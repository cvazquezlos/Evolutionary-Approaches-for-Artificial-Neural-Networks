library("xlsx")
library("ggplot2")
library("sets")
library("keras")
library("stringr")
library("gtools")

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
  }, error = function(cond) {
    bad_executions <<- c(bad_executions, x)
  }, warning = function(cond) {
    bad_executions <<- c(bad_executions, x)
  })
})
rm("y")

bad_executions <- unlist(bad_executions)
from <- 1
for (bad_execution in bad_executions) {
  iteration_results <- data.frame(iteration = integer(),
                                  avg_loss = numeric(),
                                  best_loss = numeric(),
                                  stringsAsFactors = F)
  individuals <- list.files(paste0("./classification/car/total/", bad_execution, "/history/"))
  individuals <- individuals[order(as.numeric(gsub("\\D+|\\.*", "", individuals)))]
  individual_accuracies <- unlist(lapply(individuals, function(x) {
    aux <- readRDS(paste0("./classification/car/total/", bad_execution, "/history/", x))
    aux <- aux[aux$metric == "loss" & aux$data == "training",]
    aux[-1,]$value
  }))
  to <- 20
  n <- (length(individuals) - 20) / 6
  for (iteration in c(1:n)) {
    iteration_values = head(individual_accuracies, to)
    avg <- mean(iteration_values)
    best <- min(iteration_values)
    iteration_results <- rbind(iteration_results, data.frame(iteration = iteration,
                                                             avg_loss = avg,
                                                             best_loss = best,
                                                             stringsAsFactors = F))
    to <- to + 6
  }
  saveRDS(iteration_results, paste0("./classification/car/total/", bad_execution, "/", bad_execution, "_results.rds"))
}
