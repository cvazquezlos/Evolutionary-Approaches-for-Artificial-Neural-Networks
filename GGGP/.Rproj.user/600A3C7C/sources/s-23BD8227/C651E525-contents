library("xlsx")
library("ggplot2")
library("sets")
library("keras")
library("stringr")
library("gtools")

# setwd("D:/Usuarios/cvazquezlos/GitHub/Genetic-programming-for-Artificial-Neural-Networks/results")
setwd("~/GitHub/Evolutionary-Approaches-for-Artificial-Neural-Networks/results")

TARGET_FOLDER <- "./classification/car/partial"

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

# Repair n_results.rds
for (bad_execution in bad_executions) {
  iteration_results <- data.frame(iteration = integer(),
                                  avg_loss = numeric(),
                                  best_loss = numeric(),
                                  stringsAsFactors = F)
  individuals <- list.files(paste0(TARGET_FOLDER, "/", bad_execution, "/history/"))
  individuals <- individuals[order(as.numeric(gsub("\\D+|\\.*", "", individuals)))]
  individual_accuracies <- unlist(lapply(individuals, function(x) {
    aux <- readRDS(paste0(TARGET_FOLDER, "/", bad_execution, "/history/", x))
    aux <- aux[aux$metric == "loss" & aux$data == "training",]
    tail(aux, 1)$value
  }))
  to <- 20
  n <- (length(individuals) - 20) / 6
  for (iteration in c(1:n)) {
    iteration_values = head(individual_accuracies, to)
    iteration_values <- sort(iteration_values)
    avg <- mean(head(iteration_values, 20))
    best <- min(head(iteration_values, 20))
    iteration_results <- rbind(iteration_results, data.frame(iteration = iteration,
                                                             avg_loss = avg,
                                                             best_loss = best,
                                                             stringsAsFactors = F))
    to <- to + 6
  }
  saveRDS(iteration_results, paste0(TARGET_FOLDER, "/", bad_execution, "/", bad_execution, "_results.rds"))
}

# Repair execution_results.rds
for (bad_execution in bad_executions) {
  individuals <- list.files(paste0(TARGET_FOLDER, "/", bad_execution, "/history/"))
  individual_ranking <- data.frame(architecture = character(), 
                                   acc_train = numeric(), 
                                   acc_validation = numeric(),
                                   saved_model = character(), stringsAsFactors = FALSE)
  for (individual in individuals) {
    aux <- readRDS(paste0(TARGET_FOLDER, "/", bad_execution, "/history/", individual))
    ranking_tr <- aux[aux$metric == "acc" & aux$data == "training",]
    ranking_tr<- ranking_tr[order(ranking_tr$value, decreasing = TRUE),]
    ranking_val <- aux[aux$metric == "acc" & aux$data == "validation",]
    ranking_val<- ranking_val[order(ranking_val$value, decreasing = TRUE),]
    model_name <- str_replace_all(individual, ".rds", "")
    individual_ranking <- rbind(individual_ranking, data.frame(architecture = gsub("-.*", "", model_name), 
                                                               acc_train = ranking_tr[1,]$value,
                                                               acc_validation = ranking_val[1,]$value,
                                                               saved_model = individual))
  }  
  individual_ranking <- individual_ranking[order(individual_ranking$acc_train, decreasing = TRUE),]
  best <- individual_ranking[1,]
  model_name <- str_replace_all(best$saved_model, ".rds", "")
  model <- load_model_hdf5(paste0(TARGET_FOLDER, "/", bad_execution, "/model/", model_name, ".h5"))
  acc_test <- (model %>% evaluate(X_test, y_test))['acc'][[1]]
  best$acc_test <- acc_test
  colnames(best) <- c("architecture", "acc_train", "acc_validation", "saved_model", "acc_test")
  saveRDS(best, paste0(TARGET_FOLDER, "/", bad_execution, "/", "execution_results.rds"))
}
