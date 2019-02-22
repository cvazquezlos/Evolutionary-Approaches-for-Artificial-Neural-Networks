setwd("D:/Usuarios/cvazquezlos/GitHub/Genetic-programming-for-Artificial-Neural-Networks/results")

TARGET_FOLDER <- "./classification/iris"
executions <- list.files(TARGET_FOLDER)

results <- data.frame(execution = integer(),
                      architecture = character(),
                      acc_train = double(),
                      acc_validation = double(),
                      acc_test = double(),
                      time = double(),
                      saved_model = character(),
                      stringsAsFactors = FALSE)
x <- lapply(executions, function (x) {
  df = readRDS(paste0(TARGET_FOLDER, "/", x, "/execution_results.rds"))
  results <<- rbind(results, df)
})
results <- results[order(results$execution, decreasing = FALSE),]
row.names(results) <- c(1:nrow(results))

rm("x")