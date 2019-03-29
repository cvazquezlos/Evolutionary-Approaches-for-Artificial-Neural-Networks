library("xlsx")
library("ggplot2")
library("sets")
library("keras")
library("stringr")
#install.packages("xlsx")

# setwd("D:/Usuarios/cvazquezlos/GitHub/Genetic-programming-for-Artificial-Neural-Networks/results")
setwd("~/GitHub/Evolutionary-Approaches-for-Artificial-Neural-Networks/results")

TARGET_FOLDER <- "./classification/car/total/"

# Analysis of the resulting dataframes for each execution
BASE_DATA_FRAME <- data.frame(execution = integer(),
                              architecture = character(),
                              partial_acc_train = double(),
                              partial_acc_validation = double(),
                              partial_acc_test = double(),
                              time = double(),
                              saved_model = character(),
                              total_acc_train = double(),
                              total_acc_validation = double(),
                              total_acc_test = double(),
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
# Repairing task of the executions.
bad_executions <- unlist(bad_executions)
for (bad_execution in bad_executions) {
  individual_histories <- list.files(paste0(TARGET_FOLDER, "/", bad_execution, "/history"))
  individual_ranking <- data.frame(individual = character(), 
                                   acc_train = numeric(), 
                                   acc_validation = numeric(),
                                   numeric(), stringsAsFactors = FALSE)
  for (history in individual_histories) {
    aux <- readRDS(paste0(TARGET_FOLDER, "/", bad_execution, "/history/", history))
    ranking_tr <- aux[aux$metric == "acc" & aux$data == "training",]
    ranking_tr<- ranking_tr[order(ranking_tr$value, decreasing = TRUE),]
    ranking_val <- aux[aux$metric == "acc" & aux$data == "validation",]
    ranking_val<- ranking_val[order(ranking_val$value, decreasing = TRUE),]
    individual_ranking <- rbind(individual_ranking, data.frame(individual = history, acc_train = ranking_tr[1,]$value,
                                                               acc_validation = ranking_val[1,]$value))
  }
  individual_ranking <- individual_ranking[order(individual_ranking$acc_train, decreasing = TRUE),]
  saveRDS(individual_ranking[1,], paste0(TARGET_FOLDER, "/", bad_execution, "/", "execution_results.rds"))
}

for (bad_execution in bad_executions) {
  individual <- readRDS(paste0(TARGET_FOLDER, "/", bad_execution, "/", "execution_results.rds"))
  colnames(individual) <- c("architecture", "acc_train", "acc_validation")
  individual$saved_model <- individual$architecture
  ind_name <- str_replace_all(individual$architecture, ".rds", "")
  model <- load_model_hdf5(paste0(TARGET_FOLDER, "/", bad_execution, "/model/", ind_name, ".h5"))
  individual$acc_test <- (model %>% evaluate(X_test, y_test))['acc'][[1]]
  individual$architecture <- gsub("-.*", "", individual$architecture)
  saveRDS(individual, paste0(TARGET_FOLDER, "/", bad_execution, "/", "execution_results1.rds"))
}

# for (bad_execution in bad_executions) {
#   individual <- readRDS(paste0(TARGET_FOLDER, "/", bad_execution, "/", "execution_results1.rds"))
#   individual$saved_model <- individual$architecture
#   individual$architecture <- str_replace_all(individual$architecture, ".rds", "")
#   saveRDS(individual, paste0(TARGET_FOLDER, "/", bad_execution, "/", "execution_results2.rds"))
# }

executions_results <- executions_results[order(executions_results$execution, decreasing = FALSE),]
row.names(executions_results) <- c(1:nrow(executions_results))
executions_results$architecture <- as.character(executions_results$architecture)

analysis_results <- aggregate(executions_results[,c(2:10)], 
                              by = list(executions_results$architecture), 
                              FUN = mean)
analysis_results <- analysis_results[order(analysis_results$time),c(1,3:6,8:10)]
colnames(analysis_results) <- c("architecture", "partial_acc_train", "partial_acc_validation", "partial_acc_test", "time", "total_acc_train", "total_acc_validation", "total_acc_test")
analysis_results$percentage <- unlist(lapply(analysis_results$architecture, function(x) {
  round((nrow(executions_results[executions_results$architecture == x,])/nrow(executions_results) * 100), 2)
}))
analysis_results <- analysis_results[order(analysis_results$percentage, decreasing = TRUE),]
row.names(analysis_results) <- c(1:nrow(analysis_results))
write.csv(analysis_results, file = "../results/classification/iris/executions_results_partial.csv")
write.xlsx(analysis_results, file = "../results/classification/iris/executions_results_partial.xlsx")

# Plotting the executions
executions_plotting_data <- data.frame(generation = c(1:29), stringsAsFactors = F)
z <- lapply(c(1:80), function(x) {
  df = readRDS(paste0("../results/classification/iris/partial/", x, "/", x, "_results.rds"))[, c(2:3)]
  n_colnames = c(paste0("avg_loss", x), paste0("best_loss", x))
  colnames(df) = n_colnames
  n = nrow(df)
  if (n != 29) {
    remaining_rows = 29 - n
    aux = data.frame(col1 = rep(NA, remaining_rows), col2 = rep(NA, remaining_rows), stringsAsFactors = F)
    colnames(aux) = n_colnames
    df = rbind(df, aux)
  }
  df[,paste0("avg_loss", x)] = as.double(df[,paste0("avg_loss", x)])
  df[,paste0("best_loss", x)] = as.double(df[,paste0("best_loss", x)])
  executions_plotting_data <<- cbind(executions_plotting_data, df)
})

avg_selected_cols <- colnames(executions_plotting_data)[c(FALSE, TRUE)]
best_selected_cols <- tail(colnames(executions_plotting_data)[c(TRUE, FALSE)], -1)

avg_loss_mean <- rowMeans(executions_plotting_data[,avg_selected_cols], na.rm = TRUE)
best_loss_mean <- rowMeans(executions_plotting_data[,best_selected_cols], na.rm = TRUE)
executions_plotting_data <- cbind(executions_plotting_data, avg_loss_mean)
executions_plotting_data <- cbind(executions_plotting_data, best_loss_mean)

executions_plot <- ggplot(data = executions_plotting_data, aes(x = generation)) +
  geom_point(aes(y = avg_loss1), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss2), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss3), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss4), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss5), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss6), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss7), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss8), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss9), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss10), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss11), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss12), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss13), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss14), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss15), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss16), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss17), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss18), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss19), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss20), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss21), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss22), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss23), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss24), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss25), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss26), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss27), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss28), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss29), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss30), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss31), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss32), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss33), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss34), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss35), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss36), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss37), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss38), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss39), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss40), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss41), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss42), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss43), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss44), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss45), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss46), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss47), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss48), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss49), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss50), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss51), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss52), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss53), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss54), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss55), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss56), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss57), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss58), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss59), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss60), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss61), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss62), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss63), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss64), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss65), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss66), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss67), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss68), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss69), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss70), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss71), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss72), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss73), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss74), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss75), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss76), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss77), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss78), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss79), shape = 20, colour = "gray") +
  geom_point(aes(y = avg_loss80), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss1), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss2), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss3), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss4), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss5), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss6), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss7), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss8), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss9), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss10), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss11), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss12), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss13), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss14), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss15), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss16), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss17), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss18), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss19), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss20), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss21), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss22), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss23), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss24), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss25), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss26), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss27), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss28), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss29), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss30), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss31), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss32), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss33), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss34), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss35), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss36), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss37), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss38), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss39), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss40), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss41), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss42), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss43), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss44), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss45), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss46), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss47), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss48), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss49), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss50), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss51), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss52), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss53), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss54), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss55), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss56), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss57), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss58), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss59), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss60), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss61), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss62), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss63), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss64), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss65), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss66), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss67), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss68), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss69), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss70), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss71), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss72), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss73), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss74), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss75), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss76), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss77), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss78), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss79), shape = 20, colour = "gray") +
  geom_point(aes(y = best_loss80), shape = 20, colour = "gray") +
  geom_line(aes(y = avg_loss_mean, colour = "Media"), size = 1) +
  geom_line(aes(y = best_loss_mean, colour = "Mejores"), size = 1) +
  xlab("Generaciones") +
  scale_x_continuous(breaks = c(1:29)) +
  ylab("Fitness") + 
  scale_y_continuous(breaks = c(0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0,1.1,1.2)) +
  scale_colour_manual("Individuos", values = c("Media" = "red", "Mejores" = "blue")) +
  ggtitle("Evoluci?n media de las poblaciones y sus mejores a lo largo de las 80 ejecuciones del problema Iris") + theme_classic() + theme(plot.title = element_text(hjust = 0.5))
print(executions_plot)

# BAD EXECUTIONS
# "10" "11" "13" "15" "16" "17" "18" "19" "2"  "20" "21" "22" "25" "26" "28" "3"  "31" "32" "33" "34"
# "35" "36" "37" "39" "40" "41" "43" "44" "48" "49" "50" "51" "54" "55" "56" "57" "58" "60" "64" "66"
# "7"  "71" "73" "75" "76" "77" "78" "79" "9"