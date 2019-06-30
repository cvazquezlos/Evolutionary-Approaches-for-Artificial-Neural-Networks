library("xlsx")
library("ggplot2")
library("sets")
library("keras")
library("stringr")
#install.packages("xlsx")

setwd("D:/Usuarios/cvazquezlos/GitHub/Genetic-programming-for-Artificial-Neural-Networks/results")
# setwd("~/GitHub/Evolutionary-Approaches-for-Artificial-Neural-Networks/results")

TARGET_FOLDER <- "./classification/ocean_proximity/partial/"

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
    df <- head(df[order(df$metric, decreasing = TRUE),], 1)
    model <- load_model_hdf5(paste0(TARGET_FOLDER, "/", x, "/model/", df$saved_model, ".h5"))
    partial_acc_train <- (model %>% evaluate(X_train, y_train))['acc'][[1]]
    partial_acc_validation <- (model %>% evaluate(X_validation, y_validation))['acc'][[1]]
    partial_acc_test <- (model %>% evaluate(X_test, y_test))['acc'][[1]]
    model %>% fit(rbind(X_train, X_validation), rbind(y_train, y_validation), validation_split = 0.235294, epochs = 450, verbose = 0,
                  callbacks = list(
                    callback_early_stopping(monitor = "val_loss", patience = 50, verbose = 0, mode ="auto")
                  ))
    total_acc_train <- (model %>% evaluate(X_train, y_train))['acc'][[1]]
    total_acc_validation <- (model %>% evaluate(X_validation, y_validation))['acc'][[1]]
    total_acc_test <- (model %>% evaluate(X_test, y_test))['acc'][[1]]
    saveRDS(data.frame(architecture = df$architecture,
                       partial_acc_train = partial_acc_train,
                       partial_acc_validation = partial_acc_validation,
                       partial_acc_test = partial_acc_test,
                       saved_model = df$saved_model,
                       total_acc_train = total_acc_train,
                       total_acc_validation = total_acc_validation,
                       total_acc_test = total_acc_test), file = paste0(TARGET_FOLDER, "/", x, "/execution_results.rds"))
    executions_results <<- rbind(executions_results, df)
  }, error = function(cond) {
    bad_executions <<- c(bad_executions, x)
  }, warning = function(cond) {
    bad_executions <<- c(bad_executions, x)
  })
})

##################################################################################
plot_df = data.frame("Entrenamiento total" = executions_results$acc_train,
                     "Entrenamiento parcial hasta el final" = executions_results_part$total_acc_train,
                     stringsAsFactors = FALSE)
boxplot(plot_df$Entrenamiento.total)
boxplot(executions_results_part$total_acc_validation)

ggplot(plot_df) +
  geom_boxplot(aes(x = 1, y = Entrenamiento.total)) +
  geom_boxplot(aes(x = 2, y = Entrenamiento.parcial.hasta.el.final)) +
  xlab("\nModo de entrenamiento") +
  scale_x_continuous(breaks = c(1, 2), labels = c("Total", "Parcial (hasta el final)")) +
  ylab("Precisión\n") +
  scale_y_continuous(breaks = c(0.60, 0.65, 0.70, 0.75, 0.80, 0.85, 0.90, 0.95, 1.00)) +
  ggtitle("Comparativa de las ejecuciones para\nlos datos de entrenamiento") + 
  theme_classic() + 
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = "#E8E8E8"),
    text = element_text(size = 25, family = "Cambria"),
    plot.title = element_text(family = "Cambria", size = 30, hjust = 0.5, margin = margin(0, 0, 25, 0)))

aux = data.frame(acc_train = )

ggplot(executions_results) +
  geom_histogram(aes(x = acc_test)) + 
  xlab("") +
  ylab("") +
  scale_x_continuous(breaks=c(0.90, 0.92, 0.94, 0.96, 0.98, 1.00, 1.20)) +
  ggtitle("Entrenamiento total - pruebas") + 
  theme_classic() + 
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = "#E8E8E8"),
    text = element_text(size = 35, family = "Cambria"),
    plot.title = element_text(family = "Cambria", size = 35, hjust = 0.5, margin = margin(0, 0, 25, 0)))



plot(density(executions_results$acc_train))
plot(density(executions_results_part$partial_acc_train))

plot(density(executions_results$acc_validation))
plot(density(executions_results_part$partial_acc_validation))

plot(density(executions_results$acc_test))
plot(density(executions_results_part$partial_acc_test))
