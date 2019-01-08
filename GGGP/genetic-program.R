library("dummies")
library("gramEvol")
library("jsonlite")
library("keras")
library("neuralnet")
library("stringr")

classification_type <- 1 # -1: Regression, 0: Single-label classification, 1: Multi-label classification
epochs <- 100
data <- NULL
gen_no <- 1
gen_pop_err <- list()
gen_evolution <- NULL
I <- NULL
j <- 6
input <- NULL
mode <- 0
O <- NULL
output <- NULL
X_train <- NULL      # 70%
y_train <- NULL
X_validation <- NULL # 20%
y_validation <- NULL
X_test <- NULL       # 10%
y_test <- NULL

# Data to save
# 1. GP population evolution.
gp_plot_data <- NULL
# 2. Accuracy of each final solution: train, validation and test.
sol_train_accuracy <- NULL
sol_validation_accuracy <- NULL
sol_test_accuracy <- NULL
# 3. Solution architecture.
sol_nn_architecture <- NULL
# 4. Name of the saved model at ./models.
sol_model_name <- NULL
# 5. Both loss and accuracy data at each epoch of the solution.
sol_plot_data <- NULL
# 6. Execution time.
exec_time <- NULL

data_cleaning <- function(url, sep) {
  data <<- read.csv(url, header=T, sep=sep)
  n <- nrow(data)
  if (str_detect(url, "regression")) {
    classification_type <<- -1
    max <- apply(data, 2, max)
    min <- apply(data, 2, min)
    scaled_data <- scale(data, center=min, scale=max-min)
    shuffled_df <- as.data.frame(scaled_data[sample(n),])
  } else {
    if (classification_type == 1) {
      aux <- dummy.data.frame(data, names=c("class"), sep="")
      rm("data")
      data <<- aux
    }
    shuffled_df <- as.data.frame(data[sample(n),])
  }
  colnames(shuffled_df) <- gsub("[^a-zA-Z]*", "", colnames(shuffled_df))
  train <- shuffled_df[1: round(0.7*n),]
  validation <- shuffled_df[(round(0.7*n)+1):round(0.9*n),]
  test <- shuffled_df[(round(0.9*n)+1):n,]
  X_train <<- train[,head(colnames(shuffled_df), -3)] %>% as.matrix()
  y_train <<- train[,tail(colnames(shuffled_df), 3)] %>% as.matrix()
  X_validation <<- validation[,head(colnames(shuffled_df), -3)] %>% as.matrix()
  y_validation <<- validation[,tail(colnames(shuffled_df), 3)] %>% as.matrix()
  X_test <<- test[,head(colnames(shuffled_df), -3)] %>% as.matrix()
  y_test <<- test[,tail(colnames(shuffled_df), 3)] %>% as.matrix()
  I <<- length(colnames(X_train))
  input <<- paste(colnames(X_train), collapse="+")
  O <<- length(colnames(y_train))
  output <<- paste(colnames(y_train), collapse="+")
}

extract_neurons <- function(word) {
  layers <- strsplit(toString(word), "/")[[1]]
  i <- 0
  hidden_l <- numeric(0) # Contains the number of hidden layers and the number of neurons of each hidden layer.
  for(j in head(layers, -1)) {
    if (i!=0) {hidden_l[i] <- nchar(j)}
    i <- i+1
  }
  return(hidden_l)
}

evaluation <- function(word) {
  hidden_layers <- extract_neurons(word)
  model <- keras_model_sequential()
  model %>% layer_dense(units = hidden_layers[1], input_shape = c(I), activation = 'relu')
  for (layer in tail(hidden_layers, 1)) {
    model %>% layer_dense(units = layer, activation = 'relu')
  }
  if (classification_type == -1) {
    model %>% layer_dense(units = O, activation = 'linear')
    model %>% compile(
      optimizer = 'adam',
      loss = 'mse',
      metrics = c('accuracy')
    )
  } else {
    model %>% layer_dense(units = O, activation = 'softmax')
    model %>% compile(
      optimizer = 'adam',
      loss = 'categorical_crossentropy',
      metrics = c('accuracy')
    )
  }
  # https://keras.rstudio.com/articles/training_callbacks.html - STOP TRAINING.
  history <- model %>% fit(X_train, y_train, epochs = epochs, verbose = 0)
  if (mode == 0) {
    score <- model %>% evaluate(X_validation, y_validation)
    gen_pop_err <<- c(gen_pop_err, score['loss'][[1]])
    return(score['loss'][[1]])
  } else {
    gp_plot_data <<- toJSON(gen_evolution)
    history_df <- as.data.frame(history)
    score <- model %>% evaluate(X_train, y_train)
    sol_train_accuracy <<- score['acc'][[1]]
    score <- model %>% evaluate(X_validation, y_validation)
    sol_validation_accuracy <<- score['acc'][[1]]
    score <- model %>% evaluate(X_test, y_test)
    sol_test_accuracy <<- score['acc'][[1]]
    model_name <- paste0('../results/models/iris_model_', j, '.h5')
    save_model_hdf5(model, model_name)
    sol_plot_data <<- toJSON(history_df)
    sol_model_name <<- model_name
    return(score['loss'][[1]])
  }
}

monitor <- function(results){
  pop_train <- Reduce("+", gen_pop_err) / length(gen_pop_err)
  best_train <- evaluation(results$best$expressions[[1]])
  gen_evolution <<- c(gen_evolution, toJSON(data.frame(gen_no, pop_train, best_train)))
  cat("--------------------\n")
  print(results)
  gen_pop_err <<- list()
  gen_no <<- gen_no + 1
}

grammar <- list(
  S = gsrule("<a><h>/<z>"),
  a = grule(replicate(I, "n")),
  z = grule(replicate(O, "n")),
  h = gsrule("<h><h>", "/<n>"),
  n = gsrule("n<n>", "n")
)

grammarDef <- CreateGrammar(grammar)
data_cleaning("../datasets/classification/iris.csv", ",")
results <- data.frame(gp_plot_data = character(),
                      sol_train_accuracy = double(),
                      sol_validation_accuracy = double(),
                      sol_test_accuracy = double(),
                      sol_nn_architecture = character(),
                      sol_model_name = character(),
                      sol_plot_data = character(),
                      exec_time = double(),
                      stringsAsFactors = FALSE)
for (i in c(1:5)) {
  mode <- 0
  gen_evolution <- list()
  start_time <- Sys.time()
  optimal_word <- GrammaticalEvolution(grammarDef, evaluation, mutationChance = 0.05, monitorFunc = monitor)
  end_time <- Sys.time()
  mode <- 1
  hidden_layers_optimal_word <- extract_neurons(optimal_word)
  sol_nn_architecture <- paste(I, paste0(hidden_layers_optimal_word, collapse = ":"), O, sep = ":")
  evaluation(optimal_word)
  exec_time <- as.double(toString(end_time - start_time))
  gen_no <- 1
  j <- j + 1
  results[nrow(results) + 1,] <- c(gp_plot_data, sol_train_accuracy, sol_validation_accuracy, sol_test_accuracy, sol_nn_architecture, sol_model_name,
                                   sol_plot_data, exec_time)
  k_clear_session()
}
#write.csv(results, file = "../results/iris.csv") # Empty CSV.
write.table(results, "../results/iris.csv", sep = ",", col.names = F, append = T)