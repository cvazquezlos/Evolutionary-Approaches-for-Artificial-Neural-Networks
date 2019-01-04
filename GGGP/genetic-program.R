library("dummies")
library("gramEvol")
library("jsonlite")
library("keras")
library("neuralnet")
library("stringr")

epochs <- 100
data <- NULL
I <- NULL
j <- 1
input <- NULL
mode <- 0
O <- NULL
output <- NULL
train <- NULL
X_train <- NULL      # 70%
y_train <- NULL
X_validation <- NULL # 20%
y_validation <- NULL
X_test <- NULL       # 10%
y_test <- NULL

classification_type <- 1 # -1: Regression
                         # 0: Single-label classification
                         # 1: Multi-label classification

# Data to save
# 1. Accuracy of each final solution: train, validation and test.
sol_train_accuracy <- list()
sol_validation_accuracy <- list()
sol_test_accuracy <- list()
# 2. Average accuracy of the whole individuals of the GP.
avg_train_accuracy <- list()
avg_validation_accuracy <- list()
avg_test_accuracy <- list()
# 4. Solution architecture.
sol_nn_architecture <- list()
# 5. Name of the saved model at ./models.
sol_model_name <- list()
# 6. Both loss and accuracy data at each epoch of the solution.
sol_plot_data <- list()
# 7. Execution time.
exec_time <- list()

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
  train <<- shuffled_df[1: round(0.7*n),]
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
history_df <- NULL
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
  #toJSON(history_df) # Is the JSON that is needed.
  if (mode == 0) {
    score <- model %>% evaluate(X_validation, y_validation)
    return(score['loss'][[1]])
  } else {
    score <- model %>% evaluate(X_test, y_test)
    save_model_hdf5(model, paste0('iris_model_', j, '.h5'))
    sol_plot_data <- append(sol_plot_data, toJSON(history_df))
    return(score['loss'][[1]])
  }
}

monitor <- function(results){
  cat("--------------------\n")
  print(results)
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
for (i in c(1:100)) {
  start_time <- Sys.time()
  optimal_word <- GrammaticalEvolution(grammarDef, evaluation, 1, mutationChance=0.05, monitorFunc = monitor)
  end_time <- Sys.time()
  hidden_layers_optimal_word <- extract_neurons(optimal_word)
  sol_nn_architecture <- append(sol_nn_architecture, hidden_layers_optimal_word)
  exec_time <- append(exec_time, end_time - start_time)
}
optimal_word <- GrammaticalEvolution(grammarDef, evaluation, 1, mutationChance=0.05, monitorFunc = monitor)
hidden_layers_optimal_word <- extract_neurons(optimal_word)
mode <- 1
print(evaluation(optimal_word))
