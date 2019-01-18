library("dummies")
library("gramEvol")
library("jsonlite")
library("keras")
library("neuralnet")
library("stringr")

# ----------------------------------------------------------------------------------------------------------------- #
# ---------------------------------------------- ALGORITHM VARIABLES ---------------------------------------------- #
# ----------------------------------------------------------------------------------------------------------------- #
classification_type <- 1 # -1: Regression, 0: Single-label classification, 1: Multi-label classification
data <- NULL
epochs <- 750
fitness_calculations <- data.frame(individual = character(), gen_check = integer(), acc = double(), loss = double(), 
                                   saved_model = character(), history = character(), stringsAsFactors = FALSE)
gen_no <- 1
gen_pop_err <- list()
gen_evolution <- NULL
I <- NULL
j <- 1 # Update it in each execution as: last execution + 1
k <- 0
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

# ----------------------------------------------------------------------------------------------------------------- #
# ---------------------------------------------- AUXILIARY FUNCTIONS ---------------------------------------------- #
# ----------------------------------------------------------------------------------------------------------------- #
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

extract_neurons <- function(word, mode) {
  layers <- strsplit(toString(word), "/")[[1]]
  linear_mode <- strrep("n", I)
  i <- 0
  hidden_l <- numeric(0) # Contains the number of hidden layers and the number of neurons of each hidden layer.
  for(l in head(layers, -1)) {
    if (i!=0) {
      hidden_l[i] <- nchar(l)
      linear_mode <- paste0(linear_mode, '/', l)
    }
    i <- i+1
  }
  if (mode == 0) {
    return(hidden_l)
  } else {
    return(paste0(linear_mode, strrep("n", O)))
  }
}

evaluation <- function(word) {
  hidden_layers <- extract_neurons(word, 0)
  word <- extract_neurons(word, 1)
  if (!isAssessable(word)) {
    i1 <- with(fitness_calculations, individual == hidden_layers & gen_check == (gen_no -1))
    i2 <- !duplicated(i1) & i1
    fitness_calculations$gen_check[i2] <- gen_no
    return(fitness_calculations$loss[i2])
  }
  # Cleaning up the TensorFlow graph.
  k <<- k + 1
  if(k > 5) {k_clear_session()
    k <<- 0} else k <<- k + 1
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
  history <- model %>% fit(X_train, y_train, epochs = epochs, verbose = 0, callbacks = list(
    callback_early_stopping()
  ))
  model_name <- paste0('gp_models/', j, '/', gen_no, '/', gsub("\"", "", gsub("/", "_", word)), '.h5')
  save_model_hdf5(model, model_name)
  score <- model %>% evaluate(X_validation, y_validation)
  fitness_calculations[nrow(fitness_calculations) + 1,] <<- c(word, gen_no, score['acc'][[1]], score['loss'][[1]], 
                                                             model_name, toString(toJSON(as.data.frame(history))))
  gen_pop_err <<- c(gen_pop_err, score['loss'][[1]])
  return(score['loss'][[1]])
}

isAssessable <- function(word) {
  filtered_fitness_calculations <- subset(fitness_calculations, individual == word & gen_check == (gen_no - 1))
  return(if(nrow(filtered_fitness_calculations) == 0) TRUE else FALSE)
}

monitor <- function(results){
  pop_train <- Reduce("+", gen_pop_err) / length(gen_pop_err)
  best_train <- evaluation(results$best$expressions[[1]])
  gen_evolution <<- c(gen_evolution, toString(toJSON(data.frame(gen_no, pop_train, best_train))))
  cat("--------------------\n")
  print(results)
  gen_pop_err <<- list()
  gen_no <<- gen_no + 1
  dir.create(paste0("gp_models/", j, "/", gen_no), showWarnings = FALSE)
}

# ----------------------------------------------------------------------------------------------------------------- #
# ------------------------------------------------ MAIN ALGORITHM ------------------------------------------------- #
# ----------------------------------------------------------------------------------------------------------------- #
grammar <- list(
  S = gsrule("<a><h>/<z>"),
  a = grule(strrep("n", I)),
  z = grule(strrep("n", O)),
  h = gsrule("<h><h>", "/<n>"),
  n = gsrule("n<n>", "n")
)
k_clear_session()
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
for (i in c(1:2)) {
  dir.create(paste0("gp_models/", j), showWarnings = FALSE)
  gen_evolution <- list()
  gen_no <- 1
  dir.create(paste0("gp_models/", j, "/", gen_no), showWarnings = FALSE)
  start_time <- Sys.time()
  optimal_word <- GrammaticalEvolution(grammarDef, evaluation, popSize = 5, newPerGen = 0, elitism = 3, mutationChance = 0.05, 
                                       monitorFunc = monitor, iterations = 1)
  optimal_word_layers <- extract_neurons(optimal_word, 0)
  optimal_word <- extract_neurons(optimal_word, 1)
  end_time <- Sys.time()
  gp_plot_data <- toString(toJSON(gen_evolution))
  ordered_fitness_calculations <- fitness_calculations[order(fitness_calculations$acc),]
  optimal_individual <- head(ordered_fitness_calculations[ordered_fitness_calculations$individual == optimal_word,], 1)
  model <- keras_model_sequential()
  model %>% load_model_hdf5(optimal_individual$saved_model)
  score <- model %>% evaluate(X_train, y_train)
  sol_train_accuracy <- score['acc'][[1]]
  score <- model %>% evaluate(X_validation, y_validation)
  sol_validation_accuracy <- score['acc'][[1]]
  score <- model %>% evaluate(X_test, y_test)
  sol_test_accuracy <- score['acc'][[1]]
  sol_nn_architecture <- paste(I, paste0(optimal_word_layers, collapse = ":"), O, sep = ":")
  model_name <- paste0('../results/models/iris_model_', j, '.h5')
  save_model_hdf5(model, model_name)
  sol_model_name <- model_name
  sol_plot_data <- optimal_individual$history
  exec_time <- as.double(toString(end_time - start_time))
  j <- j + 1
  results[nrow(results) + 1,] <- c(gp_plot_data, sol_train_accuracy, sol_validation_accuracy, sol_test_accuracy, sol_nn_architecture, sol_model_name,
                                   sol_plot_data, exec_time)
  fitness_calculations <- fitness_calculations[0,]
  k_clear_session()
  k <- 0
}
write.table(results, file = "../results/iris.csv", sep = ";", row.names = FALSE) # Empty CSV.
#write.table(results, "../results/iris.csv", sep = ";", col.names = F, append = T, row.names = FALSE) # Concat CSV.