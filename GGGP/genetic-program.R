library("dummies")
library("gramEvol")
library("keras")
library("neuralnet")
library("stringr")

batch_size <- 4
epochs <- 100
data <- NULL
I <- NULL
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

data_cleaning <- function(url, sep) {
  data <<- read.csv(url, header=T, sep=sep)
  colnames(data) <- gsub("[^a-zA-Z]*", "", colnames(data))
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
  train <<- shuffled_df[1: round(0.7*n),]
  validation <- shuffled_df[(round(0.7*n)+1):round(0.9*n),]
  test <- shuffled_df[(round(0.9*n)+1):n,]
  X_train <<- train[,head(colnames(data), -3)] %>% as.matrix()
  y_train <<- train[,tail(colnames(data), 3)] %>% as.matrix()
  X_validation <<- validation[,head(colnames(data), -3)] %>% as.matrix()
  y_validation <<- validation[,tail(colnames(data), 3)] %>% as.matrix()
  X_test <<- test[,head(colnames(data), -3)] %>% as.matrix()
  y_test <<- test[,tail(colnames(data), 3)] %>% as.matrix()
  I <<- length(colnames(X_train))
  input <<- paste(colnames(X_train))
  O <<- length(colnames(y_train))
  output <<- paste(colnames(y_train))
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
  history <- model %>% fit(X_train, y_train, epochs = epochs)
  if (mode == 0) {
    score <- model %>% evaluate(X_validation, y_validation, batch_size = batch_size)
  } else {
    score <- model %>% evaluate(X_test, y_test)
  }
  return(score['loss'][[1]])
}

monitor <- function(results){
  cat("--------------------\n")
  print(results)
}

data_cleaning("../datasets/classification/iris.csv", ",")
grammar <- list(
  S = gsrule("<a><h>/<z>"),
  a = grule(replicate(I, "n")),
  z = grule(replicate(O, "n")),
  h = gsrule("<h><h>", "/<n>"),
  n = gsrule("n<n>", "n")
)
grammarDef <- CreateGrammar(grammar)

optimal_word <- GrammaticalEvolution(grammarDef, evaluation, 1, popSize=5, newPerGen=30, mutationChance=0.05, monitorFunc = monitor)
hidden_layers_optimal_word <- extract_neurons(optimal_word)
arch_optimal <- neuralnet(paste(output, input, sep = "~"), data = train, hidden = hidden_layers_optimal_word)
plot(arch_optimal)
mode <- 1
print(evaluation(optimal_word))