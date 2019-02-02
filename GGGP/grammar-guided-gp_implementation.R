library("dummies")
library("gramEvol")
library("jsonlite")
library("keras")
library("neuralnet")
library("stringr")
library("ggplot2")
#install_keras(tensorflow = "gpu")

# Grammar definition
GRAMMAR <- list(
  S = gsrule("<a><h>/<z>"),
  a = grule("nnnn"), # Update a with many n as value of I.
  z = grule("nnn"),  # Update z with many n as value of O.
  h = gsrule("<h><h>", "<h><h>", "/<n>"),
  n = gsrule("n<n>", "n")
)
I <- 4
id <- 0
O <- 3

######################################################## EVOLUTIONARY OPERATORS ########################################################
generation <- function(n) {
  population <- GrammarRandomExpression(CreateGrammar(GRAMMAR), n)
  i <- 1
  for (individual in population) {
    population[[i]] <- list(id = id, architecture = gsub("\"", "", toString(individual)), evaluated = FALSE,
                            loss = NULL, metric = NULL, saved_model = NULL)
    i <- i + 1
    id <<- id + 1
  }
  return(as.data.frame(do.call(rbind, population)))
}

evaluation <- function(individual, mode) {
  # Neurons extraction
  hidden_layers <- numeric(0)
  i <- 0
  for (layer in head(strsplit(individual$architecture[[1]], "/")[[1]], -1)) {
    if (i != 0) {
      hidden_layers[i] <- nchar(layer)
    }
    i <- i + 1
  }
  # Individual evaluation
  model <- keras_model_sequential()
  model %>% layer_dense(units = hidden_layers[1], input_shape = c(I), activation = "relu")
  for (layer in tail(hidden_layers, 1)) {
    model %>% layer_dense(units = layer, activation = "relu")
  }
  if (mode == 0) {
    model %>% layer_dense(units = O, activation = "softmax")
    model %>% compile(
      optimizer = "adam",
      loss = "categorical_crossentropy",
      metrics = c("accuracy")
    )
  } else {
    # TODO: Regression NN output layer.
  }
  history <- model %>% fit(rbind(X_train, X_validation), rbind(y_train, y_validation), validation_split = 0.235294, epochs = 1000, verbose = 0, callbacks = list(
    callback_early_stopping(monitor = "val_loss", min_delta = 0, patience = 100, verbose = 1, mode = "auto")
  ))
  plot(history)
  model_name <- paste0(str_replace_all(individual$architecture, "/", "_"), "-", individual$id)
  ggsave(paste0("data/history/", model_name, ".pdf"))
  save_model_hdf5(model, paste0("data/model/", model_name, ".h5"))
  score <- model %>% evaluate(X_validation, y_validation)
  individual$evaluated <- TRUE
  individual$loss <- score['loss'][[1]]
  individual$metric <- score['acc'][[1]]
  individual$saved_model <- model_name
  return(individual)
}

# Tournament selection
selection <- function(no_childs) {
  no_parents <- no_childs
  matting_pool <- data.frame()
  for (n in c(1:(no_childs/2))) {
    parents_sample <- population[sample(nrow(population), no_childs/2),]
    ordered_parents_sample <- parents_sample[order(unlist(parents_sample$loss)),]
    matting_pool <- rbind(matting_pool, head(ordered_parents_sample, 2))
  }
  return(matting_pool)
}

crossover <- function(parents) {
  p1_parts <- strsplit(parents$architecture[[1]], split = "/")
  s_p1_parts <- tail(head(p1_parts[[1]], -1), -1)
  random_s_p1 <- sample(s_p1_parts, 1)
  p2_parts <- strsplit(parents$architecture[[2]], split = "/")
  s_p2_parts <- tail(head(p2_parts[[1]], -1), -1)
  random_s_p2 <- sample(s_p2_parts, 1)
  p1_parts <- lapply(s_p1_parts, function(x)if(x==random_s_p1)random_s_p2)
  p2_parts <- lapply(s_p2_parts, function(x)if(x==random_s_p2)random_s_p1)
  children <- NULL
  for (k in c(1:2)) {
    children[k] <- list(id = id, architecture = add_static_layers(p1_parts), evaluated = FALSE,
                        loss = NULL, metric = NULL, saved_model = NULL)
    id <<- id + 1
  }
  return(children)
}

######################################################### AUXILIARY FUNCTIONS #########################################################
add_inout_layers <- function(hidden) {
  return(paste(strrep("n", I), "/", paste0(hidden, collapse = "/"), "/", strrep("n", O), sep = ""))
}

extract_hidden_layers <- function(architecture) {
  return(strsplit(architecture, split = "/"))
}

# MAIN ALGORITHM
data <- read.csv("../datasets/classification/iris.csv", header = T, sep = ",")
n <- nrow(data)
aux <- dummy.data.frame(data, names = c("class"), sep = "")
rm("data")
data <- aux
rm("aux")
shuffled_df <- as.data.frame(data[sample(n),])
colnames(shuffled_df) <- gsub("[^a-zA-Z]*", "", colnames(shuffled_df))
c <- colnames(shuffled_df)
train <- shuffled_df[1: round(0.7*n),]
validation <- shuffled_df[(round(0.7*n)+1):round(0.9*n),]
test <- shuffled_df[(round(0.9*n)+1):n,]
X_train <- train[,head(colnames(shuffled_df), -3)] %>% as.matrix()
y_train <- train[,tail(colnames(shuffled_df), 3)] %>% as.matrix()
X_validation <- validation[,head(colnames(shuffled_df), -3)] %>% as.matrix()
y_validation <- validation[,tail(colnames(shuffled_df), 3)] %>% as.matrix()
X_test <- test[,head(colnames(shuffled_df), -3)] %>% as.matrix()
y_test <- test[,tail(colnames(shuffled_df), 3)] %>% as.matrix()
I <- length(colnames(X_train))
O <- length(colnames(y_train))

# Population creation
population <- generation(50)
for (individual in 1:nrow(population)) {
  population[individual,] = evaluation(population[individual,], 0)
}
#save.image("01022019 - Population creation.RData")
load("01022019 - Population creation.RData")
# First generation
matting_pool <- selection(10)
children <- data.frame()
for (i in c(1:(nrow(matting_pool)/2))) {
  j <- (i*2)-1
  children <- rbind(children, crossover(matting_pool[c(j:(j+1)),]))
}