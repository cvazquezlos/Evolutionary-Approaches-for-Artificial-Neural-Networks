library("gramEvol")
library("keras")
library("stringr")

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

generation <- function(n) {
  population <- GrammarRandomExpression(CreateGrammar(GRAMMAR), n)
  i <- 1
  for (individual in population) {
    population[[i]] <- list(id = id, architecture = gsub("\"", "", toString(individual)), evaluated = FALSE,
                            loss = NULL, metric = NULL, saved_model = NULL) # Cleaning up the expression.
    i <- i + 1
    id <<- id + 1
  }
  return(population)
}


evaluation <- function(individual, mode) {
  # Neurons extraction
  hidden_layers <- numeric(0)
  i <- 0
  for (layer in head(strsplit(individual$architecture, "/")[[1]], -1)) {
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
  png(filename = paste0("data/history/", individual$id, ".png"))
  plot(history)
  dev.off()
  save_model_hdf5(model, paste0("data/model/", individual$architecture, ".h5"))
  score <- model %>% evaluate(X_validation, y_validation)
  individual$evaluated <- TRUE
  individual$loss <- score['loss'][[1]]
  individual$metric <- score['acc'][[1]]
  individual$saved_model <- paste0("data/model/", individual$architecture, ".h5")
  return(double(score['loss'][[1]]))
}

selection <- function(population, no_parents) {
  
}

population <- generation(20)