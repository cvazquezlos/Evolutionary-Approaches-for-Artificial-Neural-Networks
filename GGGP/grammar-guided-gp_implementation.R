library("dummies")
library("ggplot2")
library("gramEvol")
library("jsonlite")
library("keras")
library("sqldf")
library("stringr")
# install.packages("dummies")
# install.packages("ggplot2")
# install.packages("gramEvol")
# install.packages("jsonlite")
# install.packages("keras")
# install_keras()
# install.packages("sqldf")
# install.packages("stringr")

execution <- 1
GRAMMAR <- list(
  S = gsrule("<a><h>/<z>"),
  a = grule("nnnn"), # Update a with many n as value of I.
  z = grule("nnn"),  # Update z with many n as value of O.
  h = gsrule("<h><h>", "<h><h>", "/<n>"),
  n = gsrule("n<n>", "n")
)
I <- NA
id <- 1
p <- 25
O <- NA

base_architecture <- data.frame(id = rep(NA, p), architecture = rep(NA, p), evaluated = rep(NA, p), loss = rep(NA, p), metric = rep(NA, p), 
                                saved_model = rep(NA, p), stringsAsFactors = FALSE)

############################################################# EVOLUTIONARY  OPERATORS #############################################################
generation <- function(n) {
  architectures <- GrammarRandomExpression(CreateGrammar(GRAMMAR), p)
  i <- 1
  population <- base_architecture
  for (individual in architectures) {
    population[i,] <- c(id, toString(gsub("\"", "", toString(individual))), FALSE, NA, NA, NA)
    i <- i + 1
    id <<- id + 1
  }
  return(population)
}

evaluation <- function(individual, split_crit, mode) {
  # Neurons extraction
  hidden_layers <- numeric(0)
  i <- 0
  if (split_crit == 0) {
    splitting <- head(strsplit(individual$architecture[[1]], "/")[[1]], -1)
  } else {
    splitting <- head(strsplit(toString(individual$architecture), "/")[[1]], -1)
  }
  for (layer in splitting) {
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
  history <- model %>% fit(rbind(X_train, X_validation), rbind(y_train, y_validation), validation_split = 0.235294, epochs = 2500, 
                           verbose = 0, callbacks = list(
    callback_early_stopping(monitor = "val_loss", min_delta = 0, patience = 100, verbose = 1, mode = "auto")
  ))
  plot(history)
  model_name <- paste0(str_replace_all(individual$architecture, "/", "_"), "-", individual$id)
  ggsave(paste0("data/", execution, "/history/", model_name, ".png"))
  save_model_hdf5(model, paste0("data/", execution, "/model/", model_name, ".h5"))
  score <- model %>% evaluate(X_train, y_train)
  individual$evaluated <- TRUE
  individual$loss <- score['loss'][[1]]
  individual$metric <- score['acc'][[1]]
  individual$saved_model <- model_name
  return(individual)
}

selection <- function(no_childs) {
  no_parents <- no_childs
  matting_pool <- data.frame()
  for (j in c(1:(no_childs/2))) {
    parents_sample <- population[sample(nrow(population), no_childs/2),]
    ordered_parents_sample <- parents_sample[order(unlist(parents_sample$loss)),]
    matting_pool <- rbind(matting_pool, head(ordered_parents_sample, 2))
  }
  return(matting_pool)
}

crossover <- function(parents) {
  s_p1_parts <- extract_hidden_layers(parents$architecture[[1]])
  s_p2_parts <- extract_hidden_layers(parents$architecture[[2]])
  random_s_p1 <- sample(s_p1_parts, 1)
  random_s_p2 <- sample(s_p2_parts, 1)
  s_p1_parts <- lapply(s_p1_parts, function(x) {if(x==random_s_p1) {random_s_p2} else {x}})
  s_p2_parts <- lapply(s_p2_parts, function(x) {if(x==random_s_p2) {random_s_p1} else {x}})
  df <- data.frame(id = integer(),
                   architecture = character(),
                   evaluated = logical(),
                   loss = double(),
                   metric = double(),
                   saved_model = character(),
                   stringsAsFactors = FALSE)
  df <- rbind(df, data.frame(id = id, architecture = toString(add_inout_layers(s_p1_parts)), evaluated = FALSE, loss = NA, metric = NA, saved_model = NA))
  id <<- id + 1
  df <- rbind(df, data.frame(id = id, architecture = toString(add_inout_layers(s_p2_parts)), evaluated = FALSE, loss = NA, metric = NA, saved_model = NA))
  id <<- id + 1
  return(df)
}

replacement <- function(children) {
  max_population <- rbind(population, children)
  ordered_max_population <- max_population[order(unlist(max_population$loss)),]
  return(ordered_max_population[c(1:p),])
}

############################################################### AUXILIARY FUNCTIONS ###############################################################
add_inout_layers <- function(hidden) {
  return(paste(strrep("n", I), "/", paste0(hidden, collapse = "/"), "/", strrep("n", O), sep = ""))
}

extract_hidden_layers <- function(architecture) {
  split <- strsplit(architecture, split = "/")
  return(tail(head(split[[1]], -1), -1))
}

###################################################################################################################################################
################################################################# MAIN  ALGORITHM #################################################################
###################################################################################################################################################
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
execution_results <- data.frame(execution = integer(),
                                architecture = character(),
                                acc_train = numeric(),
                                acc_validation = numeric(),
                                acc_test = numeric(),
                                time = numeric(),
                                saved_model = character(),
                                stringsAsFactors = F)
start_time = Sys.time()
iteration_results <- data.frame(iteration = integer(),
                                avg_loss = numeric(),
                                best_loss = numeric(),
                                stringsAsFactors = T)
dir.create(paste0("data/", execution), showWarnings = F)
dir.create(paste0("data/", execution, "/history"), showWarnings = F)
dir.create(paste0("data/", execution, "/model"), showWarnings = F)
# Generation
population <- generation(p)
# Evaluation
for (individual in 1:nrow(population)) {
  population[individual,] = evaluation(population[individual,], 0, 0)
}
iteration <- 1
while (T) {
  # Stop condition
  results <- sqldf("select * from population where loss <= 0.035 AND metric >= 0.98 order by id")
  if ((nrow(results) != 0) | (length(unique(population$architecture)) == 1)) {
    solution <- results[1,]
    break
  } else if (iteration == 30) {
    results <- population[order(unlist(population$loss, -population$metric)),]
    solution <- results[1,]
    break
  } else {
    # Selection
    matting_pool <- selection(8)
    # Crossover
    children <- data.frame(id = integer(),
                           architecture = character(),
                           evaluated = logical(),
                           loss = numeric(),
                           metric = numeric(),
                           saved_model = character(),
                           stringsAsFactors = FALSE)
    for (i in c(1:(nrow(matting_pool)/2))) {
      j <- (i*2)-1
      children <- rbind(children, crossover(matting_pool[c(j:(j+1)),]))
    }
    # Evaluation
    for (individual in 1:nrow(children)) {
      children[individual,] = evaluation(children[individual,], 1, 0)
    }
    # Replacement
    population <- replacement(children)
    population <- population[order(unlist(population$id)),]
  }
  iteration_results <- rbind(iteration_results, data.frame(iteration = iteration, avg_loss = ((Reduce("+", as.numeric(population$loss))) / p),
                                                           best_loss = population[which.max(population$loss), 4]))
  iteration <- iteration + 1
}
model <- load_model_hdf5(paste0("data/", execution, "/model/", solution$saved_model, ".h5"))
acc_train <- (model %>% evaluate(X_train, y_train))['acc'][[1]]
acc_validation <- (model %>% evaluate(X_validation, y_validation))['acc'][[1]]
acc_test <- (model %>% evaluate(X_test, y_test))['acc'][[1]]
end_time = Sys.time()
iteration_results$avg_loss <- as.numeric(iteration_results$avg_loss)
iteration_results$best_loss <- as.numeric(levels(iteration_results$best_loss))
plot_iteration_results <- ggplot(iteration_results, aes(iteration)) +
  geom_line(aes(y = avg_loss, color = "blue")) +
  geom_line(aes(y = best_loss, color = "red"))
plot(plot_iteration_results)
ggsave(paste0("data/", execution, "/", execution, ".pdf"))
execution_results <- rbind(execution_results, data.frame(execution = execution, architecture = solution$architecture,
                                                         acc_train = acc_train,
                                                         acc_validation = acc_validation,
                                                         acc_test = acc_test,
                                                         time = as.double(toString(end_time - start_time)),
                                                         saved_model = solution$saved_model))
write.csv(population, paste0("data/", execution, "/final_population.csv"))
write.csv(execution_results, paste0("data/", execution, "/execution_results.csv"))
execution <- execution + 1
# Para cada iteración del programa genético de cada ejecución, almacenar la media de los individuos y al mejor de ellos.
# Por cada ejecución sacar: histórico de entrenamiento y validación del mejor y de la media de la población. También, el accuracy
# del mejor individuo en testeo y almacenar la arquitectura, las 80 arquitecturas.
# En cada iteración, se guarda train para mostrarlo y el fitness sea el train, porque una red es muy buena si el train es muy bueno, aunque la condición de parada del entrenamiento es el val.
# Crear tabla agrupando por arquitectura.
# Pasar un correo cuando acabe las 80 ejecuciones.