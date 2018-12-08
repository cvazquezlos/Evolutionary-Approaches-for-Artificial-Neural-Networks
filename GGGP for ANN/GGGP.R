library("gramEvol")
library("neuralnet")

train <- NULL      #70%
validation <- NULL #20%
test <- NULL       #10%

input <- NULL
output <- NULL

data_cleaning <- function(url, sep){
  data <- read.csv(url, header=T, sep=sep)
  colnames(data) <- gsub("[^a-zA-Z]*", "", colnames(data))
  input <<- paste(head(colnames(data),-1), collapse="+")
  output <<- paste(tail(colnames(data),1))
  n <- nrow(data)
  max <- apply(data, 2, max)
  min <- apply(data, 2, min)
  scaled_data <- scale(data, center=min, scale=max-min) # Normalization for numeric datasets.
  shuffled_df <- as.data.frame(scaled_data[sample(n),])
  str(sh)
  train_indices <- 1:round(0.7*n)
  validation_indices <- (round(0.7*n)+1):round(0.9*n)
  test_indices <- (round(0.9*n)+1):n
  train <<- shuffled_df[train_indices,]
  validation <<-  shuffled_df[validation_indices,]
  test <<- shuffled_df[test_indices,]
}

extract_neurons <- function(word) {
  layers <- strsplit(word, "/")[[1]]
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
  nn <- neuralnet(rating~calories+protein+fat+sodium+fiber, trainNN, hidden=hidden_l, linear.output=T)
  nn <- neuralnet(output~input, data=train, hidden=hidden_layers, linear.output=T)
  plot(nn)
}

monitor <- function(results){
  cat("--------------------\n")
  print(results)
}

grammar <- list(
  S = gsrule("<a><h>/<z>"),
  a = gsrule(replicate(INPUT, "n")),
  z = gsrule(replicate(OUTPUT, "n")),
  h = gsrule("<h><h>", "/<n>"),
  n = gsrule("n<n>", "n")
)

grammarDef <- CreateGrammar(grammar)

EvolutionStrategy.int(genomeLen=5, genomeMin=2, genomeMax=8, suggestion=NULL,
                      popSize = 2,
                      newPerGen = 4,
                      iterations = 500,
                      terminationCost = NA,
                      mutationChance = 0.05,
                      monitorFunc = monitor,
                      evalFunc = evaluation,
                      allowrepeat = TRUE,
                      showSettings = TRUE,
                      verbose = TRUE,
                      plapply = 
                     )