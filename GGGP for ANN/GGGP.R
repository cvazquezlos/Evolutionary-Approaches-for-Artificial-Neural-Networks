library("gramEvol")
library("neuralnet")

data <- NULL
train <- NULL      #70%
validation <- NULL #20%
test <- NULL       #10%

input <- NULL
output <- NULL

data_cleaning <- function(url, sep){
  data <<- read.csv(url, header=T, sep=sep)
  colnames(data) <- gsub("[^a-zA-Z]*", "", colnames(data))
  n <- nrow(data)
  input <<- paste(head(colnames(data),-1), collapse="+")
  output <<- paste(tail(colnames(data),1))
  max <- apply(data, 2, max)
  min <- apply(data, 2, min)
  scaled_data <- scale(data, center=min, scale=max-min) # Normalization for numeric datasets.
  shuffled_df <- as.data.frame(scaled_data[sample(n),])
  train <<- shuffled_df[1:round(0.7*n),]
  validation <<-  shuffled_df[(round(0.7*n)+1):round(0.9*n),]
  test <<- shuffled_df[(round(0.9*n)+1):n,]
}
data_cleaning("./cereals.csv", ",")

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

# https://www.analyticsvidhya.com/blog/2017/09/creating-visualizing-neural-network-in-r/
evaluation <- function(word) {
  hidden_layers <- extract_neurons(word)
  
  # https://www.rdocumentation.org/packages/neuralnet/versions/1.33/topics/neuralnet PARAMETERS
  nn <- neuralnet(paste(output,input,sep="~"), data=train, hidden=hidden_layers, linear.output=T)
  
  # https://gist.github.com/abresler/d2c324b44d7319b58309 VALIDATION
  validation_prediction <- compute(nn, validation[,c(1:(length(colnames(validation))-1))])
  validation_prediction_l <- validation_prediction$net.result*(max(data[output])-min(data[output]))+min(data[output])
  results <- (validation[output])*(max(data[output])-min(data[output]))+min(data[output])
  fitness <- (sum(results - validation_prediction_l)^2)/nrow(validation)
  return(fitness)
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