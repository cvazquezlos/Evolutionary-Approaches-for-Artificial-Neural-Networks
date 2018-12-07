library("gramEvol")
library("neuralnet")

INPUT = 2
OUTPUT = 3

grammar <- list(
  S = gsrule("<a><h>/<z>"),
  a = gsrule(replicate(INPUT, "n")),
  z = gsrule(replicate(OUTPUT, "n")),
  h = gsrule("<h><h>", "/<n>"),
  n = gsrule("n<n>", "n")
)

# https://datascienceplus.com/neuralnet-train-and-test-neural-networks-using-r/
# http://www.parallelr.com/r-deep-neural-network-from-scratch/
grammarDef <- CreateGrammar(grammar)
print(grammarDef)

# evaluation <- function(l) {
#   layers <- strsplit(l, "/")[[1]]
#   i <- 0
#   hidden <- list() # Contains the number of hidden layers and the number of neurons of each hidden layer.
#   for(j in head(layers, -1)) {
#     if (i!=0) {hidden[i] <- nchar(j)}
#     i <- i+1
#   }
# }

evaluation <- function(l) {
  layers <- strsplit(l, "/")[[1]]
  i <- 0
  hidden_l <- numeric(0) # Contains the number of hidden layers and the number of neurons of each hidden layer.
  for(j in head(layers, -1)) {
    if (i!=0) {hidden_l[i] <- nchar(j)}
    i <- i+1
  }
  
  # https://medium.com/analytics-vidhya/build-your-first-neural-network-model-on-a-structured-dataset-using-keras-d9e7de5c6724
  input <- NULL
  output <- NULL
  trainingdata <- NULL
  nn <- neuralnet(output~input, trainingdata, hidden=hidden_l, threshold=0.01)
  nn$result.matrix
  plot(nn)
}
