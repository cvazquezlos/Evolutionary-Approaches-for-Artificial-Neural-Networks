library("gramEvol")
library("neuralnet")

data <- NULL
train <- NULL      #70%
validation <- NULL #20%
test <- NULL       #10%

input <- NULL
output <- NULL

I <- NULL
O <- NULL

data_cleaning <- function(url, sep){
  data <<- read.csv(url, header=T, sep=sep)
  colnames(data) <- gsub("[^a-zA-Z]*", "", colnames(data))
  n <- nrow(data)
  input <<- paste(head(colnames(data),-1), collapse="+")
  I <<- length(colnames(data)) - 1
  O <<- 1
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
  
  nn <- neuralnet(paste(output,input,sep="~"), data=train, hidden=hidden_layers, 0.01, stepmax=1e+05, rep=1, 
                  linear.output=T, learningrate=0.01, algorithm = "backprop", err.fct="sse")
  
  nn.results <- compute(nn, validation[,c(1:(length(colnames(validation))-1))])
  plot(nn)
  # results <- data.frame(actual = validation[output], predicted = nn.results$net.result)
  # print(results)
  # fitness <- (sum((results$actual-results$predicted)^2))/as.double(nrow(validation))
  # print(fitness)
  #### print(data.frame(actual=validation[output], predicted=nn.results$net.result))
  fitness <- (sum((validation[output]-nn.results$net.result)^2))/as.double(nrow(validation))
  
  # print(validation_prediction$net.result)
  # validation_prediction_l <- validation_prediction$net.result*(max(data[output])-min(data[output]))+min(data[output])
  # results <- (validation[output])*(max(data[output])-min(data[output]))+min(data[output])
  # fitness <- (sum(results - validation_prediction_l)^2)/nrow(validation)
  return(fitness)
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

optimal_word <- GrammaticalEvolution(grammarDef, evaluation, 1, popSize=5, newPerGen=30, mutationChance=0.05, monitorFunc = monitor)
hidden_layers_optimal_word <- extract_neurons(optimal_word)
optimal <- neuralnet(paste(output,input,sep="~"), data=train, hidden=hidden_layers_optimal_word, 0.01, stepmax=1e+09, 
                     rep=1, linear.output=T, learningrate=0.01, algorithm = "backprop", err.fct="sse")
optimal.results <- compute(optimal, test[,c(1:(length(colnames(test))-1))])
print(data.frame(actual=test[output], predicted=optimal.results$net.result))
fitness <- (sum((test[output]-optimal.results$net.result)^2))/as.double(nrow(test))
print(fitness)
plot(optimal)



# Añadir la arquitectura resultante. 30% una arquitectura dada, u n20% otra arquitectura... etc
#   30% una arquitectura
#   20% otra...
#    ... 
# Añadir también el tamaño de la red de neuronas.
# Añadir relación entre la profundidad y el error cuántico medio. Usar fórmula del artículo de redes de neuronas.

# LUZY REPOSITORY IRIS FLOWER.
# Usar RELU en las capas ocultas y una función lineal a la salida en las redes de neuronas para REGRESIÓN.
# Para clasificación, usar RELU en las capas ocultas y la función softmax en la salida para devolver un vector de probabilidades para CLASIFICACIÓN y no usar el error cuántico medio con softmax porque es injusto
# para la función de error habría que usar el log-loss & cross-entropy. Nos quedamos con el J(W) que es la media.
# Ahora podemos dividir los problemas en regresión y clasificación para la memoria.