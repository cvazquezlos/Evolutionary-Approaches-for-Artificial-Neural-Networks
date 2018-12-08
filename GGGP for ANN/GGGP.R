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

evaluation <- function(l) {
  layers <- strsplit(l, "/")[[1]]
  i <- 0
  hidden_l <- numeric(0) # Contains the number of hidden layers and the number of neurons of each hidden layer.
  for(j in head(layers, -1)) {
    if (i!=0) {hidden_l[i] <- nchar(j)}
    i <- i+1
  }
  print(hidden_l)
  # https://medium.com/analytics-vidhya/build-your-first-neural-network-model-on-a-structured-dataset-using-keras-d9e7de5c6724
  # http://www.learnbymarketing.com/tutorials/neural-networks-in-r-tutorial/
  # Data pre-processing
  bank <- read.csv("./bank.csv", header=TRUE, sep=";")
  bank$balance <- (bank$balance-min(bank$balance)) / (max(bank$balance)-min(bank$balance))
  bank$age <- (bank$age-min(bank$age)) / (max(bank$age)-min(bank$age))
  bank$previous <- (bank$previous-min(bank$previous)) / (max(bank$previous)-min(bank$previous))
  bank$campaign <- (bank$campaign-min(bank$campaign)) / (max(bank$campaign)-min(bank$campaign))
  bank$education <- relevel(bank$education, ref = "secondary")
  bank_matrix <- model.matrix(~age+job+marital+education+default+balance+housing+loan+contact+day+month+duration+campaign+pdays+previous+poutcome+y, data=bank)
  # There are some names that have invalid characters as "-". We have to fix it.
  colnames(bank_matrix)[3] <- "jobbluecollar"
  colnames(bank_matrix)[8] <- "jobselfemployed"
  col_list <- paste(c(colnames(bank_matrix[, -c(1,44)])), collapse="+")
  col_list <- paste(c("yyes~", col_list), colapse="")
  f <- formula(col_list)
  set.seed(2)
  nn <- neuralnet(f, data=bank_matrix, hidden=hidden_l, threshold=0.01, linear.output = T)
  plot(nn)
}
