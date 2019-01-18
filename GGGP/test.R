fitness_calculations <- data.frame(individual = character(), gen_check = integer(), acc = double(), loss = double(), 
                                   saved_model = character(), history = character(), stringsAsFactors = FALSE)
filtered_fitness_calculations <- subset(fitness_calculations, individual == "nn/nnnn/nn" & gen_check == 2)


fitness_calculations <- structure(list(individual = c("nnn/nn/nn/nn", "nnn/n/nn", "nnn/nn/nn/nn"), 
                                        gen_check = c(2L, 2L, 2L), 
                                        acc = c(0.9889, 0.7845, 0.564), 
                                        loss = c(0.0112, 0.3451, 0.4231)), 
                                   class = "data.frame", row.names = c("1", "2", "3"))


ordered_fitness_calculations <- fitness_calculations[order(-fitness_calculations$acc),]
ordered_fitness_calculations
solution <- head(ordered_fitness_calculations[ordered_fitness_calculations$individual == "nnn/nn/nn/nn",], 1)
typeof(solution$acc)


i1 <- with(ordered_fitness_calculations, individual == "nnn/nn/nn/nn")
i2 <- !duplicated(i1) & i1
fitness_calculations[i2]

i1 <- with(ordered_fitness_calculations, individual == optimal_word)
i2 <- !duplicated(i1) & i1







i1 <- with(fitness_calculations, individual == "nnn/nn/nn/nn" & gen_check ==  2)
i2 <- !duplicated(i1) & i1

fitness_calculations$gen_check[i2] <- 3
fitness_calculations