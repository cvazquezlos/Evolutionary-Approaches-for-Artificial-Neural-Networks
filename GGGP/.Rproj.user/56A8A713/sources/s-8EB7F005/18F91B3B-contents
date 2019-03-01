HISTORIES_DIRECTORY <- "../results/classification/iris/"

histories <- list.files(HISTORIES_DIRECTORY)
histories <- histories[!is.na(as.numeric(histories))]

final_populations <- data.frame(id = character(), 
                                architecture = character(), 
                                evaluated = character(), 
                                loss = character(), 
                                metric = character(), 
                                saved_model = character(), 
                                execution = character(),
                                stringsAsFactors = FALSE)
y <- lapply(histories, function(x) {
  aux = readRDS(paste0(HISTORIES_DIRECTORY, x, "/final_population.rds"))
  aux$execution = rep(x, nrow(aux))
  final_populations <<- rbind(final_populations, aux)
})
