library(gtools)

#########################   QUEUE LENGTH   #########################

directory = "Applications/omnetpp-5.3/samples/queueinglib3/statistics/"
filename = "queueLength"

file = paste(directory, "csv/", filename, ".csv", sep="")
simulationResults = read.csv(file, sep = ",", header = T, stringsAsFactors = F)
vectorResults = simulationResults[simulationResults$type == "vector" | simulationResults$attrname=="iterationvars", ]

#order the dataframe according of run's numbers 
vectorResults = vectorResults[mixedorder(vectorResults$run, decreasing=T), ]

#create a list of [simulation parameters, vector of values]
list = list()
for(i in seq(1, length(vectorResults$run), by=2)) {
  attributes = vectorResults[i, 6]
  vector_values = as.numeric(unlist(strsplit(vectorResults[i+1, "vecvalue"], " ")))
  vector_timestamps = as.numeric(unlist(strsplit(vectorResults[i+1, "vectime"], " ")))
  list[[length(list) + 1]] = list(attributes, vector_values, vector_timestamps)
}

#take the values of parameters as row names of the resul table
rownames = c()
for(i in seq(1, length(list), by = 20)) {
  rownames = c(rownames, list[[i]][[1]])
}

#create a matrix with all stats for every combination of parameters
colnames = c("", "mean", "variance", "devStd", "errStd", "confInt90", "confInt95")
m = matrix(nrow = length(rownames), ncol = length(colnames))
rownames(m) = rownames
colnames(m) = colnames;

z = 1
for(pos in seq(1, length(list), by = 20)) {
  len = pos + 19

  means = c()
  for(i in pos:len) {
    row = list[[i]]  #vettore di timestamp e valori per ogni run
    
    #crea dataframe con (lunghezza coda, periodo di tempo che la coda ha avuto quella lunghezza)
    lengths = sort(unique(row[[2]]))
    values = c(0)
    queue_lengths = data.frame(lengths, values)
    
    #per ogni lunghezza 'l' della coda calcola i periodi di tempo in cui il sistema 
    #ha avuto 'l' elementi in coda
    for(j in 1: (length(row[[2]]) - 1)) {
      key = row[[2]][[j]]
      old_value = queue_lengths$values[queue_lengths$lengths == key]
      tmp = row[[3]][[j+1]] - row[[3]][[j]]
      queue_lengths$values[queue_lengths$lengths == key] = old_value + tmp
    }
    
    #aggiunge il valore della lunghezza media della coda per il run
    means = c(means, weighted.mean(queue_lengths$lengths, queue_lengths$values))
  }
  
  m[z, 1] = round(mean(means), 6)
  m[z, 2] = round(var(means), 6)
  m[z, 3] = round(sd(means), 6)
  m[z, 4] = round(sd(means)/sqrt(length(means)), 6)
  
  #confidence interval 90%
  t = t.test(means, conf.level = 0.90)
  inf = round(t[["conf.int"]][[1]], 6)
  sup = round(t[["conf.int"]][[2]], 6)
  #inf = mean(means) - (1.729 * sqrt(var(means))/sqrt(length(means)))
  #sup = mean(means) + (1.729 * sqrt(var(means))/sqrt(length(means)))
  m[z, 5] = paste("[", inf, ", ", sup , "]", sep = "")
  
  #confidence interval 95%
  t = t.test(means, conf.level = 0.95)
  inf = round(t[["conf.int"]][[1]], 6)
  sup = round(t[["conf.int"]][[2]], 6)
  #inf = mean(means) - (2.093 * sqrt(var(means))/sqrt(length(means)))
  #sup = mean(means) + (2.093 * sqrt(var(means))/sqrt(length(means)))
  m[z, 6] = paste("[", inf, ", ", sup , "]", sep = "")
  
  z = z + 1
}

write.table(m, file=paste(directory, "out/", filename, "_statistic123.csv", sep=""), sep=",")




