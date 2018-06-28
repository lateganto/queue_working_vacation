library(gtools)

#########################   QUEUE LENGTH   #########################
vectorStatistic = function(directory, filename) {

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
  colnames = c("mean", "variance", "devStd", "errStd", "inf confInt90","sup confInt90", "inf confInt95", "sup confInt95")
  m = matrix(nrow = length(rownames), ncol = length(colnames))
  rownames(m) = rownames
  colnames(m) = colnames;
  
  z = 1
  for(pos in seq(1, length(list), by = 20)) {
    len = pos + 19
  
    means = c()
    for(i in pos:len) {
      row = list[[i]]  #run with timestamp and values vectors
      
      #create a dataframe with (queue lengths, sum of periods queue have this length)
      lengths = sort(unique(row[[2]]))
      values = c(0)
      queue_lengths = data.frame(lengths, values)
      
      #for every queue length 'l' computes periods the queue has 'l' elements
      for(j in 1: (length(row[[2]]) - 1)) {
        key = row[[2]][[j]]
        old_value = queue_lengths$values[queue_lengths$lengths == key]
        tmp = row[[3]][[j+1]] - row[[3]][[j]]
        queue_lengths$values[queue_lengths$lengths == key] = old_value + tmp
      }
      
      #add values of mean queue length for 20 runs
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
    m[z, 5] = inf
    m[z, 6] = sup
    
    #confidence interval 95%
    t = t.test(means, conf.level = 0.95)
    inf = round(t[["conf.int"]][[1]], 6)
    sup = round(t[["conf.int"]][[2]], 6)
    #inf = mean(means) - (2.093 * sqrt(var(means))/sqrt(length(means)))
    #sup = mean(means) + (2.093 * sqrt(var(means))/sqrt(length(means)))
    m[z, 7] = inf
    m[z, 8] = sup
    
    z = z + 1
  }
  
  write.table(m, file=paste(directory, "out/", filename, "_statistic.csv", sep=""), sep=",")
}

directory = "Applications/omnetpp-5.3/samples/queueinglib3/statistics/"
filenames = c("queueLength",
              "queueLengthBusy",
              "queueLengthVacation")

for(i in 1:length(filenames)) {
  vectorStatistic(directory, filenames[i])
}




