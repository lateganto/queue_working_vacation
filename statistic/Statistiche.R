library(gtools)

#########################   VECTOR STATISTICS   #########################
vectorStatistic = function(directory, filename) {
  
  file = paste(directory, filename, ".csv", sep="")
  simulationResults = read.csv(file, sep = ",", header = T, stringsAsFactors = F)
  vectorResults = simulationResults[simulationResults$type == "vector" | simulationResults$attrname=="iterationvars", ]
  
  #order the dataframe according of run's numbers 
  vectorResults = vectorResults[mixedorder(vectorResults$run, decreasing=T), ]
  
  #create a list of [simulation parameters, vector]
  list = list()
  for(i in seq(1, length(vectorResults$run), by=2)) {
    attributes = vectorResults[i, 6]
    vector = as.numeric(unlist(strsplit(vectorResults[i+1, 17], " ")))
    list[[length(list) + 1]] = list(attributes, vector)
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
  for(i in seq(1, length(list), by = 20)) {
      len = i + 19
      
      #for every 20 runs computes means that are used to compute stats
      means = c()
      for(j in i:len){
        means = c(means, mean(list[[j]][[2]]))
      }
      
      m[z, 2] = round(mean(means), 6)
      m[z, 3] = round(var(means), 6)
      m[z, 4] = round(sd(means), 6)
      m[z, 5] = round(sd(means)/sqrt(length(means)), 6)
      
      #confidence interval 90%
      t = t.test(means, conf.level = 0.90)
      inf = round(t[["conf.int"]][[1]], 6)
      sup = round(t[["conf.int"]][[2]], 6)
      #inf = mean(means) - (1.729 * sqrt(var(means))/sqrt(length(means)))
      #sup = mean(means) + (1.729 * sqrt(var(means))/sqrt(length(means)))
      m[z, 6] = paste("[", inf, ", ", sup , "]", sep = "")
      
      #confidence interval 95%
      t = t.test(means, conf.level = 0.95)
      inf = round(t[["conf.int"]][[1]], 6)
      sup = round(t[["conf.int"]][[2]], 6)
      #inf = mean(means) - (2.093 * sqrt(var(means))/sqrt(length(means)))
      #sup = mean(means) + (2.093 * sqrt(var(means))/sqrt(length(means)))
      m[z, 7] = paste("[", inf, ", ", sup , "]", sep = "")
      
      z = z + 1
  }
  
  write.table(m, file=paste(directory, "out/", filename, "_statistic.csv", sep=""), sep="\t")
}

directory = "Applications/omnetpp-5.3/samples/queueinglib3/statistic/"
filenames = c("queueLength", 
              "queueLengthBusy", 
              "queueLengthVacation", 
              "sojournTime",
              "sojournTimeBusy",
              "sojournTimeVacation")

for(i in 1:length(filenames)) {
  vectorStatistic(directory, filenames[i])
}

######################################################

data2 = read.csv(
  "Applications/omnetpp-5.3/samples/queueinglib3/statistic/vacation.csv",
  sep = ",",
  stringsAsFactors = FALSE
)

d = data2[data2$type == "vector", ]
d1 = as.numeric(unlist(strsplit(d$vectime, " ")))
d2 = as.numeric(unlist(strsplit(d$vecvalue, " ")))

data = data.frame()
data = cbind(d1, d2)
#data = as.data.frame(data)

busyPeriods = c()
for (i in seq(1, length(d1), by = 2)) {
  busyPeriods[[length(busyPeriods) + 1]] = d1[[i + 1]] - d1[[i]]
}
avgBusyPeriods = mean(busyPeriods)

vacationPeriods = c()
for (i in seq(2, length(d1) - 1, by = 2)) {
  vacationPeriods[[length(vacationPeriods) + 1]] = d1[[i + 1]] - d1[[i]]
}
avgVacationPeriods = mean(vacationPeriods)

vec = c()
for (i in 1:(length(d1) - 1)) {
  vec[[length(vec) + 1]] = d1[[i + 1]] - d1[[i]]
}
avgVec = mean(vec)
