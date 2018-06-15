#########################   BUSY/VACATION PERIOD   #########################

directory = "Applications/omnetpp-5.3/samples/queueinglib3/statistic/"
file = paste(directory, "busyVacationPeriod.csv", sep = "")

busyVacationPeriods = read.csv(file, sep = ",", header = T, stringsAsFactors = F)
vectorResults = busyVacationPeriods[busyVacationPeriods$type == "vector" | busyVacationPeriods$attrname=="iterationvars", ]

#order the dataframe according of run's numbers 
vectorResults = vectorResults[mixedorder(vectorResults$run, decreasing=T), ]

#create a list of [simulation parameters, vector of timestamps when the server goes in vacation or not]
list = list()
for(i in seq(1, length(vectorResults$run), by=2)) {
  attributes = vectorResults[i, 6]
  vector = as.numeric(unlist(strsplit(vectorResults[i+1, "vectime"], " ")))
  list[[length(list) + 1]] = list(attributes, vector)
}

#take the values of parameters as row names of the resul table
rownames = c()
for(i in seq(1, length(list), by = 20)) {
  rownames = c(rownames, list[[i]][[1]])
}

#create a matrix with all stats for every combination of parameters
colnames = c("mean", "variance", "devStd", "errStd", "confInt90", "confInt95")
busy = matrix(nrow = length(rownames), ncol = length(colnames))
vacation = matrix(nrow = length(rownames), ncol = length(colnames))
rownames(busy) = rownames
colnames(busy) = colnames
rownames(vacation) = rownames
colnames(vacation) = colnames;

z = 1
for(i in seq(1, length(list), by = 20)) {
  len = i + 19
  
  #for every 20 runs computes means that are used to compute stats
  busyMeans = c()
  vacationMeans = c()
  for(j in i:len){
    #vector with timestamps
    vector = list[[j]][[2]]
    
    #compute vector distances for busy periods
    busyPeriods = c()
    for (pos in seq(1, length(vector) - 1, by = 2)) {
      busyPeriods = c(busyPeriods, vector[[pos + 1]] - vector[[pos]])
      #busyPeriods[[length(busyPeriods) + 1]] = vector[[pos + 1]] - vector[[pos]]
    }
    
    #compute vector distances for busy periods
    vacationPeriods = c()
    for (pos in seq(2, length(vector) - 1, by = 2)) {
      vacationPeriods = c(vacationPeriods, vector[[pos + 1]] - vector[[pos]])
      #busyPeriods[[length(busyPeriods) + 1]] = vector[[pos + 1]] - vector[[pos]]
    }
    
    busyMeans = c(busyMeans, mean(busyPeriods))
    vacationMeans = c(vacationMeans, mean(vacationPeriods))
  }
  
  print(length(busyMeans))
  print(length(vacationMeans))
  
  busy[z, 1] = round(mean(busyMeans), 6)
  busy[z, 2] = round(var(busyMeans), 6)
  busy[z, 3] = round(sd(busyMeans), 6)
  busy[z, 4] = round(sd(busyMeans)/sqrt(length(busyMeans)), 6)
  
  vacation[z, 1] = round(mean(vacationMeans), 6)
  vacation[z, 2] = round(var(vacationMeans), 6)
  vacation[z, 3] = round(sd(vacationMeans), 6)
  vacation[z, 4] = round(sd(vacationMeans)/sqrt(length(vacationMeans)), 6)
  
  #BUSY confidence interval 90%
  t = t.test(busyMeans, conf.level = 0.90)
  inf = round(t[["conf.int"]][[1]], 6)
  sup = round(t[["conf.int"]][[2]], 6)
  busy[z, 5] = paste("[", inf, ", ", sup , "]", sep = "")
  
  #BUSY confidence interval 95%
  t = t.test(busyMeans, conf.level = 0.95)
  inf = round(t[["conf.int"]][[1]], 6)
  sup = round(t[["conf.int"]][[2]], 6)
  busy[z, 6] = paste("[", inf, ", ", sup , "]", sep = "")
  
  #VACATION confidence interval 90%
  t = t.test(vacationMeans, conf.level = 0.90)
  inf = round(t[["conf.int"]][[1]], 6)
  sup = round(t[["conf.int"]][[2]], 6)
  vacation[z, 5] = paste("[", inf, ", ", sup , "]", sep = "")
  
  #VACATION confidence interval 95%
  t = t.test(vacationMeans, conf.level = 0.95)
  inf = round(t[["conf.int"]][[1]], 6)
  sup = round(t[["conf.int"]][[2]], 6)
  vacation[z, 6] = paste("[", inf, ", ", sup , "]", sep = "")
  
  z = z + 1
}

write.table(busy, file=paste(directory, "out/", "busyPeriod_statistic.csv", sep=""), sep=",")
write.table(vacation, file=paste(directory, "out/", "vacationPeriod_statistic.csv", sep=""), sep=",")


