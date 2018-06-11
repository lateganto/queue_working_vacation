data = read.csv("Applications/omnetpp-5.3/samples/queueinglib3/results/asd.csv", sep=",", stringsAsFactors = FALSE);

d = data[data$type=="vector",]
d = d$vecvalue
d = as.list(d)

list = list()
for(i in 1:length(d)) {
  list[[length(list) + 1]] = as.numeric(unlist(strsplit(d[[i]], " ")))
}

lengths(list)
