
myargs = commandArgs(trailingOnly=TRUE)
data = readLines(myargs[1])


for(i in 1:length(data)) {
  data[i] = data[i]
}

writeLines(data, myargs[2])