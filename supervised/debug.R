bad.list <- as.vector(read.csv(file="testfiles.csv", 
    header=TRUE)[,1])
setwd("test")
good.list <- list.files()