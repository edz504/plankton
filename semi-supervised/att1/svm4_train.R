library(e1071)
library(dplyr)

wd.top <- "C:/Users/edz504/Documents/Data Science Projects/Kaggle/plankton"
wd.train <- paste(wd.top, "/train", sep="")

load("training_pixelandallEBfeat.RData")

start.time <- Sys.time()
svm_select <- svm(train.data.scaled,
    labels, type='C', kernel='radial',
    probability=TRUE)
end.time <- Sys.time()
print(end.time - start.time)
# note: with 975 features, 30336 (all) training data, 
# and an RBF kernel, time taken = 3.8 hours
setwd(wd.top)
save(svm_select, file="svmmodel_pixelandallEBfeat.RData")