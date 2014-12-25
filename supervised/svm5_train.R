library(e1071)
library(dplyr)

wd.top <- "C:/Users/edz504/Documents/Data Science Projects/Kaggle/plankton"
wd.train <- paste(wd.top, "/train", sep="")

load("training_1950_pixelandallEBfeat.RData")

start.time <- Sys.time()
svm_select <- svm(train.data.scaled,
    labels, type='C', kernel='radial',
    probability=TRUE)
end.time <- Sys.time()
print(end.time - start.time)
# note: with 975 features, 30336 (all) training data, 
# and an RBF kernel, time taken = 3.8 hours

# note: with 1950 features, all trainig data, and RBF kernel,
# time taken = 
save(svm_select, file="svmmodel_1950_pixelandallEBfeat.RData")