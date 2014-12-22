library(e1071)
library(dplyr)

wd.top <- "C:/Users/edz504/Documents/Data Science Projects/Kaggle/plankton"
wd.train <- paste(wd.top, "/train", sep="")

load("training_EBfeat.RData")

start.time <- Sys.time()
svm_select <- svm(train.data.scaled,
    labels, type='C', kernel='linear',
    probability=TRUE)
end.time <- Sys.time()
print(end.time - start.time)
# note: 12-18 minutes to train on 9045
# note: 2.82 hours to train on 30336 (all)
setwd(wd.top)
save(svm_select, file="svmmodel_EBfeat.RData")