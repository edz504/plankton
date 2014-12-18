library(e1071)
library(dplyr)

wd.top <- "C:/Users/edz504/Documents/Data Science Projects/Kaggle/plankton"
wd.train <- paste(wd.top, "/train", sep="")

load("training_30x30_wrRatio.RData")

# fix NaN values, NA values, inf values
train.data.1[which(is.nan(train.data.1), arr.ind=TRUE)] <- 0

inf.inds <- which(train.data.1 == Inf, arr.ind=TRUE)
train.data.1[inf.inds] <- -1
this.max <- max(train.data.1[, 1])
# somewhat arbitrary
train.data.1[inf.inds] <- this.max

start.time <- Sys.time()
svm_select <- svm(train.data.1,
    labels, type='C', kernel='linear',
    probability=TRUE)
end.time <- Sys.time()
print(end.time - start.time)
# note: 12-18 minutes to train on 9045
# note: 2.82 hours to train on 30336 (all)
setwd(wd.top)
save(svm_select, file="svmmodel_all_30x30_wrRatio.RData")