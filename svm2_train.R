library(e1071)
library(dplyr)

wd.top <- "C:/Users/edz504/Documents/Data Science Projects/Kaggle/plankton"
wd.train <- paste(wd.top, "/train", sep="")

load("training_30x30_wrRatio.RData")

# fix NaN values
train.data.1[12686, 1] <- 0
train.data.1[13014, 1] <- 0

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