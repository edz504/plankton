library(randomForest)

wd.top <- "C:/Users/edz504/Documents/Data Science Projects/Kaggle/plankton"
wd.train <- paste(wd.top, "/train", sep="")

load("training_pixelandallEBfeat.RData")

start.time <- Sys.time()
rf_model <- randomForest(x=train.data.scaled,
    y=labels, keep.forest=TRUE)
end.time <- Sys.time()
print(end.time - start.time)
# note: with 975 features, 30336 (all) training data, 
# 500 trees, time taken = 1.30791 hours
setwd(wd.top)
setwd("supervised")
save(rf_model, file="rfmodel_pixelandallEBfeat.RData")