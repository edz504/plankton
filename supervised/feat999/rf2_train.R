library(randomForest)
load("training_999.RData")

start.time <- Sys.time()
rf_model <- randomForest(x=train.data.scaled,
    y=labels)
end.time <- Sys.time()
print(end.time - start.time)
# note: with 975 features, 30336 (all) training data, 
# 500 trees, time taken = 1.310144 hours
save(rf_model, file="rfmodel_999.RData")