library(e1071)
load("training_999.RData")

start.time <- Sys.time()
print(start.time)
svm_model <- svm(train.data.scaled,
    labels, type='C', kernel='radial',
    probability=TRUE)
end.time <- Sys.time()
print(end.time - start.time)
# note: with 999 features, 30336 (all) training data, 
# and an RBF kernel, time taken 
# = ???
save(svm_model, file="svmmodel_999.RData")