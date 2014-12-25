library(h2o)

## Start a local cluster with 6gb RAM
localH2O <- h2o.init(ip = "localhost", 
    port = 54321, 
    startH2O = TRUE, 
    nthreads = -1,
    max_mem_size='6g')

load("training_pixelandallEBfeat.RData")
# add labels into the df, and then push it into the h2o instance
train_dat <- data.frame(class=labels, train.data.scaled)
train_h2o <- as.h2o(localH2O, train_dat, key = 'train_dat')

# remove extra variables to preserve memory
rm(train.means)
rm(train.sds)
rm(train.data.scaled)
rm(labels)
rm(train_dat)

# train the DNN model
start.time <- Sys.time()
print(start.time)
model <- h2o.deeplearning(
    x = 2:976,  # column numbers for predictors
    y = 1,   # column number for label
    data = train_h2o, # data in H2O format
    activation = "TanhWithDropout", # or 'Tanh'
    input_dropout_ratio = 0.2, # % of inputs dropout
    hidden_dropout_ratios = c(0.5,0.5,0.5), # % for nodes dropout
    balance_classes = TRUE, 
    hidden = c(50,50,50), # three layers of 50 nodes
    epochs = 100) # max. no. of epochs
end.time <- Sys.time()
print(end.time - start.time)
# took 1.748764 hours (the first time)
# took 1.72257 hours (the second time)


# save(model, file="h2o_DNN_model_3x50_100.RData")
# ^^^^^^^^ nooooo wrongggg
# SAVE IT THE RIGHT WAYYY
top.wd <- "C:/Users/edz504/Documents/Data Science Projects/Kaggle/plankton"
model.path <- paste(top.wd, "/supervised",
    "/DeepLearning_model1", 
    sep="")
h2o.saveModel(model, model.path)

h2o.shutdown(localH2O)