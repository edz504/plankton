library(h2o)
library(mlbench)

## Start a local cluster with 6GB RAM
localH2O <- h2o.init(ip = "localhost", 
    port = 54321, 
    startH2O = TRUE, 
    nthreads = -1,
    max_mem_size='6g')

data(BreastCancer)
## Convert Breast Cancer into H2O
dat <- BreastCancer[, -1]  # remove the ID column

# split into training and testing
train.inds <- sample(nrow(BreastCancer), 490)
train_dat <- dat[train.inds, ]
test_dat <- dat[-train.inds, ]

test_labels <- test_dat[, ncol(test_dat)]
test_dat <- test_dat[, 1:(ncol(test_dat) - 1)]

train_h2o <- as.h2o(localH2O, train_dat, key = 'train_dat')
test_h2o <- as.h2o(localH2O, test_dat, key = 'test_dat')

model <- h2o.deeplearning(
    x = 1:9,  # column numbers for predictors
    y = 10,   # column number for label
    data = train_h2o, # data in H2O format
    activation = "TanhWithDropout", # or 'Tanh'
    input_dropout_ratio = 0.2, # % of inputs dropout
    hidden_dropout_ratios = c(0.5,0.5,0.5), # % for nodes dropout
    balance_classes = TRUE, 
    hidden = c(50,50,50), # three layers of 50 nodes
    epochs = 100) # max. no. of epochs

### try to close and reload model + h2o
# save(model, localH2O, file="plankton_trial_saving.RData")
top.wd <- "C:/Users/edz504/Documents/Data Science Projects/Kaggle/plankton/"
model.path <- paste(top.wd, "/supervised",
    "/DeepLearning_aaa64d1cb7f716daaa78fd8806764dfd", 
    sep="")
h2o.saveModel(model)

#################################
library(h2o)
library(mlbench)

data(BreastCancer)
dat <- BreastCancer[, -1] 
train.inds <- sample(nrow(BreastCancer), 490)
test_dat <- dat[-train.inds, ]
test_dat <- test_dat[, 1:(ncol(test_dat) - 1)]

## Start a local cluster with 6GB RAM
localH2O <- h2o.init(ip = "localhost", 
    port = 54321, 
    startH2O = TRUE, 
    nthreads = -1,
    max_mem_size='6g')

model <- h2o.loadModel(localH2O, model.path)
test_h2o <- as.h2o(localH2O, test_dat, key = 'test_dat')



h2o_yhat_test <- h2o.predict(model, test_h2o)
 
## Converting H2O format into data frame
df_yhat_test <- as.data.frame(h2o_yhat_test)

h2o.shutdown(localH2O)