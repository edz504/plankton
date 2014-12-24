library(h2o)
library(mlbench)

## Start a local cluster with 2GB RAM
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


## Using the DNN model for predictions
h2o_yhat_test <- h2o.predict(model, test_h2o)
 
## Converting H2O format into data frame
df_yhat_test <- as.data.frame(h2o_yhat_test)

h2o.shutdown(localH2O)