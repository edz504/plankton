library(h2o)

## Start a local cluster with 6gb RAM
localH2O <- h2o.init(ip = "localhost", 
    port = 54321, 
    startH2O = TRUE, 
    nthreads = -1,
    max_mem_size='6g')

# load the model into this local cluster
top.wd <- "C:/Users/edz504/Documents/Data Science Projects/Kaggle/plankton"
model.path <- paste(top.wd, "/supervised",
    "/DeepLearning_model1/DeepLearning_85272ab43be1bbb5628dfb26365a4c6f", 
    sep="")
model <- h2o.loadModel(localH2O, model.path)

# load the testing data
load("testing_scaled_pixelandallEBfeat.RData")
test_dat <- data.frame(test.data.scaled)

# set up output
setwd(top.wd)
setwd("test")
test.files <- list.files()

setwd(paste(top.wd, "/train", sep=""))
header <- t(data.frame(c("image", list.files())))

# use on testing set given, in chunks of 1000 
# init file first
setwd(top.wd)
write.table(header, file="submission7.csv", quote=FALSE,
    col.names = FALSE, row.names=FALSE, sep=",")

INCR <- 999
i <- 1

start.time2 <- Sys.time()
check.inds <- c()
check.length <- 0
while (TRUE) {
    upper <- min(i + INCR, length(test.files))
    these.inds <- i:upper

    cat("Predicting testing samples ", 
        min(these.inds), " to ", max(these.inds), "...\n", sep="")
    
    ### prediction on these 5 test data
    these_test_dat <- test_dat[these.inds,]
    these_test_h2o <- as.h2o(localH2O, 
        these_test_dat, 
        key = 'these_test_dat')

    h2o_yhat_test <- h2o.predict(model, these_test_h2o)
    df_yhat_test <- as.data.frame(h2o_yhat_test)

    probs <- df_yhat_test[, -1]
    output <- cbind(test.files[these.inds], probs)

    write.table(output, file="submission7.csv", append=TRUE,
        quote=FALSE, col.names = FALSE, row.names=FALSE, sep=",")
    check.length <- check.length + nrow(probs)
    check.inds <- c(check.inds, these.inds)
    if (length(these.inds) < 1000) {
        break
    }

    i <- i + INCR + 1
}
end.time2 <- Sys.time()
print(end.time2 - start.time2)
print(paste("Check indicies?:", all(check.inds == 1:130400)))
print(paste("Check output length?:", check.length == 130400))
# time to predict all with DNN model, 975 feat trained on all:
# X
