load("training_pixelandallEBfeat.RData") # for rescaling
save(train.means, train.sds, file="train_stats.RData")

### close 

load("testing_pixelandallEBfeat.RData")
load("train_stats.RData")
# scale testing based on training data
test.data.scaled <- sweep(test.data.1, 2, train.means, "-")
test.data.scaled <- sweep(test.data.scaled, 2, train.sds, "/")

# fix newly scaled testing data
# fix NaNs 
test.data.scaled[which(is.nan(test.data.scaled), arr.ind=TRUE)] <- 0

# fix Inf
inf.inds <- which(test.data.scaled == Inf, arr.ind=TRUE)
test.data.scaled[inf.inds] <- -1
this.max <- max(test.data.scaled[, 1])
test.data.scaled[inf.inds] <- this.max

# fix NA
na.inds <- which(is.na(test.data.scaled), arr.ind=TRUE)
test.data.scaled[na.inds] <- 0

save(test.data.scaled, file="testing_scaled_pixelandallEBfeat.RData")