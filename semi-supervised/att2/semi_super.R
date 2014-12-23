# library(dplyr)
# load("full_data_scaled.RData")

# ## fix newly unscaled full data
# # fix NaNs 
# all.data.scaled[which(is.nan(all.data.scaled), arr.ind=TRUE)] <- 0

# # fix Inf
# inf.inds <- which(all.data.scaled == Inf, arr.ind=TRUE)
# all.data.scaled[inf.inds] <- -1
# this.max <- max(all.data.scaled[, 1])
# all.data.scaled[inf.inds] <- this.max

# # fix NA
# na.inds <- which(is.na(all.data.scaled), arr.ind=TRUE)
# all.data.scaled[na.inds] <- 0

# test.all.scaled <- all.data.scaled[30337:nrow(all.data.scaled), ]
# train.all.scaled <- all.data.scaled[1:30336,]
# labels <- all.labels[1:30336]
# save(test.all.scaled, train.all.scaled, labels,
#     file="split_data_semi.RData")

### using bgmm (no weird initialization)
library(bgmm)
load("split_data_semi.RData")
M.known <- 5000
M.unknown <- 20000
T <- 10 # number of bagging samples

for (i in 1:T) {
    start.time <- Sys.time()

    bag.train.ind <- sample(nrow(train.all.scaled), M.known)
    bag.test.ind <- sample(nrow(test.all.scaled), M.unknown)

    mod.semi.sup <- semisupervised(
        X = test.all.scaled[bag.test.ind,],
        knowns = train.all.scaled[bag.train.ind,],
        class = labels[bag.train.ind])
    end.time <- Sys.time()
    cat("Finished bag ", i, ", time = \n")
    print(end.time - start.time)
    probs <- mod.semi.sup$tij
    save(probs, 
        file=paste("bgmm_probs_975feat_bag", 
            i, ".RData", sep=""))
}

