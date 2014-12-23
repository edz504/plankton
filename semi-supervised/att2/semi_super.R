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

N <- 9
# first Q from each class
library(dplyr)
labels.df <- data.frame(labels)
counts <- labels.df %>% group_by(labels) %>% summarise(count=n())
labels.df <- cbind(labels.df, seq(1, nrow(labels.df), by=1))
colnames(labels.df) <- c("labels", "ind")
inds.df <- labels.df %>% 
    group_by(labels) %>%
        summarise(first_ind=min(ind), last_ind=max(ind))

prealloc <- 0
for (i in seq(1, nrow(counts), by=1)) {
    if (counts$count[i] > N) {
        prealloc <- prealloc + N
    } else {
        prealloc <- prealloc + counts$count[i]
    }
}

# pre-allocate
selected.ind <- rep(NA, prealloc)
j <- 1
for (i in seq(1, nrow(inds.df), by=1)) {
    if (inds.df$last_ind[i] - inds.df$first_ind[i] > N) {
        these.inds <- seq(inds.df$first_ind[i],
            inds.df$first_ind[i] + (N - 1), by=1)
    } else {
        these.inds <- seq(inds.df$first_ind[i],
            inds.df$last_ind[i], by=1)
    }

    selected.ind[j: (j + length(these.inds) - 1)] <- these.inds
    j <- j + length(these.inds)
}



M.known <- 15
M.unknown <- 1000
T <- 20 # number of bagging samples

# subscript errors are because there are labels in c (known labels) that are greater than the number of classes known

# unsure what this error is though:
# "Error in li[i, ] = as.numeric(c(colnames(kkdist)[py], rownames(kkdist)[px])) : 
#   replacement has length zero"

for (j in 1:T) {
    start.time <- Sys.time()
    bag.train.ind <- selected.ind
    bag.test.ind <- sample(nrow(test.all.scaled), M.unknown)

    a <- test.all.scaled[bag.test.ind,]
    b <- train.all.scaled[bag.train.ind,]
    c <- labels[bag.train.ind]
    B <- matrix(0, nrow=length(c), ncol=975)
    B[seq(1, nrow(b), by=1), c] <- 1

    mss <- semisupervised(
        X = a,
        knowns = b,
        B = B,
        class = c, 
        k = 121)
    
    end.time <- Sys.time()
    cat("Finished bag ", i, ", time = \n")
    print(end.time - start.time)
    probs <- mss$tij
    save(probs, 
        file=paste("bgmm_probs_975feat_bag", 
            i, ".RData", sep=""))
}


######################

# damn bgmm 
# trying Rmixmod 
load("split_data_semi.RData")
library(Rmixmod)

M.known <- 15
M.unknown <- 300
T <- 20 # number of bagging samples

for (j in 1:T) {
    start.time <- Sys.time()
    bag.train.ind <- sample(nrow(train.all.scaled), M.known)
    bag.test.ind <- sample(nrow(test.all.scaled), M.unknown)

    a <- test.all.scaled[bag.test.ind,]
    b <- train.all.scaled[bag.train.ind,]
    c <- labels[bag.train.ind]

    X <- rbind(b, a)
    these.labels <- c(c, rep(0, M.unknown))

    mmc <- mixmodCluster(data=data.frame(X), nbCluster=121,
        knownLabels=these.labels)

    end.time <- Sys.time()
    cat("Finished bag ", i, ", time = \n")
    print(end.time - start.time)
    probs <- mss$tij
    save(probs, 
        file=paste("bgmm_probs_975feat_bag", 
            i, ".RData", sep=""))
}
