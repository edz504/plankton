# use EMCluster to do semi-supervised off the bat
# (no voting between SVM and unsupervised)

# unscale training and create all labels (testing on bottom)
load("testing_pixelandallEBfeat.RData") 
# labels is a function here for some reason, overwrite with the saved labels from training
load("training_pixelandallEBfeat.RData")

train.data.unscaled <- sweep(train.data.scaled, 2,
    train.sds, "*")
train.data.unscaled <- sweep(train.data.unscaled, 2,
    train.means, "+")

# make all labels and rbind with testing (unscaled)
# df goes train and then test
all.labels <- c(labels, rep(0, nrow(test.data.1)))
all.data.unscaled <- rbind(train.data.unscaled, test.data.1)

# save and close (memory limitations)
save(all.data.unscaled, all.labels,
    file="full_data_unscaled.RData")

#################
load("full_data_unscaled.RData")
# scale all of the data
all.data.scaled <- scale(all.data.unscaled)

# save and close (memory limitations)
save(all.data.scaled, all.labels,
    file="full_data_scaled.RData")

#################
library(EMCluster)
library(dplyr)
load("full_data_scaled.RData")

## fix newly unscaled full data (this is kind of sketch)
# fix NaNs 
all.data.scaled[which(is.nan(all.data.scaled), arr.ind=TRUE)] <- 0

# fix Inf
inf.inds <- which(all.data.scaled == Inf, arr.ind=TRUE)
all.data.scaled[inf.inds] <- -1
this.max <- max(all.data.scaled[, 1])
all.data.scaled[inf.inds] <- this.max

# fix NA
na.inds <- which(is.na(all.data.scaled), arr.ind=TRUE)
all.data.scaled[na.inds] <- 0


# find class centers from training only
train.w.labels <- data.frame(class=all.labels[1:30336], 
    all.data.scaled[1:30336, ])
class.means <- train.w.labels %>%
    group_by(class) %>%
        summarise_each(funs(mean)) %>%
            select(-class)

#### execute semi-supervised EM

s <- Sys.time()
ret.EM <- init.EM(all.data.scaled, nclass=121,
    lab = all.labels,
    method="em.EM")
e <- Sys.time()
cat("EMCluster init took", s - e, "\n")

s <- Sys.time()
emcl <- emcluster(x=all.data.scaled,   # data
    emobj=ret.EM,               # initialized object
    Mu=class.means,             # centers (priors)
    lab=all.labels) # labels for semi-supervised
e <- Sys.time()
cat("EMCluster alg took", s - e, "\n")

s <- Sys.time()
probs <- e.step(all.data.scaled, emobj=emcl, norm=TRUE)$Gamma
e <- Sys.time()
cat("EMCluster last e-step (prob retrieval) took", 
    s - e, "\n")