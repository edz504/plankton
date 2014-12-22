library(e1071)
library(dplyr)

wd.top <- "C:/Users/edz504/Documents/Data Science Projects/Kaggle/plankton"
wd.train <- paste(wd.top, "/train", sep="")

load("training_30x30.RData")
labels.df <- data.frame(labels)
counts <- labels.df %>% group_by(labels) %>% summarise(count=n())
# take the first N of each kind
labels.df <- cbind(labels.df, seq(1, nrow(labels.df), by=1))
colnames(labels.df) <- c("labels", "ind")

# find the min and max index for each label (runs)
inds.df <- labels.df %>% 
    group_by(labels) %>%
        summarise(first_ind=min(ind), last_ind=max(ind))

# find pre-allocation space (first N)
N <- 100
prealloc <- 0
for (i in seq(1, nrow(counts), by=1)) {
    if (counts$count[i] > N) {
        prealloc <- prealloc + N
    } else {
        prealloc <- prealloc + counts$count[i]
    }
}

# pre-allocate (9045 here again)
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

# train using semi-supervised GMM
library(Rmixmod)

# semi-supervised, using both testing and training
load("testing_30x30.RData")

selected.train <- train.data.1[selected.ind,]
selected.labels <- labels[selected.ind]

F <- 10 # 1/10 of testing set used
M <- max(prealloc, nrow(test.data.1) / F)
# ^ we want at least as many unsupervised as supervised

selected.data <- data.frame(rbind(selected.train,
    test.data.1[sample(nrow(test.data.1), M), ]))
selected.data.labels <- c(selected.labels, rep(NA, M))
# the last M are NA because they don't have labels, but
# we have the known labels for the training set

start.time <- Sys.time()
mixmod.semi <- mixmodCluster(selected.data, nbCluster=121,
    knownLabels=selected.data.labels)
end.time <- Sys.time()
print(end.time - start.time)
# 27 min to train
save(mixmod.semi, file="mixmodel_N100_F10_30x30.RData")