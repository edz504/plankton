library(e1071)
library(dplyr)

wd.top <- "C:/Users/edz504/Documents/Data Science Projects/Kaggle/plankton"
wd.train <- paste(wd.top, "/train", sep="")

load("training_30x30_wrRatio.RData")

# visualize how many of each type
library(ggplot2)
labels.df <- data.frame(labels)
counts <- labels.df %>% group_by(labels) %>% summarise(count=n())
ggplot(counts, aes(x=labels, y=count)) + geom_bar(stat="identity")
ggsave(file="label_counts.png", width=10, height=6)

# take the first N of each kind
labels.df <- cbind(labels.df, seq(1, nrow(labels.df), by=1))
colnames(labels.df) <- c("labels", "ind")

# find the min and max index for each label (runs)
inds.df <- labels.df %>% 
    group_by(labels) %>%
        summarise(first_ind=min(ind), last_ind=max(ind))

# find pre-allocation space (first N -- 2000 is all)
N <- 2000
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

# train svm on our selected data
selected.train <- train.data.1[selected.ind,]
selected.labels <- labels[selected.ind]

start.time <- Sys.time()
svm_select <- svm(selected.train, 
    selected.labels, type='C', kernel='linear',
    probability=TRUE)
end.time <- Sys.time()
print(end.time - start.time)
# note: 12-18 minutes to train on 9045
# note: 2.82 hours to train on 30336 (all)
setwd(wd.top)
save(svm_select, file=paste("svmmodel_N", N ,"_30x30_wrRatio.RData",
    sep=""))

# check on the training sample
start.time2 <- Sys.time()
pred <- predict(svm_select, selected.train)
end.time2 <- Sys.time()
print(end.time2 - start.time2)
# note: 3.5 min to predict on 9045 training

# calculate training error
err <- length(which(selected.labels != pred)) / length(pred)
# 0 :)