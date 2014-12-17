library(EBImage) 
library(e1071)

# count the number of total images
wd.top <- "C:/Users/edz504/Documents/Data Science Projects/Kaggle/plankton"
wd.train <- paste(wd.top, "/train", sep="")
setwd(wd.train)
num.img <- 0
for (class in list.files()) {
    setwd(paste(wd.train, "/", class, sep=""))
    num.img <- num.img + length(list.files())
}

# store the dimensions in a dataframe
train.dim <- data.frame(matrix(nrow=num.img, ncol=3))
colnames(train.dim) <- c("rows", "cols", "class")
# class counter
c <- 1
# img counter
i <- 1
setwd(wd.train)
for (class in list.files()) {
    setwd(paste(wd.train, "/", class, sep=""))
    for (file in list.files()) {
        img <- readImage(file)
        train.dim[i, ] <- c(nrow(img), ncol(img), c)
        i <- i + 1
    }

    c <- c + 1
}

summary(train.dim)

# we are tempted to resize the pictures, but let's check if size 
# is a factor
library(dplyr)
train.dim %>% 
    mutate(size = rows * cols) %>% 
        group_by(class) %>%
            summarise(mean_size=mean(size), max_size=max(size), 
            min_size=min(size), med_size=median(size))


# first attempt ==> resize downwards (will be smaller)
apply(train.dim, FUN=min, 2)
# make them all 30 x 30

train.data.1 <- matrix(nrow = num.img, ncol = 30 * 30)
labels <- rep(NA, num.img)
setwd(wd.train)
c <- 1
i <- 1
for (class in list.files()) {
    setwd(paste(wd.train, "/", class, sep=""))
    for (file in list.files()) {
        img <- readImage(file)
        labels[i] <- c
        train.data.1[i, ] <- as.vector(resize(img, 30, 30))
        i <- i + 1
    }
    c <- c + 1
}

# make labels a factor
labels <- factor(labels)

# save the training and testing data
setwd(wd.top)
save(train.data.1, labels, file="training_30x30.RData")

# visualize how many of each type
library(ggplot2)
labels.df <- data.frame(labels)
counts <- labels.df %>% group_by(labels) %>% summarise(count=n())
ggplot(counts, aes(x=labels, y=count)) + geom_bar(stat="identity")
ggsave(file="label_counts.png", width=10, height=6)

# we can get 7 different labels using rows 1-2000, so let's try
# on that just to see what it looks like (and to time and get an idea for it)
svm_model <- svm(train.data.1[1:2000,], labels[1:2000], type='C', kernel='linear',
    probability=TRUE)
# note, takes roughly 30 sec

pred <- predict(svm_model, train.data.1[1:2000,],
    probability = TRUE)
attr(pred, "probabilities")[1:10,]

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

# pre-allocate (9045 here)
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
    selected.labels, type='C', kernel='linear')
end.time <- Sys.time()
print(paste("Time taken for ", prealloc, " training samples = ",
    (end.time - start.time), sep=""))
# note: 12 minutes to train on 9045

# check on the training sample
start.time2 <- Sys.time()
pred <- predict(svm_select, selected.train)
end.time2 <- Sys.time()
print(end.time2 - start.time2)
# note: 3.5 min to predict on 9045 training

# calculate training error
err <- length(which(selected.labels != pred)) / length(pred)
# 0 :)

# prepare the testing set
setwd("test")
test.data.1 <- matrix(nrow = length(list.files()), 
    ncol = 30 * 30)
c <- 1
i <- 1
for (img in list.files()) {
    if (i %% 10000 == 0) {
        print(i)
    }
    test.data.1[i, ] <- as.vector(resize(readImage(img), 30, 30))
    i <- i + 1
}

setwd(wd.top)
save(test.data.1, file="testing_30x30.RData")


# use on testing set given
start.time3 <- Sys.time()
pred <- predict(svm_select, test.data.1,
    probability = TRUE)
end.time3 <- Sys.time()
print(end.time3 - start.time3)

attr(pred, "probabilities")[1:10,]



# resize upwards