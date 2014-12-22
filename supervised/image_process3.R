# using some features from EBImage package
# INSTEAD of pixel hues
library(EBImage) 

# count the number of total images
wd.top <- "C:/Users/edz504/Documents/Data Science Projects/Kaggle/plankton"
wd.train <- paste(wd.top, "/train", sep="")
setwd(wd.train)
num.img <- 0
for (class in list.files()) {
    setwd(paste(wd.train, "/", class, sep=""))
    num.img <- num.img + length(list.files())
}

train.data.1 <- matrix(nrow = num.img, ncol = 13)
labels <- rep(NA, num.img)
setwd(wd.train)
c <- 1
i <- 1
for (class in list.files()) {
    setwd(paste(wd.train, "/", class, sep=""))
    for (file in list.files()) {
        img <- readImage(file)
        # adaptive threshold
        img2 <- thresh(img)
        segment.labels <- bwlabel(img2)

        ### ratio feature
        # find largest non-background object
        obj.label <- which.max(table(as.vector(segment.labels))[-1])
        if (length(obj.label) == 0) {
            ratio <- 0
        } else {
            obj.inds <- which(segment.labels == obj.label, 
            arr.ind=TRUE)
            ratio <- diff(range(obj.inds[,2])) / diff(range(obj.inds[,1]))
        }
        train.data.1[i, 1] <- ratio

        ### EBImage Features

        # shape features
        shape.feat <- computeFeatures.shape(segment.labels)
        if (max(segment.labels) == 0) {
            train.data.1[i, 2:7] <- rep(NA, 6)
        } else {
            train.data.1[i, 2:7] <- shape.feat[which.max(shape.feat[,1]),]
        }

        # resized shape features
        img.resize <- resize(img, 30, 30)
        img.resize.thresh <- thresh(img.resize)
        resize.segment.labels <- bwlabel(img.resize.thresh)
        shape.feat.resize <- computeFeatures.shape(resize.segment.labels)
        if (max(resize.segment.labels) == 0) {
            train.data.1[i, 8:13] <- rep(NA, 6)
        } else {
            train.data.1[i, 8:13] <- shape.feat.resize[which.max(shape.feat.resize[,1]),]
        }
        labels[i] <- c
        i <- i + 1
    }

    c <- c + 1
    cat("Finished class '", class, "'\n", sep='')
}

# make labels a factor
labels <- factor(labels)

# fix NaNs, and Inf values
train.data.1[which(is.nan(train.data.1), arr.ind=TRUE)] <- 0

inf.inds <- which(train.data.1 == Inf, arr.ind=TRUE)
train.data.1[inf.inds] <- -1
this.max <- max(train.data.1[, 1])
# somewhat arbitrary
train.data.1[inf.inds] <- this.max

# scale and save the training data
train.data.scaled <- scale(train.data.1)
setwd(wd.top)
save(train.data.scaled, labels, file="training_EBfeat.RData")


# and testing data
setwd("test")
test.data.1 <- matrix(nrow = length(list.files()), 
    ncol = 13)
c <- 1
i <- 1
for (file in list.files()) {
    if (i %% 10000 == 0) {
        cat(i, "\n")
    }
    img <- readImage(file)
    # adaptive threshold
    img2 <- thresh(img)
    segment.labels <- bwlabel(img2)

    ### ratio feature
    # find largest non-background object
    obj.label <- which.max(table(as.vector(segment.labels))[-1])
    if (length(obj.label) == 0) {
        ratio <- 0
    } else {
        obj.inds <- which(segment.labels == obj.label, 
        arr.ind=TRUE)
        ratio <- diff(range(obj.inds[,2])) / diff(range(obj.inds[,1]))
    }
    test.data.1[i, 1] <- ratio

    ### EBImage Features
    shape.feat <- computeFeatures.shape(segment.labels)
    if (max(segment.labels) == 0) {
        test.data.1[i, 2:7] <- rep(NA, 6)
    } else {
        test.data.1[i, 2:7] <- shape.feat[which.max(shape.feat[,1]),]
    }

    img.resize <- resize(img, 30, 30)
    img.resize.thresh <- thresh(img.resize)
    resize.segment.labels <- bwlabel(img.resize.thresh)
    shape.feat.resize <- computeFeatures.shape(resize.segment.labels)
    if (max(resize.segment.labels) == 0) {
        test.data.1[i, 8:13] <- rep(NA, 6)
    } else {
        test.data.1[i, 8:13] <- shape.feat.resize[which.max(shape.feat.resize[,1]),]
    }
    i <- i + 1
}

# fix NaNs, and Inf values
test.data.1[which(is.nan(test.data.1), arr.ind=TRUE)] <- 0

inf.inds <- which(test.data.1 == Inf, arr.ind=TRUE)
test.data.1[inf.inds] <- -1
this.max <- max(test.data.1[, 1])
test.data.1[inf.inds] <- this.max

# scale and save testing data
test.data.scaled <- scale(test.data.1)
setwd(wd.top)
save(test.data.scaled, file="testing_EBfeat.RData")
