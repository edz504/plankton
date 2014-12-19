# using some features from EBImage package
# AND of pixel hues
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


# features;
# 1 : WR ratio
# 6 : shape features
# 6 : resized shape features
# 5 : moment features
# 5 : resized moment features
# 26 : haralick features
# 26 : resized haralick features

train.data.1 <- matrix(nrow = num.img, ncol = 975)
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
            max.area.ind <- 1
            train.data.1[i, 2:7] <- rep(NA, 6)
        } else {
            max.area.ind <- which.max(shape.feat[,1])
            train.data.1[i, 2:7] <- shape.feat[max.area.ind,]
        }

        # resized shape features
        img.resize <- resize(img, 30, 30)
        img.resize.thresh <- thresh(img.resize)
        resize.segment.labels <- bwlabel(img.resize.thresh)
        shape.feat.resize <- computeFeatures.shape(resize.segment.labels)
        if (max(resize.segment.labels) == 0) {
            max.area.ind.resize <- 1
            train.data.1[i, 8:13] <- rep(NA, 6)
        } else {
            max.area.ind.resize <- which.max(shape.feat.resize[,1])
            train.data.1[i, 8:13] <- shape.feat.resize[max.area.ind.resize,]
        }

        # moment features
        mom.feat <- computeFeatures.moment(segment.labels)
        if (length(mom.feat) == 0) {
            train.data.1[i, 14:18] <- rep(NA, 5)
        } else {
            train.data.1[i, 14:18] <- mom.feat[max.area.ind,]
        }

        # resized moment features
        mom.feat.resize <- computeFeatures.moment(resize.segment.labels)
        if (length(mom.feat.resize) == 0) {
            train.data.1[i, 19:23] <- rep(NA, 5)
        } else {
            train.data.1[i, 19:23] <- mom.feat.resize[max.area.ind.resize,]
        }

        # haralick features
        haral.feat <- tryCatch({
            return(computeFeatures.haralick(img, segment.labels)[1,])
        }, warning = function(w) {
        }, error = function(e) {
            return(rep(NA, 26))
        }, finally = {
        })
        train.data.1[i, 24:49] <- haral.feat

        # resized haralick features
        haral.feat.resize <- tryCatch({
            return(computeFeatures.haralick(img.resize,
                resize.segment.labels)[1,])
        }, warning = function(w) {
        }, error = function(e) {
            return(rep(NA, 26))
        }, finally = {
        })

        train.data.1[i, 50:75] <- haral.feat.resize

        # pixel features
        train.data.1[i, 76:975] <- as.vector(img.resize)

        labels[i] <- c
        i <- i + 1
    }

    c <- c + 1
    cat("Finished class '", class, "'\n", sep='')
}

# make labels a factor
labels <- factor(labels)

# fix NaNs 
train.data.1[which(is.nan(train.data.1), arr.ind=TRUE)] <- 0

# fix Inf
inf.inds <- which(train.data.1 == Inf, arr.ind=TRUE)
train.data.1[inf.inds] <- -1
this.max <- max(train.data.1[, 1])
train.data.1[inf.inds] <- this.max

# fix NA
na.inds <- which(is.na(train.data.1), arr.ind=TRUE)
train.data.1[na.inds] <- 0

# scale and save the training data
train.data.scaled <- scale(train.data.1)
setwd(wd.top)
save(train.data.scaled, labels, file="training_pixelandallEBfeat.RData")




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
