# same as original image processing (30x30) but with
# width-to-height ratio via segmentation as suggested
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


# one extra feature, width-to-length ratio
train.data.1 <- matrix(nrow = num.img, ncol = 1 + 30 * 30)
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
        train.data.1[i, 2:901] <- as.vector(resize(img, 30, 30))
        labels[i] <- c
        i <- i + 1
    }
    c <- c + 1
    cat("Finished class '", class, "'\n", sep='')
}

# make labels a factor
labels <- factor(labels)

# save the training and testing data
setwd(wd.top)
save(train.data.1, labels, file="training_30x30_wrRatio.RData")


# prepare the testing set
setwd("test")
test.data.1 <- matrix(nrow = length(list.files()), 
    ncol = 1 + 30 * 30)
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
    # find largest non-background object
    obj.label <- which.max(table(as.vector(segment.labels))[-1])
    if (length(obj.label) == 0) {
            ratio <- 0
    } else {
        obj.inds <- which(segment.labels == obj.label, 
            arr.ind=TRUE)
        ratio <- diff(range(obj.inds[,2])) / diff(range(obj.inds[,1]))
        if (ratio == Inf | is.nan(ratio)) {
            ratio <- 0
        }
    }
    test.data.1[i, 1] <- ratio
    test.data.1[i, 2:901] <- as.vector(resize(img, 30, 30))
    i <- i + 1
}

setwd(wd.top)
save(test.data.1, file="testing_30x30_wrRatio.RData")
