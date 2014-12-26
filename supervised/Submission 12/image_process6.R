library(EBImage) 
library(moments)

wd.start <- getwd()

# count the number of total images
wd.top <- "C:/Users/edz504/Documents/Data Science Projects/Kaggle/plankton"
wd.train <- paste(wd.top, "/train", sep="")
setwd(wd.train)
num.img <- 0
for (class in list.files()) {
    setwd(paste(wd.train, "/", class, sep=""))
    num.img <- num.img + length(list.files())
}


# features:
(# 1 : WR ratio
# 6 : shape features (EBImage)
# 6 : resized shape features
# 5 : moment features
# 5 : resized moment features
# 26 : haralick features
# 26 : resized haralick features)
# 900 : resized pixel vals

# 1 : shape factor, (4 * PI * Area) / (Perimeter^2)
# 1 : roundness, (Perimeter^2) / 4 * PI * Area)
# 1 : ECD, 2*sqrt(Area/PI)
# 1 : thread length, (Perimeter + sqrt(Perimeter^2-16*Area))/4
# 1 : thread width, (Perimeter - sqrt(Perimeter^2-16*Area))/4
# 7 : mean, skew, kurtosis, stdev, mode, median, sum of grayscale

# same 12 (resized)

### (24 new features)


train.data <- matrix(nrow = num.img, ncol = 999)
labels <- rep(NA, num.img)
setwd(wd.train)
c <- 1
i <- 1

start.time <- Sys.time()
print(start.time)
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
        train.data[i, 1] <- ratio

        ### EBImage Features

        # shape features
        shape.feat <- computeFeatures.shape(segment.labels)
        if (max(segment.labels) == 0) {
            max.area.ind <- 1
            train.data[i, 2:7] <- rep(NA, 6)
        } else {
            max.area.ind <- which.max(shape.feat[,1])
            train.data[i, 2:7] <- shape.feat[max.area.ind,]
        }

        # resized shape features
        img.resize <- resize(img, 30, 30)
        img.resize.thresh <- thresh(img.resize)
        resize.segment.labels <- bwlabel(img.resize.thresh)
        shape.feat.resize <- computeFeatures.shape(resize.segment.labels)
        if (max(resize.segment.labels) == 0) {
            max.area.ind.resize <- 1
            train.data[i, 8:13] <- rep(NA, 6)
        } else {
            max.area.ind.resize <- which.max(shape.feat.resize[,1])
            train.data[i, 8:13] <- shape.feat.resize[max.area.ind.resize,]
        }

        # moment features
        mom.feat <- computeFeatures.moment(segment.labels)
        if (length(mom.feat) == 0) {
            train.data[i, 14:18] <- rep(NA, 5)
        } else {
            train.data[i, 14:18] <- mom.feat[max.area.ind,]
        }

        # resized moment features
        mom.feat.resize <- computeFeatures.moment(resize.segment.labels)
        if (length(mom.feat.resize) == 0) {
            train.data[i, 19:23] <- rep(NA, 5)
        } else {
            train.data[i, 19:23] <- mom.feat.resize[max.area.ind.resize,]
        }

        # haralick features
        haral.feat <- tryCatch({
            computeFeatures.haralick(img, segment.labels)[1,]
        }, warning = function(w) {
        }, error = function(e) {
            rep(NA, 26)
        }, finally = {
        })
        if (length(haral.feat) == 0) {
            haral.feat <- rep(NA, 26)
        }

        train.data[i, 24:49] <- haral.feat

        # resized haralick features
        haral.feat.resize <- tryCatch({
            computeFeatures.haralick(img.resize,
                resize.segment.labels)[1,]
        }, warning = function(w) {
        }, error = function(e) {
            rep(NA, 26)
        }, finally = {
        })
        if (length(haral.feat.resize) == 0) {
            haral.feat.resize <- rep(NA, 26)
        }

        train.data[i, 50:75] <- haral.feat.resize

        # pixel features
        train.data[i, 76:975] <- as.vector(img.resize)

        # new 12 features
        a <- shape.feat[max.area.ind,][1]
        if (length(a) == 0) {
            a <- 0
        }

        p <- shape.feat[max.area.ind,][2]
        if (length(p) == 0) {
            p <- 0
        }

        # shape factor, (4 * PI * Area) / (Perimeter^2)
        train.data[i, 976] <- (4 * pi * a) / (p^2)

        # roundness, (Perimeter^2) / 4 * PI * Area)
        train.data[i, 977] <- (p^2) / (4 * pi * a)

        # ECD, 2*sqrt(Area/PI)
        train.data[i, 978] <- 2 * sqrt(a / pi)

        # thread length, (Perimeter + sqrt(Perimeter^2-16*Area))/4
        train.data[i, 979] <- (p + sqrt(p^2 - 16 * a)) / 4

        # thread width, (Perimeter - sqrt(Perimeter^2-16*Area))/4
        train.data[i, 980] <- (p - sqrt(p^2 - 16 * a)) / 4

        # 7 : mean, skew, kurtosis, stdev, mode, median, sum of grayscale
        train.data[i, 981] <- mean(img)
        train.data[i, 982] <- skewness(as.vector(img))
        train.data[i, 983] <- kurtosis(as.vector(img))
        train.data[i, 984] <- sd(img)
        train.data[i, 985] <- as.numeric(rownames(table(img))[which.max(table(img))])
        train.data[i, 986] <- median(img)
        train.data[i, 987] <- sum(img)


        # new 12 features (resized)
        a <- shape.feat.resize[max.area.ind.resize,][1]
        if (length(a) == 0) {
            a <- 0
        }
        p <- shape.feat.resize[max.area.ind.resize,][2]
        if (length(p) == 0) {
            p <- 0
        }
        # shape factor, (4 * PI * Area) / (Perimeter^2)
        train.data[i, 988] <- (4 * pi * a) / (p^2)

        # roundness, (Perimeter^2) / 4 * PI * Area)
        train.data[i, 989] <- (p^2) / (4 * pi * a)

        # ECD, 2*sqrt(Area/PI)
        train.data[i, 990] <- 2 * sqrt(a / pi)

        # thread length, (Perimeter + sqrt(Perimeter^2-16*Area))/4
        train.data[i, 991] <- (p + sqrt(p^2 - 16 * a)) / 4

        # thread width, (Perimeter - sqrt(Perimeter^2-16*Area))/4
        train.data[i, 992] <- (p - sqrt(p^2 - 16 * a)) / 4

        # 7 : mean, skew, kurtosis, stdev, mode, median, sum of grayscale
        train.data[i, 993] <- mean(img.resize)
        train.data[i, 994] <- skewness(as.vector(img.resize))
        train.data[i, 995] <- kurtosis(as.vector(img.resize))
        train.data[i, 996] <- sd(img.resize)
        train.data[i, 997] <- as.numeric(rownames(table(img.resize))[which.max(table(img.resize))])
        train.data[i, 998] <- median(img.resize)
        train.data[i, 999] <- sum(img.resize)

        labels[i] <- c
        i <- i + 1
    }

    c <- c + 1
    cat("Finished class '", class, "'\n", sep='')
}
end.time <- Sys.time()
print(end.time - start.time)
# 1.135214 hours to process 999 features in training

# make labels a factor
labels <- factor(labels)

# fix NaNs 
train.data[which(is.nan(train.data), arr.ind=TRUE)] <- 0

# fix Inf
inf.inds <- which(train.data == Inf, arr.ind=TRUE)
train.data[inf.inds] <- -1
this.max <- max(train.data[, 1])
train.data[inf.inds] <- this.max

# fix NA
na.inds <- which(is.na(train.data), arr.ind=TRUE)
train.data[na.inds] <- 0

train.data.scaled <- scale(train.data)
# NaNs induced by zero variance in feature
train.data.scaled[which(is.nan(train.data.scaled),
    arr.ind=TRUE)] <- 0


train.means <- apply(train.data.scaled, FUN=mean, 2)
train.sds <- apply(train.data.scaled, FUN=sd, 2)


setwd(wd.start)
save(train.data.scaled, labels, train.means, train.sds,
    file="training_999.RData")

save(train.means, train.sds, file="training_999_scaling_stats.RData")


###############################
###############################
### testing data ###
library(EBImage)
library(moments)

wd.start <- getwd()
# count the number of total images
wd.top <- "C:/Users/edz504/Documents/Data Science Projects/Kaggle/plankton"
setwd(wd.top)
setwd("test")
test.data <- matrix(nrow = length(list.files()), 
    ncol = 999)
c <- 1
i <- 1

start.time <- Sys.time()
for (file in list.files()) {
    if (i %% 1000 == 0) {
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
    test.data[i, 1] <- ratio

    ### EBImage Features

    # shape features
    shape.feat <- computeFeatures.shape(segment.labels)
    if (max(segment.labels) == 0) {
        max.area.ind <- 1
        test.data[i, 2:7] <- rep(NA, 6)
    } else {
        max.area.ind <- which.max(shape.feat[,1])
        test.data[i, 2:7] <- shape.feat[max.area.ind,]
    }

    # resized shape features
    img.resize <- resize(img, 30, 30)
    img.resize.thresh <- thresh(img.resize)
    resize.segment.labels <- bwlabel(img.resize.thresh)
    shape.feat.resize <- computeFeatures.shape(resize.segment.labels)
    if (max(resize.segment.labels) == 0) {
        max.area.ind.resize <- 1
        test.data[i, 8:13] <- rep(NA, 6)
    } else {
        max.area.ind.resize <- which.max(shape.feat.resize[,1])
        test.data[i, 8:13] <- shape.feat.resize[max.area.ind.resize,]
    }

    # moment features
    mom.feat <- computeFeatures.moment(segment.labels)
    if (length(mom.feat) == 0) {
        test.data[i, 14:18] <- rep(NA, 5)
    } else {
        test.data[i, 14:18] <- mom.feat[max.area.ind,]
    }

    # resized moment features
    mom.feat.resize <- computeFeatures.moment(resize.segment.labels)
    if (length(mom.feat.resize) == 0) {
        test.data[i, 19:23] <- rep(NA, 5)
    } else {
        test.data[i, 19:23] <- mom.feat.resize[max.area.ind.resize,]
    }

    # haralick features
    haral.feat <- tryCatch({
        computeFeatures.haralick(img, segment.labels)[1,]
    }, warning = function(w) {
    }, error = function(e) {
        rep(NA, 26)
    }, finally = {
    })
    if (length(haral.feat) == 0) {
            haral.feat <- rep(NA, 26)
        }
    test.data[i, 24:49] <- haral.feat

    # resized haralick features
    haral.feat.resize <- tryCatch({
        computeFeatures.haralick(img.resize,
            resize.segment.labels)[1,]
    }, warning = function(w) {
    }, error = function(e) {
        rep(NA, 26)
    }, finally = {
    })
    if (length(haral.feat.resize) == 0) {
            haral.feat.resize <- rep(NA, 26)
        }
    test.data[i, 50:75] <- haral.feat.resize

    # pixel features
    test.data[i, 76:975] <- as.vector(img.resize)

    # new 24 features
    a <- shape.feat[max.area.ind,][1]
    if (length(a) == 0) {
        a <- 0
    }

    p <- shape.feat[max.area.ind,][2]
    if (length(p) == 0) {
        p <- 0
    }

    # shape factor, (4 * PI * Area) / (Perimeter^2)
    test.data[i, 976] <- (4 * pi * a) / (p^2)

    # roundness, (Perimeter^2) / 4 * PI * Area)
    test.data[i, 977] <- (p^2) / (4 * pi * a)

    # ECD, 2*sqrt(Area/PI)
    test.data[i, 978] <- 2 * sqrt(a / pi)

    # thread length, (Perimeter + sqrt(Perimeter^2-16*Area))/4
    test.data[i, 979] <- (p + sqrt(p^2 - 16 * a)) / 4

    # thread width, (Perimeter - sqrt(Perimeter^2-16*Area))/4
    test.data[i, 980] <- (p - sqrt(p^2 - 16 * a)) / 4

    # 7 : mean, skew, kurtosis, stdev, mode, median, sum of grayscale
    test.data[i, 981] <- mean(img)
    test.data[i, 982] <- skewness(as.vector(img))
    test.data[i, 983] <- kurtosis(as.vector(img))
    test.data[i, 984] <- sd(img)
    test.data[i, 985] <- as.numeric(rownames(table(img))[which.max(table(img))])
    test.data[i, 986] <- median(img)
    test.data[i, 987] <- sum(img)


    # new 12 features (resized)
    a <- shape.feat.resize[max.area.ind.resize,][1]
    if (length(a) == 0) {
        a <- 0
    }
    p <- shape.feat.resize[max.area.ind.resize,][2]
    if (length(p) == 0) {
        p <- 0
    }
    # shape factor, (4 * PI * Area) / (Perimeter^2)
    test.data[i, 988] <- (4 * pi * a) / (p^2)

    # roundness, (Perimeter^2) / 4 * PI * Area)
    test.data[i, 989] <- (p^2) / (4 * pi * a)

    # ECD, 2*sqrt(Area/PI)
    test.data[i, 990] <- 2 * sqrt(a / pi)

    # thread length, (Perimeter + sqrt(Perimeter^2-16*Area))/4
    test.data[i, 991] <- (p + sqrt(p^2 - 16 * a)) / 4

    # thread width, (Perimeter - sqrt(Perimeter^2-16*Area))/4
    test.data[i, 992] <- (p - sqrt(p^2 - 16 * a)) / 4

    # 7 : mean, skew, kurtosis, stdev, mode, median, sum of grayscale
    test.data[i, 993] <- mean(img.resize)
    test.data[i, 994] <- skewness(as.vector(img.resize))
    test.data[i, 995] <- kurtosis(as.vector(img.resize))
    test.data[i, 996] <- sd(img.resize)
    test.data[i, 997] <- as.numeric(rownames(table(img.resize))[which.max(table(img.resize))])
    test.data[i, 998] <- median(img.resize)
    test.data[i, 999] <- sum(img.resize)
    i <- i + 1
}
end.time <- Sys.time()
print(end.time - start.time)
# 130400 testing processing took 
# 5.171916 hours hours

# fix NaNs 
test.data[which(is.nan(test.data), arr.ind=TRUE)] <- 0

# fix Inf
inf.inds <- which(test.data == Inf, arr.ind=TRUE)
test.data[inf.inds] <- -1
this.max <- max(test.data[, 1])
test.data[inf.inds] <- this.max

# fix NA
na.inds <- which(is.na(test.data), arr.ind=TRUE)
test.data[na.inds] <- 0

# scale by training stats and save the testing data
setwd(wd.start)
load("training_999_scaling_stats.RData")
test.data.scaled <- sweep(test.data, 2, train.means, "-")
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
setwd(wd.start)
save(test.data.scaled, file="testing_train-scaled_999.RData")