library(randomForest)
library(e1071)

load("testing_train-scaled_999.RData")
load("rfmodel_999.RData")
load("svmmodel_999.RData")

top.wd <- "C:/Users/edz504/Documents/Data Science Projects/Kaggle/plankton/"
setwd(top.wd)
setwd("test")
test.files <- list.files()
setwd(paste(top.wd, "train", sep=""))
header <- t(data.frame(c("image", list.files())))

setwd(top.wd)
# RF output
write.table(header, file="submission12.csv", quote=FALSE,
    col.names = FALSE, row.names=FALSE, sep=",")
# SVM output
write.table(header, file="submission13.csv", quote=FALSE,
    col.names = FALSE, row.names=FALSE, sep=",")

INCR <- 999
i <- 1

start.time <- Sys.time()
print(start.time)
check.inds <- c()
check.length <- 0
while (TRUE) {
    upper <- min(i + INCR, length(test.files))
    these.inds <- i:upper

    cat("Predicting testing samples with RF model ", 
        min(these.inds), " to ", max(these.inds), "...\n", sep="")
    ### RF model predict
    probs.rf <- predict(rf_model,
        test.data.scaled[these.inds, ],
        "prob")
    output.rf <- cbind(test.files[these.inds], probs.rf)
    write.table(output.rf, file="submission12.csv", append=TRUE,
        quote=FALSE, col.names = FALSE, row.names=FALSE, sep=",")

    cat("Predicting testing samples with SVM model ", 
        min(these.inds), " to ", max(these.inds), "...\n", sep="")
    ### SVM model predict
    pred <- predict(svm_model,
        test.data.scaled[these.inds, ],
        probability=TRUE)
    probs.svm <- attr(pred, "probabilities")
    output.svm <- cbind(test.files[these.inds], probs.svm)
    write.table(output.svm, file="submission13.csv", append=TRUE,
        quote=FALSE, col.names = FALSE, row.names=FALSE, sep=",")

    check.length <- check.length + nrow(probs.svm)
    check.inds <- c(check.inds, these.inds)
    if (length(these.inds) < 1000) {
        break
    }

    i <- i + INCR + 1
}
end.time <- Sys.time()
print(end.time - start.time)
print(paste("Check indicies?:", all(check.inds == 1:130400)))
print(paste("Check output length?:", check.length == 130400))

# time to predict RF and SVM together (999 feat) = 
# 2.28908 hours?
s12 <- read.csv("submission12.csv")
s13 <- read.csv("submission13.csv")

##### equal weighting (50-50) (submission 14)
setwd(top.wd)
setwd("test")
test.files <- list.files()
setwd(paste(top.wd, "/train", sep=""))
header <- t(data.frame(c("image", list.files())))
setwd(top.wd)
write.table(header, file="submission14.csv", quote=FALSE,
    col.names = FALSE, row.names=FALSE, sep=",")

image.names <- s12[, 1]
s14 <- (s12[, -1] + s13[, -1]) / 2
output <- cbind(image.names, s14)

setwd(top.wd)
write.table(output, file="submission14.csv", append=TRUE,
    quote=FALSE, col.names = FALSE, row.names=FALSE, sep=",")





top.wd <- "C:/Users/edz504/Documents/Data Science Projects/Kaggle/plankton/"
setwd(top.wd)
setwd("submission")
s12 <- read.csv("submission12.csv")
s13 <- read.csv("submission13.csv")
##### 90 svm, 10 rf (submission 15)
setwd(top.wd)
setwd("test")
test.files <- list.files()
setwd(paste(top.wd, "/train", sep=""))
header <- t(data.frame(c("image", list.files())))
setwd(top.wd)
write.table(header, file="submission15.csv", quote=FALSE,
    col.names = FALSE, row.names=FALSE, sep=",")

image.names <- s12[, 1]
s15 <- (.1 * s12[, -1] + .9 * s13[, -1]) / 2
output <- cbind(image.names, s15)

setwd(top.wd)
write.table(output, file="submission15.csv", append=TRUE,
    quote=FALSE, col.names = FALSE, row.names=FALSE, sep=",")