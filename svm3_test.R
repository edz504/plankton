library(e1071)
library(dplyr)

load("testing_EBfeat.RData")
test.data.scaled <- scale(test.data.1)
load("svmmodel_EBfeat.RData")

top.wd <- "C:/Users/edz504/Documents/Data Science Projects/Kaggle/plankton/"
setwd(top.wd)
setwd("test")
test.files <- list.files()
setwd(paste(top.wd, "train", sep=""))
header <- t(data.frame(c("image", list.files())))

# use on testing set given, in chunks of 1000 
# init file first
setwd(top.wd)
write.table(header, file="submission5.csv", quote=FALSE,
    col.names = FALSE, row.names=FALSE, sep=",")

# fix the NA features (try 0 for now)
# later, should examine more why these features are NA
# also check out svm documentation regarding na.action
na.inds <- which(is.na(test.data.scaled), arr.ind=TRUE)
test.data.scaled[na.inds] <- 0

INCR <- 999
i <- 1

start.time2 <- Sys.time()
check.inds <- c()
check.length <- 0
while (TRUE) {
    upper <- min(i + INCR, length(test.files))
    these.inds <- i:upper

    cat("Predicting testing samples ", 
        min(these.inds), " to ", max(these.inds), "...\n", sep="")
    pred <- predict(svm_select,
        test.data.scaled[these.inds, ],
        probability=TRUE)
    probs <- attr(pred, "probabilities")
    cat("Probs dim=", dim(probs), "\n")
    cat("test.files[these.inds] length=", 
        length(test.files[these.inds]), "\n")
    output <- cbind(test.files[these.inds], probs)
    write.table(output, file="submission5.csv", append=TRUE,
        quote=FALSE, col.names = FALSE, row.names=FALSE, sep=",")
    check.length <- check.length + nrow(probs)
    check.inds <- c(check.inds, these.inds)
    if (length(these.inds) < 1000) {
        break
    }

    i <- i + INCR + 1
}
end.time2 <- Sys.time()
print(end.time2 - start.time2)
print(paste("Check indicies?:", all(check.inds == 1:130400)))
print(paste("Check output length?:", check.length == 130400))
# time to predict all with model 901 feat trained on all:
# 2.04 hours
# time to predict all with model 12 feat trained on all:
# 22 min

      
# check number of rows
s5 <- read.csv("submission/submission5.csv")