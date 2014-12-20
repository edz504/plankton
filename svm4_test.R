library(e1071)
library(dplyr)

load("testing_pixelandallEBfeat.RData")
load("svmmodel_pixelandallEBfeat.RData")

top.wd <- "C:/Users/edz504/Documents/Data Science Projects/Kaggle/plankton/"
setwd(top.wd)
setwd("test")
test.files <- list.files()
setwd(paste(top.wd, "train", sep=""))
header <- t(data.frame(c("image", list.files())))

# use on testing set given, in chunks of 1000 
# init file first
setwd(top.wd)
write.table(header, file="submission6.csv", quote=FALSE,
    col.names = FALSE, row.names=FALSE, sep=",")

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
    write.table(output, file="submission6.csv", append=TRUE,
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
# time to predict all with model 975 feat trained on all:
# X
      
# check number of rows
s6 <- read.csv("submission/submission6.csv")