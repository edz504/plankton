library(e1071)
library(dplyr)

load("testing_30x30.RData")
load("svmmodel_N100_30x30.RData")
top.wd <- "C:/Users/edz504/Documents/Data Science Projects/Kaggle/plankton/"

setwd("test")
test.files <- list.files()
setwd(paste(top.wd, "train", sep=""))
header <- t(data.frame(c("image", list.files())))

# use on testing set given, in chunks of 1000 
# init file first
setwd(top.wd)
write.table(header, file="submission2.csv", quote=FALSE,
    col.names = FALSE, row.names=FALSE, sep=",")

INCR <- 999
i <- 1

while (TRUE) {
    upper <- min(i + INCR, length(test.files))
    these.inds <- i:upper

    cat("Predicting testing samples ", 
        min(these.inds), " to ", max(these.inds), "...\n", sep="")
    pred <- predict(svm_select,
        test.data.1[these.inds, ],
        probability=TRUE)
    probs <- attr(pred, "probabilities")
    output <- cbind(test.files[these.inds], probs)
    write.table(output, file="submission2.csv", append=TRUE,
        quote=FALSE, col.names = FALSE, row.names=FALSE, sep=",")
    if (length(these.inds) < 1000) {
        break
    }

    i <- i + INCR + 1
}


      

# resize upwards