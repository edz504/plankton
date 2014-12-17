library(e1071)
library(dplyr)

load("testing_30x30.RData")
load("svmmodel_N100_30x30.RData")
top.wd <- "C:/Users/edz504/Documents/Data Science Projects/Kaggle/plankton/"

setwd("test")
test.files <- list.files()
setwd(paste(top.wd, "train", sep=""))
classes <- t(data.frame(list.files()))

# use on testing set given, in chunks of 1000 
# init file and 400 first
setwd(top.wd)
write.table(classes, file="submission1.csv", quote=FALSE,
    col.names = FALSE, row.names=FALSE, sep=",")

pred <- predict(svm_select, test.data.1[1:400,],
    probability = TRUE)
probs <- attr(pred, "probabilities")
output <- cbind(test.files[1:400], probs)
write.table(output, file="submission1.csv", append=TRUE,
    quote=FALSE, col.names = FALSE, row.names=FALSE, sep=",")

inds <- seq(400, nrow(test.data.1), by=1000)
for (i in seq(1, length(inds) - 1, by=1)) {
    pred <- predict(svm_select, 
        test.data.1[inds[i]:inds[i + 1], ],
        probability = TRUE)
    probs <- attr(pred, "probabilities")
    output <- cbind(test.files[inds[i]:inds[i + 1]], probs)
    write.table(output, file="submission1.csv", append=TRUE,
        quote=FALSE, col.names = FALSE, row.names=FALSE, sep=",")
}


# resize upwards