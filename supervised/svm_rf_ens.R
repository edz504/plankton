top.wd <- "C:/Users/edz504/Documents/Data Science Projects/Kaggle/plankton"
setwd(top.wd)
setwd("submission")

s9 <- read.csv("submission9.csv")
s6 <- read.csv("submission6.csv")



##### equal weighting (50-50)
# set up output
setwd(top.wd)
setwd("test")
test.files <- list.files()
setwd(paste(top.wd, "/train", sep=""))
header <- t(data.frame(c("image", list.files())))
setwd(top.wd)
write.table(header, file="submission10.csv", quote=FALSE,
    col.names = FALSE, row.names=FALSE, sep=",")

# average s6 and s7 and write
image.names <- s9[, 1]
s10 <- (s9[, -1] + s6[, -1]) / 2
output <- cbind(image.names, s10)

setwd(top.wd)
write.table(output, file="submission10.csv", append=TRUE,
    quote=FALSE, col.names = FALSE, row.names=FALSE, sep=",")



##### diff weighting (70-30, svm-rf)
setwd(top.wd)
setwd("test")
test.files <- list.files()
setwd(paste(top.wd, "/train", sep=""))
header <- t(data.frame(c("image", list.files())))
setwd(top.wd)
write.table(header, file="submission11.csv", quote=FALSE,
    col.names = FALSE, row.names=FALSE, sep=",")

# average s6 and s7 and write
image.names <- s9[, 1]
s11 <- (0.3 * s9[, -1] + 0.7 * s6[, -1]) / 2
output <- cbind(image.names, s11)

setwd(top.wd)
write.table(output, file="submission11.csv", append=TRUE,
    quote=FALSE, col.names = FALSE, row.names=FALSE, sep=",")

