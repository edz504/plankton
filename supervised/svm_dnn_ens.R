top.wd <- "C:/Users/edz504/Documents/Data Science Projects/Kaggle/plankton"
setwd(top.wd)
setwd("submission")

s7 <- read.csv("submission7.csv")
s6 <- read.csv("submission6.csv")


# set up output
setwd(top.wd)
setwd("test")
test.files <- list.files()
setwd(paste(top.wd, "/train", sep=""))
header <- t(data.frame(c("image", list.files())))
setwd(top.wd)
write.table(header, file="submission8.csv", quote=FALSE,
    col.names = FALSE, row.names=FALSE, sep=",")

# average s6 and s7 and write
image.names <- s7[, 1]
s8 <- (s7[, -1] + s6[, -1]) / 2
output <- cbind(image.names, s8)

setwd(top.wd)
write.table(output, file="submission8.csv", append=TRUE,
    quote=FALSE, col.names = FALSE, row.names=FALSE, sep=",")