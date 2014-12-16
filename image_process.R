library(EBImage) 
library(e1071)

# count the number of total images

wd.top <- "C:/Users/edz504/Documents/Data Science Projects/Kaggle/plankton/train"
num.img <- 0
for (class in list.files()) {
    setwd(paste(wd.top, "/", class, sep=""))
    num.img <- num.img + length(list.files())
}



train.dim <- data.frame(matrix(nrow=, ncol=3))

img1 <- readImage("train/acantharia_protist/100224.jpg")
img2 <- readImage("train/acantharia_protist/100723.jpg")
