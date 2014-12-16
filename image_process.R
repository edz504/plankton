library(EBImage) 
library(e1071)

# count the number of total images
wd.top <- "C:/Users/edz504/Documents/Data Science Projects/Kaggle/plankton/train"
num.img <- 0
for (class in list.files()) {
    setwd(paste(wd.top, "/", class, sep=""))
    num.img <- num.img + length(list.files())
}

# store the dimensions in a dataframe
train.dim <- data.frame(matrix(nrow=num.img, ncol=3))
colnames(train.dim) <- c("rows", "cols", "class")
# class counter
c <- 1
# img counter
i <- 1
setwd(wd.top)
for (class in list.files()) {
    setwd(paste(wd.top, "/", class, sep=""))
    for (file in list.files()) {
        img <- readImage(file)
        train.dim[i, ] <- c(nrow(img), ncol(img), c)
        i <- i + 1
    }

    c <- c + 1
}

summary(train.dim)

# we are tempted to resize the pictures, but let's check if size 
# is a factor
library(dplyr)
train.dim %>% 
    mutate(size = rows * cols) %>% 
        group_by(class) %>%
            summarise(mean_size=mean(size), max_size=max(size), 
            min_size=min(size), med_size=median(size))


# first attempt ==> resize downwards (will be smaller)
apply(train.dim, FUN=min, 2)
# make them all 30 x 30

train.data.1 <- matrix(nrow = num.img, ncol = 30 * 30 + 1)
setwd(wd.top)
c <- 1
i <- 1
for (class in list.files()) {
    setwd(paste(wd.top, "/", class, sep=""))
    for (file in list.files()) {
        img <- readImage(file)
        train.data.1[i, 1] <- c
        train.data.1[i, 2:901] <- as.vector(resize(img, 30, 30))
        i <- i + 1
    }
    c <- c + 1
}





# resize upwards (more pixels = more data, but may be unnecessar)