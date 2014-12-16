library(EBImage) 
library(e1071)

# count the number of total images
wd.top <- "C:/Users/edz504/Documents/Data Science Projects/Kaggle/plankton"
wd.train <- paste(wd.top, "/train", sep="")
setwd(wd.train)
num.img <- 0
for (class in list.files()) {
    setwd(paste(wd.train, "/", class, sep=""))
    num.img <- num.img + length(list.files())
}

# store the dimensions in a dataframe
train.dim <- data.frame(matrix(nrow=num.img, ncol=3))
colnames(train.dim) <- c("rows", "cols", "class")
# class counter
c <- 1
# img counter
i <- 1
setwd(wd.train)
for (class in list.files()) {
    setwd(paste(wd.train, "/", class, sep=""))
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

train.data.1 <- matrix(nrow = num.img, ncol = 30 * 30)
labels <- rep(NA, num.img)
setwd(wd.train)
c <- 1
i <- 1
for (class in list.files()) {
    setwd(paste(wd.train, "/", class, sep=""))
    for (file in list.files()) {
        img <- readImage(file)
        labels[i] <- c
        train.data.1[i, ] <- as.vector(resize(img, 30, 30))
        i <- i + 1
    }
    c <- c + 1
}

# make labels a factor
labels <- factor(labels)

# save the training and testing data
setwd(wd.top)
save(train.data.1, labels, file="training_30x30.RData")

# visualize how many of each type
library(ggplot2)
labels.df <- data.frame(labels)
counts <- labels.df %>% group_by(labels) %>% summarise(count=n())
ggplot(counts, aes(x=labels, y=count)) + geom_bar(stat="identity")
ggsave(file="label_counts.png", width=10, height=6)

# we can get 7 different labels using rows 1-2000, so let's try
# on that just to see what it looks like (and to time and get an idea for it)
svm_model <- svm(train.data.1[1:2000,], labels[1:2000], type='C', kernel='linear',
    probability=TRUE)
# note, takes roughly 30 sec

pred <- predict(svm_model, train.data.1[1:2000,],
    probability = TRUE)
attr(pred, "probabilities")[1:10,]

#









# resize upwards