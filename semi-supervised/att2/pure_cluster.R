library(flexclust)

# radial basis function
radialDist <- function(x, centers) {
    gamma <- 1 / 121
    z <- matrix(0, nrow(x), ncol=nrow(centers))
    for (k in 1:nrow(centers)) {
        d <- dist(rbind(x, centers[k,]), method="manhattan")
        d.mat <- as.matrix(d)
        dist <- d.mat[nrow(d.mat), -ncol(d.mat)]
        z[, k] <- exp(-gamma*dist^2)
    }
    z
}

# just normal euclidean
load("testing_scaled_pixelandallEBfeat.RData")
load("training_class_means.RData")
library(dplyr)
class.means <- class.means %>% select(-class)
clus <- kcca(test.data.scaled, k=class.means, simple=TRUE)

assigned <- attr(clus, "cluster")
centers <- attr(clus, "centers")
