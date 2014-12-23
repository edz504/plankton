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

start.time <- Sys.time()
print(start.time)
clus <- kcca(as.matrix(test.data.scaled),
    k=as.matrix(class.means), simple=TRUE)
end.time <- Sys.time()
print(end.time - start.time)

save(clus, file="cluster_output_kcca.RData")

assigned <- attr(clus, "cluster")
centers <- attr(clus, "centers")
