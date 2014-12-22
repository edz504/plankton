library(EMCluster)
library(mclust)
library(dplyr)


# find centroids / prior means in training data by class
load("training_pixelandallEBfeat.RData")
train <- data.frame(class=labels, train.data.scaled)
class.means <- train %>%
    group_by(class) %>%
        summarise_each(funs(mean))
save(class.means, file="training_class_means.RData")

load("testing_scaled_pixelandallEBfeat.RData")
load("training_class_means.RData")
pr <- matrix(nrow=6, ncol=2)
pr[,1] <- seq(2.5, 5, by=0.5)
pr[,2] <- seq(45, 95, by=10)

# test mclust, find how to output probabilities
start.time <- Sys.time()
faithfulMclust <- Mclust(faithful, 
    G=6, prior=priorControl(mean=pr))
end.time <- Sys.time()
print(end.time - start.time)
summary(faithfulMclust)
p <- faithfulMclust$z

# start.time <- Sys.time()
# testing.mclust <- Mclust(test.data.scaled, )
# end.time <- Sys.time()
# print(end.time - start.time)


############ EMCluster package

# testing on faithful
ret.EM <- init.EM(faithful, nclass=6)
emcl <- emcluster(x=faithful,   # data
    emobj=ret.EM,               # initialized object
    Mu=pr,                      # centers (priors)
    lab=rep(0, nrow(faithful))) # labels for semi-supervised

probs <- e.step(faithful, emobj=emcl, norm=TRUE)$Gamma


