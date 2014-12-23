> data(genotype)
Warning message:
In data(genotype) : data set 'genotype' not found
> 
### using bgmm (no weird initialization)
library(bgmm)
load("split_data_semi.RData")
M.known <- 100
M.unknown <- 400
T <- 20 # number of bagging samples
> > > > > > > 
> data(genotype)
Warning message:
In data(genotype) : data set 'genotype' not found
> data(genotypes)
> ls()
 [1] "bag.test.ind"     "bag.train.ind"    "end.time"         "genotypes"       
 [5] "labels"           "M.known"          "M.unknown"        "start.time"      
 [9] "T"                "test.all.scaled"  "train.all.scaled"
> names(genotypes)
[1] "X"      "knowns" "B"      "labels"
> dim(genotypes$X)
[1] 318   2
>     start.time <- Sys.time()
    bag.train.ind <- sample(nrow(train.all.scaled), M.known)
    bag.test.ind <- sample(nrow(test.all.scaled), M.unknown)
> > >     a <- test.all.scaled[bag.test.ind,]
    b <- train.all.scaled[bag.train.ind,]
    c <- labels[bag.train.ind]
> > > dim(a)
[1] 400 975
> a <- a[1:318, 1:2]
> dim(genotypes$knowns)
[1] 15  2
> dim(b)
[1] 100 975
> b <- b[1:15, 1:2]
> dim(genotypes$labels)
NULL
> length(genotypes$labels)
[1] 15
> length(c)
[1] 100
> c <- c[1:15]
>     mod.semi.sup <- semisupervised(
        X = a,
        knowns = b,
        class = c)
    end.time <- Sys.time()
+ + + Error in `[<-`(`*tmp*`, i, id[i], value = 1) : subscript out of bounds
> > dim(a)
[1] 318   2
> dim(b)
[1] 15  2
> length(c)
[1] 15
> mss <- semisupervised(X=genotypes$X, knowns=genotypes$knowns, class=gneotypes$class)
Error in semisupervised(X = genotypes$X, knowns = genotypes$knowns, class = gneotypes$class) : 
  object 'gneotypes' not found
> mss <- semisupervised(X=genotypes$X, knowns=genotypes$knowns, class=gnenotypes$class)
Error in semisupervised(X = genotypes$X, knowns = genotypes$knowns, class = gnenotypes$class) : 
  object 'gnenotypes' not found
> mss <- semisupervised(X=genotypes$X, knowns=genotypes$knowns, class=genotypes$class)
Error in semisupervised(X = genotypes$X, knowns = genotypes$knowns, class = genotypes$class) : 
  Argument class need to be specified
> mss <- semisupervised(X=genotypes$X, knowns=genotypes$knowns, class=genotypes$labels)
> mss 
$pi
        1         3         2 
0.4504505 0.1261261 0.4234234 

$mu
          [,1]      [,2]
[1,] 0.7648964 0.2455181
[2,] 0.1961935 0.6126590
[3,] 0.7449416 0.6197700

$cvar