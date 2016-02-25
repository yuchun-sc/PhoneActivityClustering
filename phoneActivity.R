# clustering analysis by K-means and and Hierarchical Clustering, SVD
# uses the dataset at 
# https://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones


features <- read.table("phoneDataset/features.txt")
activities <- read.table("phoneDataset/activity_labels.txt")
xVals <- read.table("phoneDataset/train/X_train.txt")
yVals <- read.table("phoneDataset/train/y_train.txt")
subject <- read.table("phoneDataset/train/subject_train.txt")

colnames(xVals) <- features[,2]

for (i in 1:6) {
    yVals[yVals == i] <- as.character(activities[i,2])
}

xVals$subject <- subject
xVals$activity <- yVals

samsungData <- xVals;

par(mfrow = c(1,2), mar = c(5, 4, 1,1))
samsungData$activity <- factor(unlist(samsungData$activity))
sub1 <- subset(samsungData, subject == 1)

plot(sub1[,1], col = sub1$activity, ylab = names(sub1)[1])
plot(sub1[,2], col = sub1$activity, ylab = names(sub1)[2])

par(mfrow = c(1, 2))
plot(sub1[,10], pch = 19, col = sub1$activity, ylab = names(sub1)[10])
plot(sub1[,11], pch = 19, col = sub1$activity, ylab = names(sub1)[11])

# separating moving from non-moving
distanceM <- dist(sub1[,10:12])
hclustering <- hclust(distanceM)
myplclust(hclustering, lab.col = unclass(sub1$activity))

# Singular Value Decomposition -- getting better separated 
svd1 <- svd(scale(sub1[,-c(562, 563)]))
par(mfrow = c(1, 2))
plot(svd1$u[,1], col = sub1$activity, pch = 19)
plot(svd1$u[,2], col = sub1$activity, pch = 19)

plot(svd1$v[,2], pch = 19)

maxCon <- which.max(svd1$v[, 2])
distanceMa <- dist(sub1[, c(10:12, maxCon)])
hclustering <- hclust(distanceMa)
par(mfrow = c(1, 1))
myplclust(hclustering, lab.col = unclass(sub1$activity))

# k-means -- suboptimal because inital conditions are randomized 
kClust <- kmeans(sub1[, -c(562:563)], centers = 6, nstart = 100);
table(kClust$cluster, sub1$activity)
plot(kClust$center[1, 1:20], pch = 19, ylab = "Cluster Center", xlab = "")
plot(kClust$center[4, 1:20], pch = 19, ylab = "Cluster Center", xlab = "")


myplclust <- function( hclust, lab=hclust$labels, lab.col=rep(1,length(hclust$labels)), hang=0.1,...){
    ## modifiction of plclust for plotting hclust objects *in colour*!
    ## Copyright Eva KF Chan 2009
    ## Arguments:
    ##    hclust:    hclust object
    ##    lab:        a character vector of labels of the leaves of the tree
    ##    lab.col:    colour for the labels; NA=default device foreground colour
    ##    hang:     as in hclust & plclust
    ## Side effect:
    ##    A display of hierarchical cluster with coloured leaf labels.
    y <- rep(hclust$height,2); x <- as.numeric(hclust$merge)
    y <- y[which(x<0)]; x <- x[which(x<0)]; x <- abs(x)
    y <- y[order(x)]; x <- x[order(x)]
    plot( hclust, labels=FALSE, hang=hang, ... )
    text( x=x, y=y[hclust$order]-(max(hclust$height)*hang),
          labels=lab[hclust$order], col=lab.col[hclust$order], 
          srt=90, adj=c(1,0.5), xpd=NA, ... )
}
    