library(flipCluster)
data("consultant", package = "flipExampleData")
nms <- names(consultant)
dat <- consultant[, match("Q050__1", nms):match("Q050__25", nms) ]
meanDat <- as.data.frame(lapply(dat, unclass))
mn <- matrix(apply(meanDat, 2, mean, na.rm = TRUE), byrow = TRUE, ncol = 25, nrow = nrow(meanDat))
meanDat[is.na(meanDat)] <- mn[is.na(meanDat)]

test_that("kmeans",
          {

                missing = "Imputation (replace missing values with estimates)"
                expect_error(suppressWarnings(KMeans(data = dat, missing = missing, show.labels = TRUE, centers = 3)), NA)
                expect_error(KMeans(data = meanDat, missing = missing, show.labels = TRUE, centers = 3))

          })



#
# data(iris)
# library(e1071)
# bc1 <- bclust(iris[,1:4], 3, base.centers=5)
# KMeans(iris[,1:4], 3, algorithm = "Bagging")
#
# plot(bc1)
#
# table(clusters.bclust(bc1, 3))
# centers.bclust(bc1, 3)
