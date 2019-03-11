setwd("C:\\Users\\himank\\Documents\\R\\myimagereduction")
library(imager)
image<-load.image("me.jpg")
#getting grayscale
megray<-grayscale(image)
#turning into matrix
memat<-as.matrix(megray)
image(memat)
#reversing the photo
me<-memat[,ncol(memat):1]
image(me)
#single value decomposition
svd1<-svd(scale(me))
#plotting to see variance explained
plot(svd1$d^2/sum(svd1$d^2),pch=19,xlab="Singular vector",ylab="Variance explained")
#reducing the size/vectors hence the size
approx5<-svd1$u[,1:5] %*% diag(svd1$d[1:5]) %*% t(svd1$v[,1:5])

approx10<-svd1$u[,1:10] %*% diag(svd1$d[1:10]) %*% t(svd1$v[,1:10])

approx20<-svd1$u[,1:20] %*% diag(svd1$d[1:20]) %*% t(svd1$v[,1:20])
par(mfrow=c(2,2))
#plotting for reduced images
image(approx5,main="(a)")
image(approx10,main="(b)")
image(approx20,main="(c)")
image(me,main="(d)")

png("5_vectors.png")
image(approx5,main="(a)")
dev.off()

png("20_vectors.png")
image(approx20)
dev.off()

png("full_image.png")
image(me)
dev.off()