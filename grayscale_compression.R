setwd("C:\\Users\\himank\\Documents\\R\\myimagereduction")
library(imager)
par(mfrow=c(1,1))
image<-load.image("flower.jpg")
plot(image,axes=FALSE)
#getting grayscale
megray<-grayscale(image)
plot(megray,axes=FALSE)
        
#single value decomposition
svd1<-svd(scale(megray))
str(svd1)
#plotting to see variance explained
plot(svd1$d^2/sum(svd1$d^2),pch=19,xlab="Singular vector",ylab="Variance explained")
#reducing the size/vectors hence the size
approx5<-svd1$u[,1:5] %*% diag(svd1$d[1:5]) %*% t(svd1$v[,1:5])

approx10<-svd1$u[,1:10] %*% diag(svd1$d[1:10]) %*% t(svd1$v[,1:10])

approx25<-svd1$u[,1:25] %*% diag(svd1$d[1:25]) %*% t(svd1$v[,1:25])
par(mfrow=c(2,2),mar=c(1,1,1,1))
#plotting for reduced images
plot(as.cimg(approx5),main="(a) 5 vectors",axes=FALSE)
plot(as.cimg(approx10),main="(b) 10 vectors",axes=FALSE)
plot(as.cimg(approx25),main="(c) 25 vectors",axes=FALSE)
plot(as.cimg(megray),main="(d) full image",axes=FALSE)

png("5_vectors.png")
plot(as.cimg(approx5),axes=FALSE)
dev.off()

png("10_vectors.png")
plot(as.cimg(approx10),axes=FALSE)
dev.off()

png("25_vectors.png")
plot(as.cimg(approx25),axes=FALSE)
dev.off()

