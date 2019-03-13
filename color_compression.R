setwd("C:\\Users\\himank\\Documents\\R\\myimagereduction")
library(imager)
image<-load.image("flower.jpg")
par(mfrow=c(1,1))
plot(image,axes=FALSE)
svd1<-svd(scale(grayscale(image)))
dev.off()
plot(svd1$d^2/sum(svd1$d^2),axes=TRUE)
R = image[,,1]
G = image[,,2]
B = image[,,3]
par(mfrow=c(3,1),mar=c(1,1,1,1))
cscale <- function(v) rgb(v,0,0)
grayscale(image) %>% plot(colourscale=cscale,rescale=FALSE,axes=FALSE)

cscale <- function(v) rgb(0,v,0)
grayscale(image) %>% plot(colourscale=cscale,rescale=FALSE,axes=FALSE)

cscale <- function(v) rgb(0,0,v)
grayscale(image) %>% plot(colourscale=cscale,rescale=FALSE,axes=FALSE)

svdR<-svd(R)
svdG<-svd(G)
svdB<-svd(B)
svdRGB<-list(svdR,svdG,svdB)
par(mfrow=c(2,2),mar=c(1,1,1,1))
for(j in c(5,10,25,50)){
comp <- sapply(svdRGB, function(i){
        compressed = i$u[,1:j] %*% diag(i$d[1:j]) %*% t(i$v[,1:j])
}, simplify = 'array')
comp<-as.cimg(comp)
plot(comp,axes=FALSE,main=paste("rank=",j))
}


png("red.jpg")
cscale <- function(v) rgb(v,0,0)
grayscale(image) %>% plot(colourscale=cscale,rescale=FALSE,axes=FALSE)
dev.off()
png("green.jpg")
cscale <- function(v) rgb(0,v,0)
grayscale(image) %>% plot(colourscale=cscale,rescale=FALSE,axes=FALSE)
dev.off()
png("blue.jpg")
cscale <- function(v) rgb(0,0,v)
grayscale(image) %>% plot(colourscale=cscale,rescale=FALSE,axes=FALSE)
dev.off()
