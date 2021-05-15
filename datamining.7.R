#7장. 군집분석 
#응집 분석 예제 
ex2 <- read.csv ("ex7-2.csv", header=T)
ex2
dist(ex2)

single <- hclust(dist(ex2, method="manhattan"), method="single")
complete <- hclust(dist(ex2, method="manhattan"), method="complete")
average <- hclust(dist(ex2, method="manhattan"), method="average")

clustering1 <- hclust(dist(ex2, method="manhattan"), method="single")
clustering2 <- hclust(dist(ex2, method="manhattan"), method="complete")
clustering3 <- hclust(dist(ex2, method="manhattan"), method="average")

par(mfrow=c(1,3))
plot(clustering1)
plot(clustering2)
plot(clustering3)

library(cluster)
divclustering <- diana(ex2, metric="manhattan")
plot(divclustering)

#K-평균 군집분석 예제
library(kmeans)
ex2 <- as.matrix(ex2)
ave <- hclust(dist(ex2), method="average")
initial <- tapply(ex2, list(rep(cutree(ave,2), ncol(ex2)),col(ex2)),mean)
kmeans <- kmeans(ex2, initial, algorithm="MacQueen")
kmeans

#붓꽃자료의 군집분석
iris2 <- as.matrix(iris[,1:4])
iclustering1 <- hclust(dist(iris2), method="single")
iclustering2 <- hclust(dist(iris2), method="complete")
iclustering3 <- hclust(dist(iris2), method="average")

library(cluster)
idivclustering <- diana(iris2)

#붓꽃자료에 대한 계층적 군집분석 및 나무형 그림
par(mfrow=c(2,3))
plot(iclustering1)
plot(iclustering2)
plot(iclustering3)
plot(idivclustering)

#붓꽃자료에 대한 계층적 군집분석 분할표 
id.iclustering1 <- cutree(iclustering1,3)
id.iclustering2 <- cutree(iclustering2,3)
id.iclustering3 <- cutree(iclustering3,3)
id.idivclustering <- cutree(idivclustering, 3)

table(iris[,5],id.iclustering1)
table(iris[,5],id.iclustering2)
table(iris[,5],id.iclustering3)
table(iris[,5],id.idivclustering)

#붓꽃자료의 비계층적 군집분석 결과 
initial <- tapply(iris2, list(rep(cutree(iclustering3, 3), ncol(iris2)), col(iris2)), mean)
ikmeans <- kmeans(iris2, initial, algorithm="MacQueen")
ikmeans
                  
table(iris[,5], ikmeans$cluster)
                  
                  
                  
                  
                  