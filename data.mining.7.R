#7장. 군집분석 
#응집 분석 예제 
#응집 분석의 단점 = 개체 수가 많을 경우 시간이 오래 걸린다는 단점 
#응집 분석이 모아간다면 분할분석은 나눠 가는 느낌 
ex2 <- read.csv ("ex7-2.csv", header=T)
ex2
dist(ex2) #거리행렬을 생성하는 함수 (def=유클리디안)
dist(ex2, method="manhattan") #맨하탄 방식 

#hclust 응집분석하라 
single <- hclust(dist(ex2, method="manhattan"), method="single") #단일연결법
complete <- hclust(dist(ex2, method="manhattan"), method="complete") #완전연결법
average <- hclust(dist(ex2, method="manhattan"), method="average") #평균연결법

clustering1 <- hclust(dist(ex2, method="manhattan"), method="single")
clustering2 <- hclust(dist(ex2, method="manhattan"), method="complete")
clustering3 <- hclust(dist(ex2, method="manhattan"), method="average")

par(mfrow=c(1,3)) #1행 3열의 공간을 열어라 
plot(clustering1) 
plot(clustering2)
plot(clustering3)
#세로축: 각각의 군집이 형성될 때의 거리 

library(cluster)
divclustering <- diana(ex2, metric="manhattan")
plot(divclustering)

#K-평균 군집분석 예제
ex2 <- as.matrix(ex2)
ave <- hclust(dist(ex2), method="average") #응집분석으로 사전분석을 한번 해봄 
initial <- tapply(ex2, list(rep(cutree(ave,2), ncol(ex2)),col(ex2)),mean)
#군집을 두개 만들어서 두 군집의 평균 구한 것 
kmeans <- kmeans(ex2, initial, algorithm="MacQueen")
kmeans

#붓꽃자료의 군집분석
iris2 <- as.matrix(iris[,1:4])
iclustering1 <- hclust(dist(iris2), method="single") #두 군집의 거리는 가장 가까운 거
iclustering2 <- hclust(dist(iris2), method="complete") #가장 먼 것
iclustering3 <- hclust(dist(iris2), method="average") #두 군집의 거리는 평균값 

library(cluster)
idivclustering <- diana(iris2)
#응집분석으로 보면 단일, 최장은 2개 정도로 나눌 수 있는데 평균, 분할분석으로 하면 3개 정도
#이럴 때는 주관에 따라 달라질 수 있다. 
#붓꽃자료에 대한 계층적 군집분석 및 나무형 그림
par(mfrow=c(2,3))
plot(iclustering1)
plot(iclustering2)
plot(iclustering3)
plot(idivclustering)

#붓꽃자료에 대한 계층적 군집분석 분할표 
#위의 자료를 토대로 3개로 나눠보기로 함 
id.iclustering1 <- cutree(iclustering1,3)
id.iclustering2 <- cutree(iclustering2,3)
id.iclustering3 <- cutree(iclustering3,3)
id.idivclustering <- cutree(idivclustering, 3)

table(iris[,5],id.iclustering1)
table(iris[,5],id.iclustering2)
table(iris[,5],id.iclustering3)
table(iris[,5],id.idivclustering)

#붓꽃자료의 비계층적 군집분석 결과 
#비계층적 군집분석은 계층적 군집분석보다 조금 더 복잡함 
initial <- tapply(iris2, list(rep(cutree(iclustering3, 3), ncol(iris2)), col(iris2)), mean)
ikmeans <- kmeans(iris2, initial, algorithm="MacQueen")
ikmeans
                  
table(iris[,5], ikmeans$cluster)
                  
                  
                  
                  
                  