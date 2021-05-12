#3장 나무모형 
#타이타닉 데이터 전처리 
titanic = read.csv("titanic.csv", header=T, sep=",")
summary(titanic)
str(titanic)

titanic$Class <- as.factor(titanic$Class)
titanic$Age <- as.factor(titanic$Age)
titanic$Sex <- as.factor(titanic$Sex)
titanic$Survived <-as.factor(titanic$Survived)

titanic$Survived=ifelse(titanic$Survived=="Yes", 1, 0)

#CART 나무모형 실행 
library(rpart)
my.control <- rpart.control(xval=10, cp=-0.01, minsplit=1)
fit.titanic <- rpart(Survived ~., data=titanic, method="class", control=my.control)
print(fit.titanic)
printcp(fit.titanic)

#가지치기
fit.prun.titanic <- prune(fit.titanic, cp=0.0)
print(fit.prun.titanic)

#cart 나무모형 그리기
plot(fit.prun.titanic, uniform=T, compress=T, margin=0.1)
text(fit.prun.titanic, use.n=T, col="blue")
summary(fit.prun.titanic)

#독일신용평가데이터 
german = read.table("C:/rdata/germandata.txt", header=T)
german$numcredits = factor (german$numcredits)
german$residence = factor (german$residence)
german$residpeople = factor(german$residpeople)
german$y = ifelse(german$y == "good", 1, 0) #good = 1, bad = 0
  
summary(german)
threshold = 0.5 #cutoff

library(rpart)
my.control <- rpart.control(xval=10, cp=0, minsplit=5)
fit.german <- rpart(y ~., data = german, method="class", control=my.control)
print(fit.german)

printcp(fit.german)

ii = which.min(fit.german$cp[,4]) #find min xerror
ii
fit.prun.german = prune(fit.german, cp = fit.german$cp[ii,1]) #Pruning
print(fit.prun.german)

pred.german <- predict(fit.prun.german, newdata=german, type="class")
tab=table(german$y, pred.german, dnn=c("Actual","Predicted"))
print(tab)

1-sum(diag(tab))/sum(tab)

#훈련데이터와 검증데이터로 분할하여 분류나무 검증 
set.seed(1234)
i = sample(1:nrow(german), round(nrow(german)*0.7)) #천개중에 70프로 샘플
i
german.train = german[i,] #training data 70%
german.test = german[-i,] #tested data 30%

my.control = rpart.control(xval=10, cp=0, minsplit=5)
fit.german = rpart (y ~ ., data=german.train, method="class", control=my.control)
printcp(fit.german)

ii = which.min(fit.german$cp[,4]) #find min xerror
ii
fit.prun.german = prune(fit.german, cp=fit.german$cp[ii,1]) #이 구문과 아래가 같은 뜻임 
fit.prun.german

p.german.test=predict(fit.prun.german, newdata=german.test, type="class")
tab=table(german.test$y, p.german.test, dnn=c("Actual", "Predicted"))
print(tab)
1-sum(diag(tab))/sum(tab)

#3.5 회귀나무모형의 분할방법 
#보스턴 데이터 
library(MASS)
Boston$chas = factor(Boston$chas)
Boston$rad = factor(Boston$rad)
summary(Boston)

my.control=rpart.control(xval=10, cp=0, minsplit=nrow(Boston)*0.05) #회귀나무라서 다르게지정 
fit.Boston = rpart(medv ~ ., data = Boston, method="anova", control=my.control)
printcp(fit.Boston)

ii = which.min(fit.Boston$cp[,4]) #find min xerror
ii
fit.prun.Boston = prune(fit.Boston, cp=fit.Boston$cp[ii,1]) 
print(fit.prun.Boston)

plot(fit.prun.Boston, uniform=T, compress=T, margin=0.1)
text(fit.prun.Boston, use.n=T, col="blue")

#CART 회귀나무모형의 적합값 및 평균오차제곱합
Boston$medv.hat = predict(fit.prun.Boston, newdata=Boston, type="vector")
mean((Boston$medv - Boston$medv.hat)^2)

#훈련 데이터와 검증 데이터로 분할하여 회귀나무 평가 
set.seed(1234)
i=sample(1:nrow(Boston), round(nrow(Boston)*0.7))
Boston.train = Boston[i,] #70% for training data
Boston.test = Boston[-i,] #30% for test data
fit.Boston = rpart(medv ~., data=Boston.train, method="anova", control=my.control)
printcp(fit.Boston)

ii = which.min(fit.Boston$cp[,4]) #find min xerror
ii
fit.prun.Boston = prune(fit.Boston, cp=fit.Boston$cp[ii,1])  
fit.prun.Boston

medv.hat.test = predict(fit.prun.Boston, newdata=Boston.test, type="vector")
mean((Boston.test$medv - yhat.tree)^2) #PMSE

