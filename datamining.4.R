#4장. 앙상블 모형
library(MASS)
head(Boston)
install.packages("randomForest")
library("randomForest")
Boston$chas = factor(Boston$chas)
Boston$rad = factor(Boston$rad)

rf.boston <- randomForest(medv ~., data=Boston, ntree=100, mtry=5, importance=T, na.action=na.omit)
importance(rf.boston, type=1) #랜덤포레스트 방법의 변수 중요도 

Boston$medv.hat <- predict(rf.boston, newdata=Boston)
mean((Boston$medv - Boston$medv.hat)^2) 

plot(Boston$medv, Boston$medv.hat, xlab="Observed Values", ylab="Fitted Values")

#검증 데이터를 이용한 랜덤포레스트 방법의 예측 성능 
set.seed(1234)
i=sample(1:nrow(Boston), round(nrow(Boston)*0.7))
Boston.train = Boston[i,] #70% for training data
Boston.test = Boston[-i,] #30% for test data

rf.train.boston <- randomForest(medv ~ ., data = Boston.train, ntree=100, mtry=5, 
                      importance=T, na.action=na.omit)

medv.hat.test <- predict(rf.train.boston, newdata=Boston.test)
mean((Boston.test$medv - medv.hat.test)^2)

#독일신용평가데이터

library(rpart)
install.packages("adabag")
library(adabag)
german = read.table("C:/rdata/germandata.txt", header=T)
german$numcredits = factor (german$numcredits)
german$residence = factor (german$residence)
german$residpeople = factor(german$residpeople)
german$y = factor(german$y)

#배깅 방법의 실행 
my.control = rpart.control(xval=0, cp=0, minsplit=5, maxdepth=10)
bag.german <- bagging(y ~ ., data = german, mfinal=50, control=my.control)
#배깅 방법의 변수 중요도
print(bag.german$importance)
importanceplot(bag.german)
#배깅 방법에서 각 집단별 투표비율
pred.bag.german <- predict.bagging(bag.german, newdata=german)
head(pred.bag.german$prob,10)
#배깅 방법의 훈련 데이터에 대한 예측정확도
print(pred.bag.german$confusion)
1-sum(diag(pred.bag.german$confusion))/sum(pred.bag.german$confusion)
#배깅 방법의 분류기와 오분류율
evol.german <- errorevol(bag.german, newdata=german)
plot.errorevol(evol.german)
                  
set.seed(1234)
i = sample(1:nrow(german), round(nrow(german)*0.7)) #천개중에 70프로 샘플
i
german.train = german[i,] #training data 70%
german.test = german[-i,] #tested data 30%
bag.german <- bagging(y ~ ., data = german.train, mfinal=50, control=my.control)

#검증 데이터를 이용한 배깅 방법의 예측 성능
pred.bag.german <- predict.bagging(bag.train.german, newdata=german.test)
print(pred.bag.german$confusion)
1-sum(diag(pred.bag.german$confusion))/sum(pred.bag.german$confusion)

#부스팅 방법
my.control <- rpart.control(xval=0, cp=0, maxdepth=1)
boo.german <- boosting (y ~ ., data = german, boos=T, mfinal=100, control=my.control)
summary(boo.german)
boo.german$trees

#일단 여기까지 

p.test.bag = predict.bagging(fit.bag, newdata=german.test)$prob[,2] #특정 변수만
p.test.bag

threshold = 0.5
yhat.test.bag = ifelse(p.test.bag > threshold, levels(german$y)[2], levels(german$y)[1])

tab=table(german.test$y, yhat.test.bag, dnn=c("Observed", "Predicted"))
print(tab) #classification table
1-sum(diag(tab))/sum(tab) #misclassficiation rate
tab[,2]/apply(tab, 1, sum) #specificity/sensitivity

evol.german=errorevol(fit.bag,german.train)
plot.errorevol(evol.german)
my.control = rpart.control(xval=0, cp=0, maxdepth=1)
german.train$y = factor(german.train$y)

fit.boo = boosting(y ~ ., data = german.train, boos=T, mfinal=100, control=my.control)

p.test.boo= predict.boosting(fit.boo, newdata=german.test)$prob[,2]
yhat.test.boo = ifelse(p.test.boo > threshold, levels(german$y)[2], levels(german$y[1]))



