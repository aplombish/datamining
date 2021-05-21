#6장. 모형비교평가 - 지금까지 배운 모형 비교 
#목표 변수가 연속형인 경우 모형의 예측력 측도 PMSE (prediction  mean squred error)
#6.1 평가모형

library(MASS)
Boston$chas = factor(Boston$chas)
Boston$rad = factor (Boston$rad)
summary(Boston)

#선형 회귀모형 적합 및 예측 결과 
set.seed(1234) 
i = sample(1:nrow(Boston), round(nrow(Boston)*0.7))
Boston.train = Boston[i,] #70% training data
Boston.test = Boston[-i,]

fit.reg = lm (medv ~ ., data = Boston.train)
fit.step.reg = step (fit.reg, direction="both", trace=FALSE) #Stepwise variable selection

yhat.reg = predict(fit.step.reg, newdata=Boston.test, type="response")
mean((Boston.test$medv - yhat.reg)^2) #PMSE

#교차타당도 - 우연히 특정 모형에 유리하게 분할될 가능성 존재하므로 여러 부분 분할하여 반복 검증
#회귀나무모형 적합 및 예측 결과
library(rpart)

set.seed(1234)
i = sample(1:nrow(Boston), round(nrow(Boston)*0.7))
Boston.train = Boston[i,] #70% training data
Boston.test = Boston[-i,]

my.control = rpart.control(xval=10, cp=0, minsplit=nrow(Boston.train)*0.05)
fit.tree = rpart (medv ~ ., data = Boston.train, method="anova", control=my.control)
ii = which.min(fit.tree$cp[,4]) #find min xerror
fit.prun.tree = prune(fit.tree, cp = fit.tree$cp[ii,1]) #Pruning
head(fit.prun.tree)
yhat.tree = predict(fit.prun.tree, newdata=Boston.test, type="vector")
mean((Boston.test$medv - yhat.tree)^2)

#신경망 모형 적합 및 예측결과
library(neuralnet)
Boston2 = Boston
for(i in 1: ncol(Boston2)) if (!is.numeric(Boston2[,i])) Boston2[,i]=as.numeric(Boston2[,i])

max1 = apply(Boston2, 2, max)
min1 = apply(Boston2, 2, min)

sdat = scale(Boston2, center = min1, scale = max1 - min1)
sdat = as.data.frame(sdat)

set.seed(1234)
i = sample(1:nrow(Boston2), round(nrow(Boston2)*0.7))
Boston2.train = sdat[i,]
Boston2.test = sdat[-i,]

#neuralnet 에서 원하는 방식으로 모든 입력변수를 다 입력을 해줘야 하는데 간단하게 한 것 
vname = names(Boston2.train)
form = as.formula(paste("medv ~", paste(vname[!vname %in% "medv"], collapse="+")))
form
fit.nn = neuralnet(form, data=Boston2.train, hidden=c(3,2), linear.output=T)
plot(fit.nn)

pred <- compute(fit.nn,Boston2.test[,1:13]) #작성한 모형을 다른 데이터에 적용
#Boston2 의 입력변수 제외한 값들만 넣은 데이터 
#여기서 에러남 
pred
Boston2.test
yhat.nn = pred$net.result*(max(Boston2$medv)-min(Boston2$medv))+min(Boston2$medv)
Boston2.test$medv = Boston2.test$medv*(max(Boston2$medv)-min(Boston2$medv))+min(Boston2$medv)
mean((Boston2.test$medv - yhat.nn)^2) #PMSE

#랜덤 포레스트 적합 및 예측 결과
library(randomForest)

set.seed(1234)
i = sample(1:nrow(Boston), round(nrow(Boston)*0.7))
Boston.train = Boston[i,]
Boston.test = Boston[-i,]

fit.rf = randomForest(medv ~ ., data = Boston.train, ntree=100, mtry=5, importance=T, 
                      na.action=na.omit)
yhat.rf=predict(fit.rf, newdata=Boston.test, type="response")
mean((Boston.test$medv - yhat.rf)^2)

#관측치와 예측치의 산점도 생성
par(mfrow=c(2,2))

plot(Boston.test$medv,yhat.reg, xlab="Observed",ylab="Predicted", main="Regression",
     xlim=c(0,55), ylim=c(0,55))
abline(a=0, b=1, lty=2)

plot(Boston.test$medv, yhat.tree, xlab="Observed",ylab="Predicted", main="decision tree",
     xlim=c(0,55), ylim=c(0,55))
abline(a=0, b=1, lty=2)

plot(Boston.test$medv, yhat.nn, xlab="Observed",ylab="Predicted", main="neural network",
      xlim=c(0,55), ylim=c(0,55))
abline(a=0, b=1, lty=2)

plot(Boston.test4medv, yhat.rf, xlab="Observed",ylab="Predicted", main="random forest",
     xlim=c(0,55), ylim=c(0,55))
abline(a=0, b=1, lty=2)

#독일신용평가데이터분석
german = read.table("germandata.txt", header=T)
german$numcredits = factor (german$numcredits)
german$residence = factor(german$residence)
german$residpeople = factor(german$residpeople)
german$y = relevel(german$y, ref="bad")
german$y = ifelse(german$y == "bad", 0, 1)
head(german)  
threshhold=0.5

#로지스틱 회귀모형 적합 및 예측 결과
set.seed(1234)
i = sample(1:nrow(german), round(nrow(german)*0.7))
german.train = german[i,]
german.test = german[-i,]

head(german)
fit.reg = glm(y ~ ., family = binomial(link = "logit"), data = german.train)
fit.step.reg = step(fit.reg, direction="both", trace=F)

p.test.reg = predict(fit.step.reg, newdata=german.test, type="response") #probabilities
yhat.test.reg = ifelse(p.test.reg > threshold, 1, 0)
#왜 굳이 이래놨지
#yhat.test.reg = ifelse(p.test.reg > threshold, levels(german$y)[2], levels(german$y)[1])

tab = table(german.test$y, yhat.test.reg, dnn=c("Observed", "Predicted"))
print(tab) #classification table

1-sum(diag(tab))/sum(tab) #misclassification rate
tab[,2]/apply(tab,1,sum) #specificity/sensitivity

#분류나무모형 적합 및 예측 결과
library(rpart)
set.seed(1234)
i = sample(1:nrow(german), round(nrow(german)*0.7))
german.train = german[i,]
german.test = german[-i,]

my.control = rpart.control(xval=10, cp=0, minsplit=5)
fit.tree = rpart(y ~ ., data = german.train, method="class", control=my.control)
ii = which.min(fit.tree$cp[,4]) #find min xerror
fit.prun.tree = prune(fit.tree, cp=fit.tree$cp[ii,1])

p.test.tree = predict(fit.prun.tree, newdata=german.test, type="prob")[,2]#probabilities
yhat.test.tree = ifelse(p.test.tree > threshold, 1, 0)

tab = table(german.test$y, yhat.test.tree, dnn=c("Observed", "Predicted"))
print(tab)

1-sum(diag(tab))/sum(tab) #misclassification rate
tab[,2]/apply(tab,1,sum) #specificity/sensitivity

#신경망 모형 적합 및 예측결과
library(neuralnet)
library(dummy)

dvar=c(4,9,10,15,17) #명목변수 지정
german2 = dummy(x=german[,dvar]) #명목변수를 더미변수화
german2 = german2[,-c(10,14,17,20,24)] #중복변수 삭제 - 더미화의 일반적 방법 (변수개수-1)
german2 = cbind(german[,-dvar],german2) #변수 결합
for(i in 1: ncol(german2)) if(!is.numeric(german2[,i])) german2[,i]=as.numeric(german2[,i])
german2$y = ifelse(german$y =="good", 1, 0)

max1 = apply(german2, 2, max)
min1 = apply(german2, 2, min)

gdat = scale(german2, center = min1, scale = max1 - min1)
gdat = as.data.frame(gdat)

i = sample(1:nrow(german2), round(nrow(german2)*0.7))
german2.train = gdat[i,]
german2.test = gdat[-i,]

gn = names(german2.train)
f = as.formula(paste("y ~", paste(gn[!gn %in% "y"], collapse="+")))
f
fit.nn = neuralnet(f, data=german2.train, hidden=c(3,2), linear.output=F)
#이 부분에서 에러남 

p.test.nn = compute(fit.nn, german2.test[,-16])$net.result
yhat.test.nn = ifelse(p.test.nn > threshold, 1, 0) 

tab = table(german.test$y, yhat.test.nn, dnn=c("Observed", "Predicted"))
print(tab)

1-sum(diag(tab))/sum(tab) #misclassification rate
tab[,2]/apply(tab,1,sum) #specificity/sensitivity

#배깅 적합 및 예측 결과
german = read.table("C:/rdata/germandata.txt", header=T)
german$numcredits = factor (german$numcredits)
german$residence = factor (german$residence)
german$residpeople = factor(german$residpeople)
german$y = factor(german$y)
german$y = relevel(german$y, ref="bad") #reference level 0 = "bad"

library(rpart)
library(adabag)

set.seed(1234)
i = sample(1:nrow(german), round(nrow(german)*0.7))
german.train = german[i,]
german.test = german[-i,]

head(german.train)
str(german.train)
my.control = rpart.control(xval=0, cp=0, minsplit=5, maxdepth=10)
fit.bag = bagging(y ~ ., data =german.train, mfinal=50, control=my.control) #에러

p.test.bag = predict.bagging(fit.bag, newdata=german.test)$prob[,2]
yhat.test.bag = ifelse(p.test.bag > threshold, 1, 0)

tab = table(german.test$y, yhat.test.bag, dnn=c("Observed","Predicted"))
print(tab)

1-sum(diag(tab))/sum(tab) #misclassification rate
tab[,2]/apply(tab,1,sum) #specificity/sensitivity
      

#부스팅 적합 및 예측 결과
library(rpart)
library(adabag)

set.seed(1234)
i = sample(1:nrow(german), round(nrow(german)*0.7))
german.train = german[i,]
german.test = german[-i,]

my.control = rpart.control(xval=0, cp=0, maxdepth=10)
fit.boo = boosting(y ~ ., data =german.train, mfinal=100, control=my.control) #y가 factor 여야 작동 numeric 불가 

p.test.boo = predict.boosting(fit.boo, newdata=german.test)$prob[,2]
yhat.test.boo = ifelse(p.test.boo > threshold, 1, 0)

tab = table(german.test$y, yhat.test.boo, dnn=c("Observed", "Predicted"))
print(tab)

1-sum(diag(tab))/sum(tab) #misclassification rate
tab[,2]/apply(tab,1,sum) #specificity/sensitivity

#ROC 및 AUC 결과 도출
library(ROCR)

pred.reg = predict(p.test.reg, german.test$y); perf.reg = performance(pred.reg, "tpr", "fpr")
pred.tree = predict(p.test.tree, german.test$y); perf.tree = performance(pred.tree, "tpr", "fpr")
pred.nn = predict(p.test.nn, german.test$y); perf.nn = performance(pred.nn, "tpr", "fpr")
pred.bag = predict(p.test.bag, german.test$y); perf.bag = performance(pred.bag, "tpr", "fpr")
pred.boo = predict(p.test.boo, german.test$y); perf.reg = performance(pred.boo, "tpr", "fpr")
pred.rf = predict(p.test.rf, german.test$y); perf.rf = performance(pred.rf, "tpr", "fpr")

#draw ROC
plot(perf.reg, lty=1, col=1, xlim=c(0,1), ylim=c(0,1), xlab="1-specificity", ylab="sensitivity", main="ROC Curve")
plot(perf.tree, lty=2, col=2, add=T)
plot(perf.nn, lty=3, col=3, add=T)
plot(perf.bag, lty=4, col=4, add=T)
plot(perf.boo, lty=5, col=5, add=T)
plot(perf.rf, lty=6, col=6, add=T)
lines(x=c(0,1), y=c(0,1), col="grey")
legend(0.6, 0.3, c("Regression", "Decision Tree", "Neural Network", "Bagging", "Boosting", "Random Forest"), lty=1:6, col=1:6)

#compute auc
performance(pred.reg, "auc")@y.values #regression
performance(pred.tree, "auc")@y.values #regression
performance(pred.nn, "auc")@y.values #regression
performance(pred.bag, "auc")@y.values #regression
performance(pred.boo, "auc")@y.values #regression
performance(pred.rf, "auc")@y.values #regression
