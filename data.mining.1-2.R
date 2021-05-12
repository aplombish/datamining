#1장. 데이터 과학과 데이터 마이닝
#보스턴 데이터 전처리
library(MASS)
i <- which(Boston$medv == 50)
boston$chas = factor(boston$chas)
boston$rad = factor(boston$rad)
boston$y = factor(boston$y)

#독일 신용평가 데이터 전처리
german = read.table("germandata.txt", header=T)
german$numcredits = factor (german$numcredits)
german$residence = factor(german$residence)
german$residpeople = factor(german$residpeople)
german$y = relevel(german$y, ref="bad")
german$y = ifelse(german$y == "bad", 0, 1)

#numeric 이어야 하는 경우
dvar=c(4,9,10,15,17) #명목변수 지정
german2 = dummy(x=german[,dvar]) #명목변수를 더미변수화
german2 = german2[,-c(10,14,17,20,24)] #중복변수 삭제 - 더미화의 일반적 방법 (변수개수-1)
german2 = cbind(german[,-dvar],german2) #변수 결합
for(i in 1: ncol(german2)) if(!is.numeric(german2[,i])) german2[,i]=as.numeric(german2[,i])
german2$y = ifelse(german$y =="good", 1, 0)

#2장. 회귀모형
library(MASS)
i = which(Boston$medv == 50) #which = find the values
boston = Boston[-i,] # delete cases with medv=50
boston$chas = factor(boston$chas)
boston$rad = factor(boston$rad)

#보스턴 데이터 선형회귀 
fit.all = lm(medv ~ ., data = boston) # fit a linear model with all variables
fit.all
summary(fit.all)
fit.step = step(fit.all, direction="both") #stepwise selection
fit.step$anova #변수선택과정 보여준다 
yhat = predict(fit.step, newdata=boston, type="response") #predictions
print(yhat)
plot(boston$medv, fit.step$fitted, xlim=c(0,50), ylim=c(0,50), xlab="Observed Values", ylab="Fitted Values")
abline(a=0, b=1)
mean((boston$medv - yhat)^2) #MSE


#독일신용평가 데이터 로지스틱 회귀분석 
german = read.table("C:/rdata/germandata.txt", header=T)
head(german)
german$numcredits = factor (german$numcredits)
german$residence = factor (german$residence)
german$residpeople = factor(german$residpeople)
german$y = ifelse(german$y == "good", 1, 0) #good = 1, bad = 0

fit.all = glm(y ~ ., family = binomial, data=german)
fit.step = step(fit.all, direction="both") #stepwise variable selection
fit.step$anova
summary (fit.step)

p = predict(fit.step, newdata=german, type="response") #prediction
threshold = 0.5 #cutoff
yhat = ifelse(p > threshold, 1, 0)

yhat

class.tab = table(german$y, yhat, dnn=c("Actual", "Predicted"))
print(class.tab) #classification table

sum(german$y==yhat)/length(german$y) #prediction accuracy
sum(german$y!=yhat)/length(german$y) #Misclassification Rate
class.tab[1,1]/apply(class.tab, 1, sum)[1] #Specficity
class.tab[2,2]/apply(class.tab, 1, sum)[2] #Sensitivity

install.packages('ROCR')
library(ROCR)

pred <- prediction(p, german$y)
perf <- performance (pred, "tpr","fpr")
plot(perf, lty=1, col=2, xlim=c(0,1), ylim=c(0,1), xlab="1-Specificity", ylab="Sensitivity", main="ROC Curve")
lines (x=c(0,1), y=c(0,1), col="grey")

performance(pred, "auc")@y.values #AUC
