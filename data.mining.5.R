#데이터 마이닝 5장 신경망 모형
#neuralnet 함수 
#은닉층이 hidden에 지정된 신경망모형을 작성 
install.packages("neuralnet")
#함수의 구조 
neuralnet(formula, data, hidden=c(a,b)), linear.output=T, stepmax=1e+05, rep=1,
startweights=NULL, act.fct="logistic", linear.output=TRUE, constant.weights=NULL)
#formula = 목표변수 입력변수 지정
#hidden 첫번째 은닉층 a개 두번째 b개
#stepmax 신경망 학습을 위한 step 의 최댓값
#rep = 학습과정의 반복수
#startweights = 가중치의 초기값 
#act.fct=활성함수의 형태, 기본은 로지스틱
#linear.output = 기본 활성함수 형태가 출력층 적용시 T 아니면 F
#constant.weights = 학습과정에서 제외되어 상수로 간주되는 가중치의 벡터값 

#plot  함수
plot(y.nn)
#작성된 신경망모형의 구조 및 가중치를 그림으로 출력

#코사인 함수(cos(4pix))의 추정
library(neuralnet)
set.seed(130)
ind1=1:100 #수열 생성
ind2=ind1/100 #수열을 조밀하게 만든다
cos2=cos(ind2*4*pi) #코사인 함수 
cdat<-data.frame(cbind(ind2, cos2)) #입력값과 출력값으로 이루어진 데이터 생성 
cos2.nn=neuralnet(cos2~ind2, data=cdat, hidden=5, linear.output=T) #은닉층 5개 신경망 모형
plot(cos2.nn)
print(cos2.nn)

#위에서 neuralnet 으로 만든 모형을 compute 를 통해 적합시킴 
cos.pred <- compute(cos2.nn, cdat) #교재에는 ind2 라고 되어 있는데 neuralnet 패키지에는 데이터셋 입력해줘야 돌아감 
plot(ind1, cos.pred$net.result)
lines(cos2)
cos.pred$net.result #neuralnet 을 통한 예측값 
cos(4*pi*ind2) #실제값 

#보스턴 하우징 데이터 예제 - 회귀
set.seed(100)
library(MASS)
library(neuralnet)

bdat = Boston
bdat<-bdat[,-15]
bdat$chas = as.numeric(bdat$chas)
bdat$rad = as.numeric(bdat$rad)
class(bdat$chas);class(bdat$rad) #데이터 타입을 범주에서 수치형으로 변환 

i = sample(1:nrow(bdat), round(0.5*nrow(bdat))) #50% 랜덤추출
max1 <- apply(bdat, 2, max)
min1 <- apply(bdat, 2, min)

sdat=scale(bdat, center=min1, scale=max1-min1) #변수조정
sdat=as.data.frame(sdat)

train = sdat[i,] #학습샘플
test = sdat[-i,] #테스트샘플

n = names(train)
form = as.formula(paste("medv ~", paste(n[!n %in% "medv"], collapse= "+"))) #변수이름 사용하기 위해 추출
#종속변수를 제외한 n 안에 있는 모든 변수명을 다 집어넣고 더하여라.

nn1 = neuralnet (form, data=train, hidden=c(5,3), linear.output=T)
plot(nn1)

pred.nn1 = compute(nn1, test[,1:13])
pred1=pred.nn1$net.result*(max(bdat$medv)-min(bdat$medv))+min(bdat$medv)
#목표변수를 조정전 변수값으로 환원 

PMSE = sum((bdat[-i,14]-pred1)^2)/nrow(test) #회귀이기 때문에 (실제값-예측값)^2 의 평균
PMSE

#독일신용평가 데이터 
install.packages("dummy")
library(dummy)
set.seed(1000)
library(neuralnet)
library(dummy)
german = read.table('germandata.txt',header = T)

dvar=c(4,9,10,15,17) #명목변수 지정
german2 = dummy(x=german[,dvar]) #명목변수를 더미변수화
german2 = german2[,-c(10,14,17,20,24)] #중복변수 삭제 - 더미화의 일반적 방법 (변수개수-1)
german2 = cbind(german[,-dvar],german2) #변수 결합

for(i in 1:ncol(german2)) if(!is.numeric(german2[,i])) german2[,i] = as.numeric(german2[,i])
#여타 순서가 있는 범주형 변수의 수치형 변수화 
german2$y = ifelse(german$y == 'good',1,0) #목표변수 변환
#여기까지가 데이터 가공하는 과정 

i = sample(1:nrow(german2),round(0.75*nrow(german2))) #75% 랜덤추출 
max2 = apply(german2, 2, max)
min2 = apply(german2, 2, min)
gdat = scale(german2, center = min2, scale = max2 - min2) #변수조정 (0,1 dummy 변수는 변화없음)
gdat = as.data.frame(gdat)

train = gdat[i,] #학습샘플과 테스트 샘플 추출 
test = gdat[-i,]

gn = names(german2)
f = as.formula(paste('y~',paste(gn[!gn %in% 'y'],collapse = '+')))
nn1 = neuralnet(f,data=train,hidden=c(3,2),linear.output=F) #F=분류문제라서 
#은닉층 2개이고 각각 3,2개의 노드를 갖는 신경망모형
plot(nn1) 
plot(nn1, intercept=F, show.weights=F) #값을 안 보이게 해서 좀 단순하게 만든 plot

pred.nn0 = compute(nn1,train[,c(1:15,17:dim(german2)[2])])
cbind(german2[1,16],round(pred.nn0$net.result,10)) #학습샘플의 실제값과 예측값(적합값)

pred.nn1 = compute(nn1,test[,c(1:15,17:dim(german2)[2])])
pred.nn2 = ifelse(pred.nn1$net.result>0.5,1,0) #0.5를 경계로 1과 0 분류
cbind(german2[-i,16],pred.nn2) #테스트 샘플의 실제값과 예측값
sum(german2[-i,16]!=pred.nn2) / length(german2[-i,16]) #테스트 샘플 예측의 오분류율

library(nnet) #nnet 이용 (단일 은닉층 모형 만들어서 비교)
nnet1 =nnet(f,data=train,size = 3, linout = F)
pred.nnet1 = predict(nnet1,test[,c(1:15,17:dim(german2)[2])])
pred.nnet2 = ifelse(pred.nnet1>0.5,1,0)
head(cbind(german2[-i,16],pred.nnet2)) #테스트 샘플의 실제값과 예측값
sum(german2[-i,16]!=pred.nnet2) / length(german2[-i,16]) #테스트 샘플 예측의 오분류율



