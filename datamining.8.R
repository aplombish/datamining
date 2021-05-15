#8장. 연관성 분석
#basket 형태로 거래자료 읽기 
install.packages("arules")
library(arules)
tr1 <- read.transactions("trans1.txt", format="basket", sep=",")
as(tr1, "data.frame")

rules1 <- apriori(tr1, parameter=list(supp=0.4, conf=0.4))
inspect(rules1)

#income 데이터 분석(arules에 내장)
data(Income)
str(Income)

rules <- apriori(Income, parameter=list(supp=0.4, conf=0.8))

#연관규칙의 시각화 
install.packages("arulesViz")
library(arulesViz)
plot(rules)
plot(rules, method="grouped")
plot(rules, method="graph")
plot(rules, method="paracoord")







