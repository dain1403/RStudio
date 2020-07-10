setwd("c:/Rdata")
data = read.csv("programming.csv")
head(data)

## glm 을통해 로지스틱회귀모형을 fitting시킨다. 
## family='binomial' 인자를 통해, glm으로 로지스틱 회귀모형을 쓸 수 있다
model = glm(Success ~ Experience,data = data, family = binomial(logit))
summary(model)
cbind(data$Experience, model$fitted.values)
plot(Success~Experience, data = data)
points(model$fitted.values ~ data$Experience, col = 2)

table(data$Success, model$fitted.values > 0.5)
c('민감도' = 8/11, '특이도' = 11/24)

data = read.csv("coupon.csv")
head(data)
model2 = glm(cbind(N_redeemed, N-N_redeemed)~Price_reduc, data = data, 
    family = binomial(logit))
summary(model2)
exp(0.096834)

data = read.csv("disease.csv")
head(data)
model3 = glm(disease ~., data = data, family = binomial(logit))
summary(model3)

model4 = glm(disease~age+sector, data= data, family = binomial(logit))
summary(model4)
anova(model3, model4, test='Chisq')

table(data$disease)
32/98
kk = table(data$disease, model4$fitted.values > 0.3163265)
sum(kk)
reduce_M = c('민감도' = 23/31, '특이도' = 47/(47+20))
kk1 = table(data$disease, model4$fitted.values > 0.3163265)
kk1
fulmode_M = c('민감도' = 23/31, '특이도' = 49/(49+18))
reduce_M
fulmode_M
err_m1 = 28/sum(kk)
kk1
err_m2 = 26/sum(kk1)
err_m1
err_m2
install.packages("Deducer")
library(Deducer)
rocplot(model3)
rocplot(model4)













##########################################################
##### HW 로지스틱
## 연습문제 풀이

data = read.csv("flushot.csv")
head(data)
log_model = glm(flushot~., data= data, family = binomial(logit))
summary(log_model)
exp(0.07179)
exp(-0.09899)
exp(0.43397)
log_model12 = glm(flushot ~ age+aware,data= data, family = binomial())
summary(log_model12)
table(data$flushot)
24/(134+24)      ## --> 0.1518987
tt = table(data$flushot, log_model12$fitted.values>0.1518987)  ## 0.1518987 : 임계값
c('민감도' = 19/(5+19), '특이도' = 95/(95+40), '에러율' = 45/sum(tt))  ## 에러율45는 40+5해서 나온것
rocplot(log_model12)

tab_01 = table(data$flushot, log_model12$fitted.values>0.1)
tab_015 = table(data$flushot, log_model12$fitted.values>0.15)
tab_02 = table(data$flushot, log_model12$fitted.values>0.2)

tab_01
tab_015
tab_02
res01 = c('민감도' = tab_01[2,2]/sum(tab_01[2,1]), 
                                  '특이도' = tab_01[1,1]/sum(tab_01[1,]),
                                  '에러율' = (tab_01[1,2]+tab_01[2,1]/sum(tab_01)))
res01

res015 = c('민감도' = tab_015[2,2]/sum(tab_015[2,1]), 
          '특이도' = tab_015[1,1]/sum(tab_015[1,]),
          '에러율' = (tab_015[1,2]+tab_015[2,1]/sum(tab_015)))
res015

res02 = c('민감도' = tab_02[2,2]/sum(tab_02[2,1]), 
           '특이도' = tab_02[1,1]/sum(tab_02[1,]),
           '에러율' = (tab_02[1,2]+tab_02[2,1]/sum(tab_02)))
res02

model4$fitted.values

jang = function() {
  k = seq(0.01, 0.5, 0.01)
  
  n = length(k)
  
  err_min = vector(length = n)
  sens[i] = vector(length = n)
  spec[i] = vector(length = n)
  
  for(i in 1:n) :
    tab = table(data$flushot, log_model2$fitted.values = k[i])
    res =  c('민감도' = tab[2,2]/sum(tab[2,1]), 
             '특이도' = tab[1,1]/sum(tab[1,]),
             '에러율' = (tab[1,2]+tab_02[2,1]/sum(tab)))
  

  print(err_min)
  print(paste("최소의 error rate = ",min(err_min),"이다"))
  print(err_min <= min(err_min))
  print(index)
  print(paste("해당되는 민감도 = ",sens[min(index)],"이다."))
  print(paste("해당되는 특이도 = ",spec[min(index)],"이다."))
  print(paste("해당되는 에러율 = ",err_min[min(index)],"이다."))
  print(paste("해당되는 cutoff = ",k[min(index)],"이다."))
}
plot(1-spec, sens, col =2)

jang()









##########################################################################
############### Lecture12_다변량자료탐색_주성분분석   

crime = read.csv("http://datasets.flowingdata.com/crimeRatesByState-formatted.csv")
head(crime)

rownames(crime)

rownames(crime) = crime[, 1]
rownames(crime)

## draw.segments = TRUE : 색채우기
stars(crime[, 2:8], flip.labels = FALSE, draw.segments = TRUE, key.loc = 2)


## 체르노프페이스
install.packages("aplpack")
library(aplpack)
faces(crime[, 2:8])


education = read.csv("http://datasets.flowingdata.com/education.csv")
head(education)

library(lattice)
parallel(education[, 2:7])
parallel(education[, 2:7], horizontal.axis = FALSE)
parallel(education[, 2:7], horizontal.axis = FALSE, col = 1)
summary(education$reading)
color = education$reading>523
color
color+1
parallel(education[, 2:7], horizontal.axis = FALSE, col = color + 1)
summary(education$dropout_rate)

color = education$dropout_rate > 5.3
color
color+1
parallel(education[, 2:7], horizontal.axis = FALSE, col = color + 1)

data = read.csv("20140528_baseball.csv")
data

model = prcomp(data[, 2:6], scale = T)
model

summary(model)
plot(model)
head(data)
rownames(data) = data[,1]
head(data)

model = prcomp(data[, 2:6], scale = T)
biplot(model)
  


##########################################
###### 다변량 연습문제  - HW 다변량분석

####1번 20140528_baseball.csv는 2014년 5월 28일 현재 한국 프로야구 각 팀의 성적을 보여준다.이 자료를 이용해 별그림,체르노프페이스, 나이팅게일 차트를 적절한 label을 포함하여 그리고 비슷한 패턴을 가지는 그룹으로 나누어 각 그룹이 어떤 변수적 특징을 가지는지 서술하여라.
data = read.csv("20140528_baseball.csv")
head(data)
rownames(data) = data[,1]
head(data)
stars(data[, 2:6], flip.labels = F, key.loc = c(9,3), draw.segments = T)
faces(data[, 2:6])


#### 2번	2013_baseball.csv는 2013년 시즌 각 타자의 성적을 포함하고 있다.평행좌표 플롯을 활용해 선수들의 성적 패턴에 어떤 경향이 있는지 알아보려 한다.포지션 별,팀별 평행좌표 플롯을 그리고 각 포지션,혹은 팀별 타자의 성적에 어떤 경향이 있는지 서술하라.
bb2013 = read.csv("2013_baseball.csv")
head(bb2013)
position = bb2013$포지션
head(position)
base2_pos = bb2013[,c(2, 4:11)]
base2_pos2 = aggregate(base2_pos[, 2:9], by = list(포지션 = base2_pos$포지션), sum)
head(base2_pos2)
rownames(base2_pos2) = base2_pos2[,1]
head(base2_pos2)
library(lattice)
parallel(base2_pos2[,2:9], horizontal.axis = F, col =1)


# 팀별평행좌표
team = bb2013$팀
parallel(~bb2013[,4:11]|team,horizontal.axis = F, col =1)


######## 3번	2013_baseball.csv에 있는 각 타자에 대한 변수를 사용하여 주성분 분석을 시행하라.적당한 주성분의 개수를 파악하고 해당 주성분들로 설명되는 분산의 비율을 구하라.행렬도를 통해 선수들의 특징이 어떻게 파악되는지 살펴보시오.
rownames(bb2013) = bb2013[,1]
rownames(bb2013)
model = prcomp(bb2013[,4:11], scale = T)
plot(model)
summary(model)
biplot(model)





