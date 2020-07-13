install.packages("dplyr")
install.packages("ggplot2")
install.packages("caret")
library(dplyr)
library(ggplot2)
library(caret)

## 데이터 불러오기
#setwd("C:/Users/dain1/Desktop/")
setwd("c:/Rdata")
data = read.csv("ai_sales_data.csv")
head(data)

## 데이터 구성 - 데이터는10개의 변수와 120개의 object로 구성되어 있음.
str(data)

## data1 --> 비타민음료만
data1 = data %>% filter(CATEGORY == "비타민음료")
str(data1)
head(data1)

## data1 - 영향변수들만
data1_eff = data1 %>% select(-X, -YM, -CATEGORY)  ## -x ? --> -X 하면 both에서 saleday안나옴
data1_eff

## data2 --> 스포츠,이온음료만
data2 = data %>% filter(CATEGORY == "스포츠,이온음료")
str(data2)
head(data2)

## data2 - 영향변수들만
data2_eff = data2 %>% select(-X, -YM, -CATEGORY)  ## -X ?
data2_eff



## shapiro.test : 데이터가 정규 분포를 따르는지 샤피로 윌크 검정을 수행한다
### 방법 : 일반 탄산음료와 과즙음료를 비교 분석하여 진행.
### 각각 하스토그램과 정규성 테스트를 통하여 기준을 p_value>0.05로 선정.

shapiro.test(data1$QTY)
shapiro.test(data2$QTY)

hist(data1$QTY)
hist(data2$QTY)


## 상관관계분석
cor(data1_eff)
cor(data2_eff)

## 다중회귀분석
## R에서는 step() 함수를 사용하여 변수선택을 결정할 수 있다.
## direction이 변수선택 방법을 지정하는 것이다. direction이 both이면 단계이고, forward이면 전진, backward이면 후진이 된다.
## QTY를 종속변수로 하고, 나머지 변수를 모두 독립변수로 한 다중회귀분석
out1 = lm(QTY~., data = data1_eff)
out2 = lm(QTY~., data = data2_eff)
both1 = step(out1, direction="both",trace = FALSE)
both2 = step(out2, direction="both",trace = FALSE)
summary(both1)
summary(both2)


## 추정을 위한 회귀모형에 따른 유의성 검증과 잔차 분석
## 분산분석 (ANOVA : Analysis Of Variance)    ----> 더알아보기!!

anova(both1)
anova(both2)

par(mfrow = c(2,2))
plot(lm(QTY~.,data=data1_eff))


## 예측모형 검증
data1_eff = data1_eff %>% 
  mutate(QTY_pred = -1054 + 22.46*ITEM_CNT + 0.6854*PRICE + 8.875 * MAXTEMP + 0.006731*RAIN_DAY)
data1_eff

data2_eff = data2_eff %>% 
  mutate(QTY_pred =2328 - 3.122*PRICE + 66.72 * MAXTEMP + 0.01273*SALEDAY + 76.38*HOLIDAY)
data2_eff

## train_set, test_set
idx = sample(1:nrow(data1_eff),size = nrow(data1_eff)*0.7, replace = F)

#train.idx = createDataPartition(data1_eff$QTY, p = 0.7, list = F)  

data1_train = data1_eff[train.idx, ]
data1_test = data1_eff[-train.idx, ]

## 70프로 30프로 나눈 것 확인
dim(data1_train)
dim(data1_test)





## Accurancy
data1_eff = data1_eff %>%
  mutate(QTY_acc = (QTY_pred/QTY) * 100)
data1_eff

