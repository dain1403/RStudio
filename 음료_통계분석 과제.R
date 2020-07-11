install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)

## 데이터 불러오기
setwd("C:/Users/dain1/Desktop/")
data = read.csv("ai_sales_data.csv")
head(data)

## 데이터 구성 - 데이터는10개의 변수와 120개의 object로 구성되어 있음.
str(data)

## data1 --> 비타민음료만
data1 = data %>% filter(CATEGORY == "비타민음료")
str(data1)
head(data1)

## data1 - 영향변수들만
data1_eff = data1 %>% select(-X, -YM, -CATEGORY)
data1_eff

## data2 --> 스포츠,이온음료만
data2 = data %>% filter(CATEGORY == "스포츠,이온음료")
str(data2)
head(data2)

## data2 - 영향변수들만
data2_eff = data2 %>% select(-X, -YM, -CATEGORY)
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

