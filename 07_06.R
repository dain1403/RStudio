#################### Doit_part7 ################################

# 결측치 표기 - 대문자 NA
df <- data.frame(sex = c("M", "F", NA, "M", "F"),
                 score = c(5, 4, 3, 4, NA))
df


# 결측치 확인하기
is.na(df)         # 결측치 확인
table(is.na(df))  # 결측치 빈도 출력


# 변수별로 결측치 확인하기
table(is.na(df$sex))    # sex 결측치 빈도 출력
table(is.na(df$score))  # score 결측치 빈도 출력


# 결측치 포함된 상태로 분석
mean(df$score)  # 평균 산출
sum(df$score)   # 합계 산출


# 결측치 있는 행 제거하기
library(dplyr) # dplyr 패키지 로드
df %>% filter(is.na(score))   # score가 NA인 데이터만 출력

df %>% filter(!is.na(score))  # score 결측치 제거


# 결측치 제외한 데이터로 분석하기
df_nomiss <- df %>% filter(!is.na(score))  # score 결측치 제거
mean(df_nomiss$score)                      # score 평균 산출

sum(df_nomiss$score)                       # score 합계 산출


# 여러 변수 동시에 결측치 없는 데이터 추출하기
# score, sex 결측치 제외
df_nomiss <- df %>% filter(!is.na(score) & !is.na(sex))
df_nomiss  


# 결측치가 하나라도 있으면 제거하기
df_nomiss2 <- na.omit(df)  # 모든 변수에 결측치 없는 데이터 추출
df_nomiss2                 


# 함수의 결측치 제외 기능 이용하기 - na.rm = T
mean(df$score, na.rm = T)  # 결측치 제외하고 평균 산출

sum(df$score, na.rm = T)   # 결측치 제외하고 합계 산출


# summarise()에서 na.rm = T사용하기
# •	결측치 생성
setwd("c:/Rdata")  
exam <- read.csv("csv_exam.csv")            # 데이터 불러오기
exam[c(3, 8, 15), "math"] <- NA             # 3, 8, 15행의 math열에 NA 할당

exam
kk = table(is.na(exam$math))
tt = barplot(kk, col = rainbow(2),ylim = c(0,20))
text(tt,kk, label=kk, pos=3)


# •	평균 구하기
exam %>% summarise(mean_math = mean(math))             # 평균 산출

exam %>% summarise(mean_math = mean(math, na.rm = T))  # 결측치 제외하고 평균 산출


# 다른 함수들에 적용
exam %>% summarise(mean_math = mean(math, na.rm = T),      # 평균 산출
                   sum_math = sum(math, na.rm = T),        # 합계 산출
                   median_math = median(math, na.rm = T))  # 중앙값 산출



# 평균값으로 결측치 대체하기
# 평균 구하기
mean(exam$math, na.rm = T)  # 결측치 제외하고 math 평균 산출

# 평균으로 대체하기
exam$math <- ifelse(is.na(exam$math), 55, exam$math)  # math가 NA면 55로 대체
table(is.na(exam$math))                               # 결측치 빈도표 생성

exam
mean(exam$math)  # math 평균 산출



########### 07 - 01 혼자해보기 ############

mpg <- as.data.frame(ggplot2::mpg)           # mpg 데이터 불러오기
mpg[c(65, 124, 131, 153, 212), "hwy"] <- NA  # NA 할당하기

# Q1
table(is.na(mpg$drv))
table(is.na(mpg$hwy))

# Q2
library(dplyr) # dplyr 패키지 로드
df_ec_hwy <- mpg %>% filter(!is.na(hwy)) %>% group_by(drv) %>% 
  summarise(mean_hwy = mean(hwy))
df_ec_hwy




############# 07 - 02 이상한 데이터를 찾아라! - 이상치 정제하기

# 이상치 포함된 데이터 생성 - sex 3, score 6
outlier <- data.frame(sex = c(1, 2, 1, 3, 2, 1),
                      score = c(5, 4, 3, 4, 2, 6))
outlier

table(outlier$sex)
table(outlier$score)

# 결측 처리하기 - sex
# sex가 3이면 NA 할당
outlier$sex <- ifelse(outlier$sex == 3, NA, outlier$sex)
outlier

# 결측 처리하기 - score
# sex가 1~5 아니면 NA 할당
outlier$score <- ifelse(outlier$score > 5, NA, outlier$score)
outlier

#결측치 제외하고 분석
outlier %>%
  filter(!is.na(sex) & !is.na(score)) %>%
  group_by(sex) %>%
  summarise(mean_score = mean(score))


# 상자그림으로 극단치 기준 정해서 제거하기
# 상자그림 생성
mpg <- as.data.frame(ggplot2::mpg)
boxplot(mpg$hwy)

#상자그림 통계치 출력
boxplot(mpg$hwy)$stats  # 상자그림 통계치 출력

# 결측 처리하기
# 12~37 벗어나면 NA 할당
mpg$hwy <- ifelse(mpg$hwy < 12 | mpg$hwy > 37, NA, mpg$hwy)
table(is.na(mpg$hwy))

# 결측치 제외하고 분석하기
mpg %>%
  group_by(drv) %>%
  summarise(mean_hwy = mean(hwy, na.rm = T))



########## 07-2 혼자서 해보기 ############3

mpg <- as.data.frame(ggplot2::mpg)                  # mpg 데이터 불러오기
mpg[c(10, 14, 58, 93), "drv"] <- "k"                # drv 이상치 할당
mpg[c(29, 43, 129, 203), "cty"] <- c(3, 4, 39, 42)  # cty 이상치 할당

# Q1
mpg$drv <- ifelse(mpg$drv %in% c(4,"f","r"), mpg$drv, NA)
table(mpg$drv)

# Q2
boxplot(mpg$cty)
boxplot(mpg$cty)$stats
mpg$cty <- ifelse(mpg$cty < 9 | mpg$cty > 26, NA, mpg$cty)
table(is.na(mpg$cty))
boxplot(mpg$cty)

# Q3
mpg %>% filter(!is.na(drv)) %>% 
  group_by(drv) %>% summarise(mean_cty = mean(cty, na.rm = T))









############################ Doit_part8 그래프 만들기 ###########

library(ggplot2)

# 1. 배경 설정하기
# x축 displ, y축 hwy로 지정해 배경 생성
ggplot(data = mpg, aes(x = displ, y = hwy))

# 2. 그래프 추가하기
# 배경에 산점도 추가
ggplot(data = mpg, aes(x = displ, y = hwy)) + geom_point()

# 3. 축 범위를 조정하는 설정 추가하기
# x축 범위 3~6으로 지정
ggplot(data = mpg, aes(x = displ, y = hwy)) + geom_point() + xlim(3, 6)

# 3. 축 범위를 조정하는 설정 추가하기
# x축 범위 3~6, y축 범위 10~30으로 지정
ggplot(data = mpg, aes(x = displ, y = hwy)) + 
  geom_point() + 
  xlim(3, 6) + 
  ylim(10, 30)


# ggplot2 코드 가독성 높이기
# •	한 줄로 작성
ggplot(data = mpg, aes(x = displ, y = hwy)) + geom_point() + xlim(3, 6) + ylim(10, 30)

# •	+ 뒤에서 줄 바꾸기
ggplot(data = mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  xlim(3, 6) +
  ylim(10, 30)




######## 08-02 혼자서 해보기 ##############

# Q1
ggplot(data = mpg, aes(x = cty, y = hwy)) + geom_point()

# Q2
midwest <- as.data.frame(ggplot2::midwest)
ggplot(data = midwest, aes(x = poptotal, y=popasian)) + geom_point() +
  xlim(0,500000) + ylim(0,10000)





########## 08 - 03 막대 그래프 - 집단 간 차이 표현하기 #############

# 1. 집단별 평균표 만들기
library(dplyr)
mpg <- as.data.frame(ggplot2::mpg)

df_mpg <- mpg %>%
  group_by(drv) %>%
  summarise(mean_hwy = mean(hwy))

df_mpg


# 2. 그래프 생성하기
# geom_col() :	평균 막대 그래프 - 데이터를 요약한 평균표를
#                                  먼저 만든 후 평균표를 이용해 그래프 생성 
ggplot(data = df_mpg, aes(x = drv, y = mean_hwy)) + geom_col()

# 3. 크기 순으로 정렬하기
ggplot(data = df_mpg, aes(x = reorder(drv, -mean_hwy), y = mean_hwy)) + geom_col()



# 막대 그래프 2 - 빈도 막대 그래프
# •	값의 개수(빈도)로 막대의 길이를 표현한 그래프
# x축 범주 변수, y축 빈도
# geom_bar() : 빈도 막대 그래프 -  별도로 표를 만들지 않고 
#                                   원자료를 이용해 바로 그래프 생성 
ggplot(data = mpg, aes(x = drv)) + geom_bar()

# x축 연속 변수, y축 빈도
ggplot(data = mpg, aes(x = hwy)) + geom_bar()





######## 08-03 혼자서 해보기 ##############

# Q1
mpg
df_menu_suv <- mpg %>%filter(class == "suv") %>% 
  group_by(manufacturer) %>% summarise(mean_cty = mean(cty)) %>% 
  arrange(desc(mean_cty)) %>% head(5)

df_menu_suv

ggplot(data = df_menu_suv, aes(x = reorder(manufacturer, -mean_cty), y = mean_cty)) + geom_col()


# Q2
df_menu_class <- mpg %>% select(class)
df_menu_class
ggplot(data = df_menu_class, aes(x = class)) + geom_bar()




########## 08-4. 선 그래프 - 시간에 따라 달라지는 데이터 표현하기 ########## 

# 시계열 그래프 만들기
ggplot(data = economics, aes(x = date, y = unemploy)) + geom_line()



######## 08-04 혼자서 해보기 ##############
ggplot(data = economics, aes(x = date, y = psavert)) + geom_line()











########## 08-5. 상자 그림 - 집단 간 분포 차이 표현하기 ##########

# 상자 그림 만들기
ggplot(data = mpg, aes(x = drv, y = hwy)) + geom_boxplot()






######## 08-05 혼자서 해보기 ##############
df <- mpg %>% filter(class %in% c("compact","subcompact","suv")) 

ggplot(data = df, aes(x = class, y = cty)) + geom_boxplot()

















