###### Doit_part03

x=rnorm(100,175,2) ## random normality  ## 데이터 개수 , 평균, 표준편차
x
hist(x, breaks = 5, probability = T)
lines(density(x), col =2, type = 'h', lwd=0.5)
shapiro.test(x)

a <- 1
a

a = 2
a

var1 <- c(1, 2, 5, 7, 8)    # 숫자 다섯 개로 구성된 var1 생성
var1

var2 <- c(1:5)
var2

var3 = seq(1,5)
var3

var4 = seq(1,10,by=2) ## 1부터 10까지 2간격으로 연속값
var4

var1
var1 + 2

var2
var1 + var2

str1 = "a"
str1

str4 = c("a","b","c")
str4

str5 = c("i","am","a boy")
str5
str5_paste <- paste(str5, collapse = " ")
str5_paste

x = c(1,4,7)
x
mean(x)
min(x)
sd(x)  ##표준편차


###### 03-3

install.packages("ggplot2")  ## 설치
library(ggplot2)    ##로드


x <- c("a", "a", "b", "c")  # 여러 문자로 구성된 변수 생성
x

qplot(x)  # 빈도 그래프 출력


# x축 drv, y축 hwy, 상자 그림 형태, drv별 색 표현
qplot(data = mpg, x = drv, y = hwy, geom = "boxplot", colour = drv)
mpg   ## ggplot2에있는 data

?qplot

### 혼자서해보기

student = c(80, 60, 70, 50, 90)
student

mean(student)
sd(student)


############################################################


#### DOit_part04

english <- c(90, 80, 60, 70)  # 영어 점수 변수 생성
english

math <- c(50, 60, 100, 20)    # 수학 점수 변수 생성
math

df_midterm <- data.frame(english, math)
df_midterm

class <- c(1, 1, 2, 2)
class

df_midterm$english  ## df_midterm안에 english만 보이도록
mean(df_midterm$english)  
sd(df_midterm$english)
sqrt(df_midterm$english)
sqrt(var(df_midterm$english))


###혼자서해보기

# 데이터 프레임 만들기
sales <- data.frame(fruit = c("사과", "딸기", "수박"),
                    price = c(1800, 1500, 3000),
                    volume = c(24, 38, 13))

sales

mean(sales$price) # 가격 평균
mean(sales$volume) # 판매량 평균




########## 04-3

install.packages("readxl")  # readxl 패키지 설치
library(readxl)   # readxl 패키지 로드


## 에서 새로운 워킹 디렉토리를 지정하려면 setwd() 명령어를 이용
setwd("c:/Rdata")  
df_midterm
write.csv(df_midterm, "df_midterm.csv")

df_mid_test <- read.csv("df_midterm.csv")
df_mid_test



plot.new()

hist(mpg$hwy,probability = T)
lines(density(mpg$hwy), col = 2, type = 'h', lwd =1)


##########################################################
#### Doit_parta05

exam <- read.csv("csv_exam.csv")
exam

head(exam,15) # 앞에서부터 15행까지 출력
tail(exam,10)

View(exam)  #뷰어 창에서 데이터 확인하기

dim(exam)  # 행, 열 출력

str(exam)  # 데이터 속성 확인

summary(exam)  # 요약통계량 출력

boxplot(exam$math, horizontal = T, col=2)
hist(exam$math)

x = sample(0:100, 80, replace = T) #0~100까지 80개의 데이터,replace는 겹쳐도 된다는것 
plot(x, pch=ifelse(x>=60, 7, 15))  ## x>=60  참이면 pch = 7, 거짓이면 15
abline(h = 60)  #수평으로 60에 직선

mpg=as.data.frame(ggplot2::mpg) # ggplo2의 mpg 데이터를 데이터 프레임 형태로 불러오기
head(mpg)



######## 05 -2 데이터 수정하기 - 변수명 바꾸기


install.packages("dplyr")  # dplyr 설치
library(dplyr)             # dplyr 로드

df_raw <- data.frame(var1 = c(1, 2, 1), var2 = c(2, 3, 2))
df_raw

df_new <- df_raw  # 복사본 생성
df_new            

df_new <- rename(df_new, v2 = var2)  # var2를 v2로 수정
df_new


###### 혼자서 해보기  - 제출하기

mpg_df = as.data.frame(ggplot2::mpg)
mpg_df

mpg_copy_df = mpg_df

mpg_copy_df <- rename(mpg_copy_df, city = cty)
mpg_copy_df <- rename(mpg_copy_df, highway = hwy)

head(mpg_copy_df)



########### 05-3 파생변수 만들기

df <- data.frame(var1 = c(4, 3, 8),
                 var2 = c(2, 6, 1))
df

df$var_sum <- df$var1 + df$var2  # var_sum 파생변수 생성
df

df$var_mean <- (df$var1 + df$var2)/2  # var_mean 파생변수 생성
df

mpg$total <- (mpg$cty + mpg$hwy)/2  # 통합 연비 변수 생성
head(mpg)

mean(mpg$total)


### 조건문을 활용해 파생변수 만들기

summary(mpg$total)  # 요약 통계량 산출

hist(mpg$total)     # 히스토그램 생성

# 20 이상이면 pass, 그렇지 않으면 fail 부여
mpg$test <- ifelse(mpg$total >= 20, "pass", "fail")

head(mpg, 20) # 데이터 확인

table(mpg$test)  # 연비 합격 빈도표 생성

library(ggplot2)  # ggplot2 로드
qplot(mpg$test)   # 연비 합격 빈도 막대 그래프 생성


#### 분석 도전 
# 문제 1
midwest=as.data.frame(ggplot2::midwest)
head(midwest)
tail(midwest)

# 문제 2
library(dplyr)
midwest <- rename(midwest, total = poptotal)
midwest <- rename(midwest, asian = popasian)

# 문제 3
midwest$ratio <- midwest$asian/midwest$total*100
hist(midwest$ratio)

# 문제 4
mean(midwest$ratio)

midwest$group <- ifelse(midwest$ratio > 0.4872462, "large", "small")

# 문제 5
table(midwest$group)

library(ggplot2)
qplot(midwest$group)




####################################################################
##### Doit_part06 자유자재로 데이터 가공하기


### 조건에 맞는 데이터만 추출하기

library(dplyr)
exam <- read.csv("csv_exam.csv")
exam

# exam에서 class가 1인 경우만 추출하여 출력
exam %>% filter(class == 1)

# 2반인 경우만 추출
exam %>% filter(class == 2)

# 1반이 아닌 경우
exam %>% filter(class != 1)

# 수학 점수가 50점을 초과한 경우
exam %>% filter(math > 50)

# 1반 이면서 수학 점수가 50점 이상인 경우
exam %>% filter(class == 1 & math >= 50)

# 수학 점수가 90점 이상이거나 영어점수가 90점 이상인 경우
exam %>% filter(math >= 90 | english >= 90)

# %in% 기호 이용하기
exam %>% filter(class %in% c(1,3,5))  # 1, 3, 5 반에 해당하면 추출

# class가 1인 행 추출, class1에 할당
class1 <- exam %>% filter(class == 1)  
class1



###########혼자서 해보기

## Q1
mpg_df <- as.data.frame(ggplot2::mpg)

mpg_displ_4 <- mpg_df %>% filter(displ <= 4)
mpg_displ_5 <- mpg_df %>% filter(displ >= 5)

mean(mpg_displ_4$displ)
mean(mpg_displ_5$displ)


## Q2

mpg_menu_audi <- mpg_df %>% filter(manufacturer == "audi")
mpg_menu_toyota <- mpg_df %>% filter(manufacturer == "toyota")

mean(mpg_menu_audi$cty)
mean(mpg_menu_toyota$cty)


## Q3

mpg_menu_3 <- mpg_df %>% filter(manufacturer %in% c("chevrolet", "ford", "honda"))
mean(mpg_menu_3$hwy)



########### 06-3

exam %>% select(math)  #math 추출출
exam %>% select(english)  # english 추출

exam %>% select(class, math, english)  # class, math, english 변수 추출

exam %>% select(-math)  # math 제외

exam %>% filter(class == 1) %>% select(english) # class가 1인 행만 추출한 다음 english 추출


########## 06 -4

exam %>% arrange(math)  # math 오름차순 정렬
exam %>% arrange(desc(math))  # math 내림차순 정렬
exam %>% arrange(class, math)  # class 및 math 오름차순 정렬



######### 06 -4 혼자서 해보기

mpg_df <- as.data.frame(ggplot2::mpg)          
mpg_df %>% filter(manufacturer == "audi") %>% arrange(desc(hwy)) %>% head(5) 




######### 06-5. 파생변수 추가하기

exam %>%
  mutate(total = math + english + science) %>%  # 총합 변수 추가
  head                                          # 일부 추출


exam %>%
  mutate(total = math + english + science,          # 총합 변수 추가
         mean = (math + english + science)/3) %>%   # 총평균 변수 추가
  head                                              # 일부 추출


exam %>%
  mutate(test = ifelse(science >= 60, "pass", "fail")) %>%
  head

exam %>%
  mutate(total = math + english + science) %>%  # 총합 변수 추가
  arrange(total) %>%                            # 총합 변수 기준 정렬
  head                                          # 일부 추출


######### 06-5 혼자서 해보기

#Q1
mpg <- as.data.frame(ggplot2::mpg)
mpg_df <- mpg
mpg_df <- mpg_df %>% mutate(total = cty + hwy)

# Q2
mpg_df <- mpg_df %>% mutate(mean = total/2)

# Q3
mpg_df %>% arrange(desc(mean)) %>% head(3)

# Q4
mpg %>% mutate(total = cty + hwy, mean = total/2) %>%
  arrange(desc(mean)) %>% head(3)





###### 06-6. 집단별로 요약하기

exam %>% summarise(mean_math = mean(math))  # math 평균 산출

# 집단별로 요약
exam %>%
  group_by(class) %>%                # class별로 분리
  summarise(mean_math = mean(math))  # math 평균 산출


#여러 요약통계량 한 번에 산출하기
exam %>%
  group_by(class) %>%                   # class별로 분리
  summarise(mean_math = mean(math),     # math 평균
            sum_math = sum(math),       # math 합계
            median_math = median(math), # math 중앙값
            n = n())                    # 학생 수

# 각 집단별로 다시 집단 나누기
mpg %>%
  group_by(manufacturer, drv) %>%      # 회사별, 구방방식별 분리
  summarise(mean_cty = mean(cty)) %>%  # cty 평균 산출
  head(10)                             # 일부 출력

# dplyr 조합하기
mpg %>%
  group_by(manufacturer) %>%           # 회사별로 분리
  filter(class == "suv") %>%           # suv 추출
  mutate(tot = (cty+hwy)/2) %>%        # 통합 연비 변수 생성
  summarise(mean_tot = mean(tot)) %>%  # 통합 연비 평균 산출
  arrange(desc(mean_tot)) %>%          # 내림차순 정렬
  head(5)                              # 1~5위까지 출력


#####3 혼자해보기 6-6

# Q1
mpg %>%
  group_by(class) %>%
  summarise(mean_cty = mean(cty)) %>%  
  head(10) 

# Q2
mpg %>%
  group_by(class) %>%
  summarise(mean_cty = mean(cty)) %>%  
  arrange(desc(mean_cty)) %>% head(10) 

# Q3
mpg %>%
  group_by(manufacturer) %>% 
  summarise(mean_hwy = mean(hwy)) %>% 
  arrange(desc(mean_hwy)) %>% head(3) 


# Q4
mpg
mpg %>%
  filter(class == "compact") %>% 
  group_by(manufacturer) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))
  





######### 06-7. 데이터 합치기


#가로로 합치기

# 중간고사 데이터 생성
test1 <- data.frame(id = c(1, 2, 3, 4, 5),
                    midterm = c(60, 80, 70, 90, 85))

# 기말고사 데이터 생성
test2 <- data.frame(id = c(1, 2, 3, 4, 5),
                    final = c(70, 83, 65, 95, 80))

# id 기준으로 합치기
total <- left_join(test1, test2, by = "id")  # id 기준으로 합쳐 total에 할당
total                                        

#다른 데이터 활용해 변수 추가하기
#반별 담임교사 명단 생성
name <- data.frame(class = c(1, 2, 3, 4, 5),
                   teacher = c("kim", "lee", "park", "choi", "jung"))
name

# class 기준 합치기
exam_new <- left_join(exam, name, by = "class")
exam_new


####### 06-7 혼자서 해보기

fuel <- data.frame(fl = c("c", "d", "e", "p", "r"),
                   price_fl = c(2.35, 2.38, 2.11, 2.76, 2.22),
                   stringsAsFactors = F)
fuel  # 출력


# Q1

mpg <- as.data.frame(ggplot2::mpg)

mpg <- left_join(mpg, fuel, by = "fl")


# Q2
mpg %>% 
  select(model, fl, price_fl) %>% head(5)











########################## sales_data 

sales_data <- read.csv("sales_data.csv")
sales_data

sales_data <-sales_data %>% filter(CATEGORY %in% c("비타민음료","스포츠,이온음료"))

write.csv(sales_data, "ai_sales_data.csv")















