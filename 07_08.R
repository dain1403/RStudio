setwd("c:/Rdata")
df = data(package = "ggplot2")
df$results

mpg = as.data.frame(ggplot2::mpg)
head(mpg)
table(mpg$drv)

library(dplyr)
library(ggplot2)

df_g = mpg %>% 
  group_by(drv) %>% 
  summarise(mean_cty = mean(cty))
df_g

ggplot(data=df_g, aes(x=reorder(drv, -mean_cty), y = mean_cty)) +
  geom_col(fill = c("red","blue","orange")) +
  geom_text(aes(label = df_g$mean_cty),hjust = -0.2, col="red") +
  coord_flip() + xlab("구동타입") + ylab("도시연비비")

# seed함수
runif(3)   ## 랜덤하게 숫자 찍음 0~1
rnorm(3)  ## 랜덤하게 숫자 찍음 - 음수도 포함됨됨
rnorm(3,mean = 0, sd = 1)
runif(15)
set.seed(1234)
runif(15)











#####################################################

setwd(c:/Rdata)
library(dplyr)
library(ggplot2)
install.packages("ggmap")
library(ggmap)
library(stringr)
library(rvest)

register_google(key = "")  # <--메모장or드라이브 : 장영완강사님 key
tt = get_map(location = "전남 고흥군 도덕면 가야리",
             zoom = 15,
             maptype = 'roadmap', #hybrid, satellite, terrian, ...
             source = 'google')
ggmap(tt)

tt = get_map(location = "전남 고흥군 도덕면 가야리",
             zoom = 15,
             maptype = 'satellite',
             source = 'google')
ggmap(tt)

qmap(location = "전남 고흥군 도덕면 가야리",
     zoom = 15,
     maptype = 'satellite',
     source = 'google')

plot.new()
frame()
geocodeQueryCheck()
geocode(location = "전남 고흥군 도덕면",
        output = 'latlon',
        source = 'google')

geocode(location = "전남 고흥군 도덕면",
        output = 'latlona',
        source = 'google')

geocode(location = enc2utf8(x ="전남 고흥군 도덕면$language = ko"),
        output = 'latlon',
        source = 'google')

myloc = geocode(location = '대전광역시 동구 동대전로 171',
                output = 'latlon',
                souce = 'google')
myloc
center = c(myloc$lon, myloc$lat)
qmap(laocation = center,
     zoom = 18,
     maptype = 'hybrid',
     source = 'google') +
  geom_point(data = myloc,
             mapping = aes(x= lon, y=lat),
             shape='*',
             color = 'red',
             stroke=18, size = 10)










###############################################################
### 서울 특별시의 대학목록추출하기
url = "https://namu.wiki/w/%EC%84%9C%EC%9A%B8%ED%8A%B9%EB%B3%84%EC%8B%9C%EC%9D%98%20%EB%8C%80%ED%95%99%EA%B5%90%20%EB%AA%A9%EB%A1%9D"

hdoc = read_html(url, encoding = 'UTF-8')
df = hdoc %>% 
  html_nodes(".wiki-paragraph a") %>% 
  html_text()
head(df, 50)
univ = ifelse(str_detect(df, pattern = '대학교'), df, "")
univ
kk = univ %>% 
  data.frame()
kk = Filter(function(x){nchar(x) > 5}, univ )
kk

univName = kk[2:28]
univName

univCord = geocode(location = univName,
                   output = 'latlon',
                   source = 'google')
univDf = data.frame(univ = univName, lon = univCord$lon,
                    lat = univCord$lat)
univDfNa = na.omit(univDf)
univDfNa

center = c(mean(x = univDfNa$lon),
           mean(x = univDfNa$lat))
center

qmap(location = center,
     zoom = 12,
     maptype = 'satellite',
     source = 'google') + 
  geom_point(data = univDfNa,
             aes(x = lon, y = lat),
             shape = '*',
             color = 'red',
             size = 6) +
  geom_text(data = univDfNa,
            aes(x = lon, y = lat, label = univ),
            color = 'green', hjust = 0.5,
            vjust = -0.1,
            fontface = 'bold',
            family = 'NanumCodic')











##################### movie 크롤링을 통한 시나리오 분석##############
library(rvest)
library(stringr)
library(dplyr)

title = c()
grade = c()
time = c()

t_css = ".color_b"
gr_css = ".list_netizen_score em"  ## .은 안에라는 뜻
pt_css=".title+ .num"

base_url = "https://movie.naver.com/movie/point/af/list.nhn?&page="

for ( i in 1:100){
  cr_url = paste0(base_url, 1)
  cr_url
  
  hdoc = read_html(cr_url, encoding = "CP949")
  n_title = html_nodes(hdoc, t_css)
  n_title
  
  n_gr = html_nodes(hdoc, gr_css)
  n_pt = html_nodes(hdoc, pt_css)
  
  title_part = html_text(n_title)
  grade_part = html_text(n_gr)
  grade_part
  pt_part = html_text(n_pt)
  time_part = str_sub(pt_part, -8)
  
  title = c(title, title_part)
  grade = c(grade, grade_part)
  time = c(time, time_part)
}

movie = data.frame(title, grade, time)
View(movie)

write.csv(movie, "movie.csv")

data = read.csv("movie.csv")
head(data)

## 가장높은 평가총점을 가진것의 top10

top10_A = data %>% 
  select(title, grade) %>% 
  group_by(title) %>% 
  summarise(total = sum(grade),count = n()) %>% 
  arrange(desc(total), desc(count)) %>% 
  head(10)

top10_A
ggplot(data = top10_A, aes(x = title, y = total)) +
  geom_col(fill = 'red') +
  geom_text(aes(label = top10_A$total), hjust = -0.2, col = 'blue')+
  coord_flip()






#################################################################
########################## ppt : Lecture8_상관분석_단순회귀분석

setwd("c:/Rdata")
View(attitude)
cov(attitude)
cor(attitude)  ## 관계성

with(attitude,cor.test(rating, complaints))
cor.test(attributes())
plot(attributes())


##단순회귀분석
## 회귀분석 – 원인이 되는 변수 (설명변수)에 따른 종속변수의 결과
##            예측 (의존적 관계)

fasu = data.frame(fa,su)
fasu
lm(su~fa, data=fasu)

data = read.csv("cars.csv")
## 차의 속도와 급브레이크를 밟았을 때 멈추기까지 걸린 거리
data

## lm(종속변수 ~설명변수, data) - 설명변수를 종속변수에 회귀분석
out = lm(dist ~ speed, data = data)
summary(out)
## 산점도와 회귀선
plot(dist~speed,data=data,col="blue")
abline(out,col="red")

summary(lm(dist~speed+0,data=data))
plot(lm(dist~speed+0,data=data))

par(mfrow = c(2,2))
plot(lm(dist~speed+0,data=data))

shapiro.test(data$dist)
shapiro.test(log(data$dist))
shapiro.test(sqrt(data$dist))
out3 = lm(sqrt(dist) ~ speed+0, data=data)
summary(out3)
plot(out3)

#####################################################
########################## ppt :Lecture 9_단순회귀_다중회귀

out3$fitted.values
cbind(data$speed, out3$fitted.values)
new = data.frame(speed = data$speed)
cbind(new$speed, predict(out3, new))
cbind(new$speed, predict(out3,new, interval = "confidence"))
cbind(new$speed, predict(out3,new, interval = "prediction"))


## 다중회귀분석
data = read.csv("salary.csv")
head(data)
out=lm(salary~experience+score,data = data) 
summary(out)

plot(out)
cbind(data$experience, data$score, out$fitted.values)

summary(lm(rating~complaints+learning, data=attitude))
summary(lm(rating~learning, data=attitude))
out = lm(rating~., data=attitude)
summary(out)
backward = step(out, direction = "backward", trace = FALSE)
summary(backward)

both = step(out, direction="both",trace = FALSE)
summary(both)

##All subset method
install.packages("leaps")
library(leaps)

leaps = regsubsets(rating~., data=attitude, nbest = 5)
summary(leaps)
plot(leaps)
par(mfrow = c(1,1))
plot(leaps, scale='bic')
out_bic = glm(rating ~ complaints, data=attitude)
summary(out_bic)
plot(leaps, scale = 'Cp')
out_cp = lm(rating ~ complaints+learning, data = attitude)
summary(out_cp)
plot(leaps, scale = "adjr2")
out_adjr = lm(rating~complaints+learning+advance, data = attitude)
summary(out_adjr)
