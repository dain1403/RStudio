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

