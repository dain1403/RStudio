###### Doit_part03

x=rnorm(100,175,2) ## random normality  ## ������ ���� , ���, ǥ������
x
hist(x, breaks = 5, probability = T)
lines(density(x), col =2, type = 'h', lwd=0.5)
shapiro.test(x)

a <- 1
a

a = 2
a

var1 <- c(1, 2, 5, 7, 8)    # ���� �ټ� ���� ������ var1 ����
var1

var2 <- c(1:5)
var2

var3 = seq(1,5)
var3

var4 = seq(1,10,by=2) ## 1���� 10���� 2�������� ���Ӱ�
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
sd(x)  ##ǥ������


###### 03-3

install.packages("ggplot2")  ## ��ġ
library(ggplot2)    ##�ε�


x <- c("a", "a", "b", "c")  # ���� ���ڷ� ������ ���� ����
x

qplot(x)  # �� �׷��� ���


# x�� drv, y�� hwy, ���� �׸� ����, drv�� �� ǥ��
qplot(data = mpg, x = drv, y = hwy, geom = "boxplot", colour = drv)
mpg   ## ggplot2���ִ� data

?qplot

### ȥ�ڼ��غ���

student = c(80, 60, 70, 50, 90)
student

mean(student)
sd(student)


############################################################


#### DOit_part04

english <- c(90, 80, 60, 70)  # ���� ���� ���� ����
english

math <- c(50, 60, 100, 20)    # ���� ���� ���� ����
math

df_midterm <- data.frame(english, math)
df_midterm

class <- c(1, 1, 2, 2)
class

df_midterm$english  ## df_midterm�ȿ� english�� ���̵���
mean(df_midterm$english)  
sd(df_midterm$english)
sqrt(df_midterm$english)
sqrt(var(df_midterm$english))


###ȥ�ڼ��غ���

# ������ ������ �����
sales <- data.frame(fruit = c("���", "����", "����"),
                    price = c(1800, 1500, 3000),
                    volume = c(24, 38, 13))

sales

mean(sales$price) # ���� ���
mean(sales$volume) # �Ǹŷ� ���




########## 04-3

install.packages("readxl")  # readxl ��Ű�� ��ġ
library(readxl)   # readxl ��Ű�� �ε�


## ���� ���ο� ��ŷ ���丮�� �����Ϸ��� setwd() ���ɾ �̿�
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

head(exam,15) # �տ������� 15����� ���
tail(exam,10)

View(exam)  #��� â���� ������ Ȯ���ϱ�

dim(exam)  # ��, �� ���

str(exam)  # ������ �Ӽ� Ȯ��

summary(exam)  # �����跮 ���

boxplot(exam$math, horizontal = T, col=2)
hist(exam$math)

x = sample(0:100, 80, replace = T) #0~100���� 80���� ������,replace�� ���ĵ� �ȴٴ°� 
plot(x, pch=ifelse(x>=60, 7, 15))  ## x>=60  ���̸� pch = 7, �����̸� 15
abline(h = 60)  #�������� 60�� ����

mpg=as.data.frame(ggplot2::mpg) # ggplo2�� mpg �����͸� ������ ������ ���·� �ҷ�����
head(mpg)



######## 05 -2 ������ �����ϱ� - ������ �ٲٱ�


install.packages("dplyr")  # dplyr ��ġ
library(dplyr)             # dplyr �ε�

df_raw <- data.frame(var1 = c(1, 2, 1), var2 = c(2, 3, 2))
df_raw

df_new <- df_raw  # ���纻 ����
df_new            

df_new <- rename(df_new, v2 = var2)  # var2�� v2�� ����
df_new


###### ȥ�ڼ� �غ���  - �����ϱ�

mpg_df = as.data.frame(ggplot2::mpg)
mpg_df

mpg_copy_df = mpg_df

mpg_copy_df <- rename(mpg_copy_df, city = cty)
mpg_copy_df <- rename(mpg_copy_df, highway = hwy)

head(mpg_copy_df)



########### 05-3 �Ļ����� �����

df <- data.frame(var1 = c(4, 3, 8),
                 var2 = c(2, 6, 1))
df

df$var_sum <- df$var1 + df$var2  # var_sum �Ļ����� ����
df

df$var_mean <- (df$var1 + df$var2)/2  # var_mean �Ļ����� ����
df

mpg$total <- (mpg$cty + mpg$hwy)/2  # ���� ���� ���� ����
head(mpg)

mean(mpg$total)


### ���ǹ��� Ȱ���� �Ļ����� �����

summary(mpg$total)  # ��� ��跮 ����

hist(mpg$total)     # ������׷� ����

# 20 �̻��̸� pass, �׷��� ������ fail �ο�
mpg$test <- ifelse(mpg$total >= 20, "pass", "fail")

head(mpg, 20) # ������ Ȯ��

table(mpg$test)  # ���� �հ� ��ǥ ����

library(ggplot2)  # ggplot2 �ε�
qplot(mpg$test)   # ���� �հ� �� ���� �׷��� ����


#### �м� ���� 
# ���� 1
midwest=as.data.frame(ggplot2::midwest)
head(midwest)
tail(midwest)

# ���� 2
library(dplyr)
midwest <- rename(midwest, total = poptotal)
midwest <- rename(midwest, asian = popasian)

# ���� 3
midwest$ratio <- midwest$asian/midwest$total*100
hist(midwest$ratio)

# ���� 4
mean(midwest$ratio)

midwest$group <- ifelse(midwest$ratio > 0.4872462, "large", "small")

# ���� 5
table(midwest$group)

library(ggplot2)
qplot(midwest$group)




####################################################################
##### Doit_part06 ��������� ������ �����ϱ�


### ���ǿ� �´� �����͸� �����ϱ�

library(dplyr)
exam <- read.csv("csv_exam.csv")
exam

# exam���� class�� 1�� ��츸 �����Ͽ� ���
exam %>% filter(class == 1)

# 2���� ��츸 ����
exam %>% filter(class == 2)

# 1���� �ƴ� ���
exam %>% filter(class != 1)

# ���� ������ 50���� �ʰ��� ���
exam %>% filter(math > 50)

# 1�� �̸鼭 ���� ������ 50�� �̻��� ���
exam %>% filter(class == 1 & math >= 50)

# ���� ������ 90�� �̻��̰ų� ���������� 90�� �̻��� ���
exam %>% filter(math >= 90 | english >= 90)

# %in% ��ȣ �̿��ϱ�
exam %>% filter(class %in% c(1,3,5))  # 1, 3, 5 �ݿ� �ش��ϸ� ����

# class�� 1�� �� ����, class1�� �Ҵ�
class1 <- exam %>% filter(class == 1)  
class1



###########ȥ�ڼ� �غ���

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

exam %>% select(math)  #math ������
exam %>% select(english)  # english ����

exam %>% select(class, math, english)  # class, math, english ���� ����

exam %>% select(-math)  # math ����

exam %>% filter(class == 1) %>% select(english) # class�� 1�� �ุ ������ ���� english ����


########## 06 -4

exam %>% arrange(math)  # math �������� ����
exam %>% arrange(desc(math))  # math �������� ����
exam %>% arrange(class, math)  # class �� math �������� ����



######### 06 -4 ȥ�ڼ� �غ���

mpg_df <- as.data.frame(ggplot2::mpg)          
mpg_df %>% filter(manufacturer == "audi") %>% arrange(desc(hwy)) %>% head(5) 




######### 06-5. �Ļ����� �߰��ϱ�

exam %>%
  mutate(total = math + english + science) %>%  # ���� ���� �߰�
  head                                          # �Ϻ� ����


exam %>%
  mutate(total = math + english + science,          # ���� ���� �߰�
         mean = (math + english + science)/3) %>%   # ����� ���� �߰�
  head                                              # �Ϻ� ����


exam %>%
  mutate(test = ifelse(science >= 60, "pass", "fail")) %>%
  head

exam %>%
  mutate(total = math + english + science) %>%  # ���� ���� �߰�
  arrange(total) %>%                            # ���� ���� ���� ����
  head                                          # �Ϻ� ����


######### 06-5 ȥ�ڼ� �غ���

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





###### 06-6. ���ܺ��� ����ϱ�

exam %>% summarise(mean_math = mean(math))  # math ��� ����

# ���ܺ��� ���
exam %>%
  group_by(class) %>%                # class���� �и�
  summarise(mean_math = mean(math))  # math ��� ����


#���� �����跮 �� ���� �����ϱ�
exam %>%
  group_by(class) %>%                   # class���� �и�
  summarise(mean_math = mean(math),     # math ���
            sum_math = sum(math),       # math �հ�
            median_math = median(math), # math �߾Ӱ�
            n = n())                    # �л� ��

# �� ���ܺ��� �ٽ� ���� ������
mpg %>%
  group_by(manufacturer, drv) %>%      # ȸ�纰, �����ĺ� �и�
  summarise(mean_cty = mean(cty)) %>%  # cty ��� ����
  head(10)                             # �Ϻ� ���

# dplyr �����ϱ�
mpg %>%
  group_by(manufacturer) %>%           # ȸ�纰�� �и�
  filter(class == "suv") %>%           # suv ����
  mutate(tot = (cty+hwy)/2) %>%        # ���� ���� ���� ����
  summarise(mean_tot = mean(tot)) %>%  # ���� ���� ��� ����
  arrange(desc(mean_tot)) %>%          # �������� ����
  head(5)                              # 1~5������ ���


#####3 ȥ���غ��� 6-6

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
  





######### 06-7. ������ ��ġ��


#���η� ��ġ��

# �߰����� ������ ����
test1 <- data.frame(id = c(1, 2, 3, 4, 5),
                    midterm = c(60, 80, 70, 90, 85))

# �⸻���� ������ ����
test2 <- data.frame(id = c(1, 2, 3, 4, 5),
                    final = c(70, 83, 65, 95, 80))

# id �������� ��ġ��
total <- left_join(test1, test2, by = "id")  # id �������� ���� total�� �Ҵ�
total                                        

#�ٸ� ������ Ȱ���� ���� �߰��ϱ�
#�ݺ� ���ӱ��� ���� ����
name <- data.frame(class = c(1, 2, 3, 4, 5),
                   teacher = c("kim", "lee", "park", "choi", "jung"))
name

# class ���� ��ġ��
exam_new <- left_join(exam, name, by = "class")
exam_new


####### 06-7 ȥ�ڼ� �غ���

fuel <- data.frame(fl = c("c", "d", "e", "p", "r"),
                   price_fl = c(2.35, 2.38, 2.11, 2.76, 2.22),
                   stringsAsFactors = F)
fuel  # ���


# Q1

mpg <- as.data.frame(ggplot2::mpg)

mpg <- left_join(mpg, fuel, by = "fl")


# Q2
mpg %>% 
  select(model, fl, price_fl) %>% head(5)











########################## sales_data 

sales_data <- read.csv("sales_data.csv")
sales_data

sales_data <-sales_data %>% filter(CATEGORY %in% c("��Ÿ������","������,�̿�����"))

write.csv(sales_data, "ai_sales_data.csv")














