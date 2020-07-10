########### 6-3 혼자서 해보기

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




################ 06 -4 혼자서 해보기

mpg_df <- as.data.frame(ggplot2::mpg)          
mpg_df %>% filter(manufacturer == "audi") %>% arrange(desc(hwy)) %>% head(5) 




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







####### 분석도전
  
midwest <- as.data.frame(ggplot2::midwest)

# Q1
midwest <- midwest %>% mutate(result = (poptotal-popadults)/poptotal * 100)

# Q2
midwest %>% arrange(desc(result)) %>% select(county, result) %>% head(5)

# Q3
midwest <- midwest %>% mutate(grade = ifelse(result >= 40, "large",
                                             ifelse(result >= 30, "middle","small")))
table(midwest$grade)

# Q4
midwest %>%  mutate(asian = popasian/poptotal * 100) %>% 
  arrange(asian) %>% select(state, county, asian) %>% head(10)






















