# 웹크롤링 - 영화 
setwd("c:/Rdata")
library(rvest)
library(stringr)
library(dplyr)

rm(list = ls())
title = c()
grade = c()
url_b = "https://movie.naver.com/movie/point/af/list.nhn?&page="

for(i in 1:100){
craw_url = paste0(url_b, 1, sep="")

t_css = ".color_b"
g_css = "#old_content em"
#hdoc = read_html(craw_url, encoding = "EUC - KR")
title_part = read_html(craw_url, encoding = "CP949") %>% 
  html_nodes(t_css) %>% 
  html_text

#title_part
title = c(title, title_part)
}
View(title)





