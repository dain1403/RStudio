setwd("c:/Rdata") 
data = read.csv("programming.csv")
head(data)

## glm ?쓣?넻?빐 濡쒖??뒪?떛?쉶洹紐⑦삎?쓣 fitting?떆?궓?떎. 
## family='binomial' ?씤?옄瑜? ?넻?빐, glm?쑝濡? 濡쒖??뒪?떛 ?쉶洹紐⑦삎?쓣 ?벝 ?닔 ?엳?떎
model = glm(Success ~ Experience,data = data, family = binomial(logit))
summary(model)
cbind(data$Experience, model$fitted.values)
plot(Success~Experience, data = data)
points(model$fitted.values ~ data$Experience, col = 2)

table(data$Success, model$fitted.values > 0.5)
c('誘쇨컧?룄' = 8/11, '?듅?씠?룄' = 11/24)

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
reduce_M = c('誘쇨컧?룄' = 23/31, '?듅?씠?룄' = 47/(47+20))
kk1 = table(data$disease, model4$fitted.values > 0.3163265)
kk1
fulmode_M = c('誘쇨컧?룄' = 23/31, '?듅?씠?룄' = 49/(49+18))
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
##### HW 濡쒖??뒪?떛
## ?뿰?뒿臾몄젣 ???씠

data = read.csv("flushot.csv")
head(data)
log_model = glm(flushot~., data= data, family = binomial(logit))
summary(log_model)
exp(0.07179)
exp(-0.09899)
exp(0.43397)
log_model2 = glm(flushot ~ age+aware,data= data, family = binomial())
summary(log_model2)
table(data$flushot)
24/(134+24)      ## --> 0.1518987
tt = table(data$flushot, log_model2$fitted.values>0.1518987)  ## 0.1518987 : ?엫怨꾧컪
c('誘쇨컧?룄' = 19/(5+19), '?듅?씠?룄' = 95/(95+40), '?뿉?윭?쑉' = 45/sum(tt))  ## ?뿉?윭?쑉45?뒗 40+5?빐?꽌 ?굹?삩寃?
rocplot(log_model2)

tab_01 = table(data$flushot, log_model2$fitted.values>0.1)
tab_015 = table(data$flushot, log_model2$fitted.values>0.15)
tab_02 = table(data$flushot, log_model2$fitted.values>0.2)

tab_01
tab_015
tab_02
res01 = c('誘쇨컧?룄' = tab_01[2,2]/sum(tab_01[2,1]), 
                                  '?듅?씠?룄' = tab_01[1,1]/sum(tab_01[1,]),
                                  '?뿉?윭?쑉' = (tab_01[1,2]+tab_01[2,1]/sum(tab_01)))
res01

res015 = c('誘쇨컧?룄' = tab_015[2,2]/sum(tab_015[2,1]), 
          '?듅?씠?룄' = tab_015[1,1]/sum(tab_015[1,]),
          '?뿉?윭?쑉' = (tab_015[1,2]+tab_015[2,1]/sum(tab_015)))
res015

res02 = c('誘쇨컧?룄' = tab_02[2,2]/sum(tab_02[2,1]), 
           '?듅?씠?룄' = tab_02[1,1]/sum(tab_02[1,]),
           '?뿉?윭?쑉' = (tab_02[1,2]+tab_02[2,1]/sum(tab_02)))
res02

model4$fitted.values

jang = function() {
  k = seq(0.01, 0.5, 0.01)
  
  n = length(k)
  
  err_min = vector(length = n)
  sens = vector(length = n)
  spec = vector(length = n)
  
  for(i in 1:n) {
    tab = table(data$flushot, log_model2$fitted.values > k[i])
    res =  c('誘쇨컧?룄' = tab[2,2]/sum(tab[2,1]), 
             '?듅?씠?룄' = tab[1,1]/sum(tab[1,]),
             '?뿉?윭?쑉' = (tab[1,2]+tab_02[2,1]/sum(tab)))
    err_min[i]=tab[2,1]/sum(tab[2,])
    spec[i] = tab[1,1]/sum(tab[1,])
    print(res)
  }

  print(err_min)
  print(paste("理쒖냼?쓽 error rate = ",min(err_min),"?씠?떎"))
  index=which(err_min<=min(err_min))
  print(err_min <= min(err_min))
  print(index)
  print(paste("?빐?떦?릺?뒗 誘쇨컧?룄 = ",sens[min(index)],"?씠?떎."))
  print(paste("?빐?떦?릺?뒗 ?듅?씠?룄 = ",spec[min(index)],"?씠?떎."))
  print(paste("?빐?떦?릺?뒗 ?뿉?윭?쑉 = ",err_min[min(index)],"?씠?떎."))
  print(paste("?빐?떦?릺?뒗 cutoff = ",k[min(index)],"?씠?떎."))

  plot(1-spec, sens, col =2)
}


jang()









##########################################################################
############### Lecture12_?떎蹂?웾?옄猷뚰깘?깋_二쇱꽦遺꾨텇?꽍   

crime = read.csv("http://datasets.flowingdata.com/crimeRatesByState-formatted.csv")
head(crime)

rownames(crime)

rownames(crime) = crime[, 1]
rownames(crime)

## draw.segments = TRUE : ?깋梨꾩슦湲?
stars(crime[, 2:8], flip.labels = FALSE, draw.segments = TRUE, key.loc = 2)


## 泥대Ⅴ?끂?봽?럹?씠?뒪
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
###### ?떎蹂?웾 ?뿰?뒿臾몄젣  - HW ?떎蹂?웾遺꾩꽍

####1踰? 20140528_baseball.csv?뒗 2014?뀈 5?썡 28?씪 ?쁽?옱 ?븳援? ?봽濡쒖빞援? 媛? ???쓽 ?꽦?쟻?쓣 蹂댁뿬以?떎.?씠 ?옄猷뚮?? ?씠?슜?빐 蹂꾧렇由?,泥대Ⅴ?끂?봽?럹?씠?뒪, ?굹?씠?똿寃뚯씪 李⑦듃瑜? ?쟻?젅?븳 label?쓣 ?룷?븿?븯?뿬 洹몃━怨? 鍮꾩듂?븳 ?뙣?꽩?쓣 媛吏?뒗 洹몃９?쑝濡? ?굹?늻?뼱 媛? 洹몃９?씠 ?뼱?뼡 蹂?닔?쟻 ?듅吏뺤쓣 媛吏?뒗吏 ?꽌?닠?븯?뿬?씪.
data = read.csv("20140528_baseball.csv")
head(data)
rownames(data) = data[,1]
head(data)
stars(data[, 2:6], flip.labels = F, key.loc = c(9,3), draw.segments = T)
faces(data[, 2:6])


#### 2踰?	2013_baseball.csv?뒗 2013?뀈 ?떆利? 媛? ???옄?쓽 ?꽦?쟻?쓣 ?룷?븿?븯怨? ?엳?떎.?룊?뻾醫뚰몴 ?뵆濡??쓣 ?솢?슜?빐 ?꽑?닔?뱾?쓽 ?꽦?쟻 ?뙣?꽩?뿉 ?뼱?뼡 寃쏀뼢?씠 ?엳?뒗吏 ?븣?븘蹂대젮 ?븳?떎.?룷吏?뀡 蹂?,??蹂? ?룊?뻾醫뚰몴 ?뵆濡??쓣 洹몃━怨? 媛? ?룷吏?뀡,?샊?? ??蹂? ???옄?쓽 ?꽦?쟻?뿉 ?뼱?뼡 寃쏀뼢?씠 ?엳?뒗吏 ?꽌?닠?븯?씪.
bb2013 = read.csv("2013_baseball.csv")
head(bb2013)
position = bb2013$?룷吏?뀡
head(position)
base2_pos = bb2013[,c(2, 4:11)]
base2_pos2 = aggregate(base2_pos[, 2:9], by = list(?룷吏?뀡 = base2_pos$?룷吏?뀡), sum)
head(base2_pos2)
rownames(base2_pos2) = base2_pos2[,1]
head(base2_pos2)
library(lattice)
parallel(base2_pos2[,2:9], horizontal.axis = F, col =1)


# ??蹂꾪룊?뻾醫뚰몴
team = bb2013$??
parallel(~bb2013[,4:11]|team,horizontal.axis = F, col =1)


######## 3踰?	2013_baseball.csv?뿉 ?엳?뒗 媛? ???옄?뿉 ???븳 蹂?닔瑜? ?궗?슜?븯?뿬 二쇱꽦遺? 遺꾩꽍?쓣 ?떆?뻾?븯?씪.?쟻?떦?븳 二쇱꽦遺꾩쓽 媛쒖닔瑜? ?뙆?븙?븯怨? ?빐?떦 二쇱꽦遺꾨뱾濡? ?꽕紐낅릺?뒗 遺꾩궛?쓽 鍮꾩쑉?쓣 援ы븯?씪.?뻾?젹?룄瑜? ?넻?빐 ?꽑?닔?뱾?쓽 ?듅吏뺤씠 ?뼱?뼸寃? ?뙆?븙?릺?뒗吏 ?궡?렣蹂댁떆?삤.
rownames(bb2013) = bb2013[,1]
rownames(bb2013)
model = prcomp(bb2013[,4:11], scale = T)
plot(model)
summary(model)
biplot(model)





