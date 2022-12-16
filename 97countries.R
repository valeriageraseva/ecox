library (dplyr)
library (tidyr)
library(xtable)
library(readr)
library(lmtest)       # тестирование гипотез
library(sandwich)     # робастные стандартные ошибки
library(stargazer)    # представление результатов нескольких регрессий в одной таблице
library(modelsummary)
library ('ggplot2')

setwd("D:/Downloads")
data<-readxl::read_xlsx('Average_all.xlsx') 



data <- na.omit(data)


data$logpatents=log(data$patents)



m1 = lm(gdp_growth ~ 1 + rd, data=data)
cov_m1 <- vcovHC(m1, type = "HC0")
cov_m1
coeftest(m1, data = data, vcov = cov_m1)
summary(m1)


m2 = lm(gdp_growth ~ 1 + rd + capital_formation + patents + highed + empl + gini+ ed_exp, data=data)
cov_m2 <- vcovHC(m2, type = "HC0")
cov_m2
coeftest(m2, data = data, vcov = cov_m2)
summary(m2)


m3 = lm(gdp_growth ~ 1 + rd + capital_formation + logpatents + highed + empl + gini+ ed_exp, data=data)
cov_m3 <- vcovHC(m3, type = "HC0")
cov_m3
coeftest(m3, data = data, vcov = cov_m3)
summary(m3)




modelsummary(models = list(m1, m2, m3),                       # список оцененных моделей
             vcov = list(cov_m1, cov_m2, cov_m3),             # список ковариционных матриц для расчета стандартных ошибок
             statistic = "std.error",                                     # выводить стандартные ошибки
             stars = TRUE,                                                # звездочки для уровня значимости
             gof_omit = ".*",                                             # не выводить никаких показателей качества моделей
             notes = list("В скобках даны робастные стандартные ошибки"), # комментарий по поводу расчета стандартных ошибок
             title = "Результаты оценки")   

