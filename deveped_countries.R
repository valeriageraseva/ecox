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
dc<-readxl::read_xlsx('Developed.xlsx') 


dc$logpatents=log(dc$patents)


r1 = lm(gdp_growth ~ 1 + rd, data=dc)
cov_r1 <- vcovHC(r1, type = "HC0")
cov_r1
coeftest(r1, data = dc, vcov = cov_r1)
summary(r1)


r2 = lm(gdp_growth ~ 1 + rd + capital_formation + patents + highed + empl + gini+ ed_exp, data=dc)
cov_r2 <- vcovHC(r2, type = "HC0")
cov_r2
coeftest(r2, data = dc, vcov = cov_r2)
summary(r2)


r3 = lm(gdp_growth ~ 1 + rd + capital_formation + logpatents + highed + empl + gini+ ed_exp, data=dc)
cov_r3 <- vcovHC(r3, type = "HC0")
cov_r3
coeftest(r3, data = dc, vcov = cov_r3)
summary(r3)



modelsummary(models = list(r1, r2, r3),                       # список оцененных моделей
             vcov = list(cov_r1, cov_r2, cov_r3),             # список ковариционных матриц для расчета стандартных ошибок
             statistic = "std.error",                                     # выводить стандартные ошибки
             stars = TRUE,                                                # звездочки для уровня значимости
             gof_omit = ".*",                                             # не выводить никаких показателей качества моделей
             notes = list("В скобках даны робастные стандартные ошибки"), # комментарий по поводу расчета стандартных ошибок
             title = "Результаты оценки")   

