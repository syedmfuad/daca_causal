# Borjas Method

library(foreign)
library(stargazer)
library(AER)

memory.limit(size=56000)
library(data.table)

data <- fread("E:/Desktop(Current)/DACA/Data/New Data/Newest Data/DACA Eligible (past settlement IV)/Over-All_Elig.csv")

#subset of data

#total US sample

data <- subset(data, YEAR>=2012)

# industry dummy
data$Ind_01 <- ifelse(data$industry==1, 1, 0)
data$Ind_02 <- ifelse(data$industry==2, 1, 0)
data$Ind_03 <- ifelse(data$industry==3, 1, 0)
data$Ind_04 <- ifelse(data$industry==4, 1, 0)
data$Ind_05 <- ifelse(data$industry==5, 1, 0)
data$Ind_06 <- ifelse(data$industry==6, 1, 0)
data$Ind_07 <- ifelse(data$industry==7, 1, 0)
data$Ind_08 <- ifelse(data$industry==8, 1, 0)
data$Ind_09 <- ifelse(data$industry==9, 1, 0)
data$Ind_10 <- ifelse(data$industry==10, 1, 0)
data$Ind_11 <- ifelse(data$industry==11, 1, 0)
data$Ind_12 <- ifelse(data$industry==12, 1, 0)
data$Ind_13 <- ifelse(data$industry==13, 1, 0)
data$Ind_14 <- ifelse(data$industry==14, 1, 0)
data$Ind_15 <- ifelse(data$industry==15, 1, 0)
data$Ind_16 <- ifelse(data$industry==16, 1, 0)

#schl dummy

data$schl_02 <- ifelse(data$schl==2, 1, 0)
data$schl_03 <- ifelse(data$schl==3, 1, 0)
data$schl_04 <- ifelse(data$schl==4, 1, 0)

#shock
data$shock <- data$m_ijt/(data$m_ijt+data$n_ijt)
data$logshock <- log(data$shock)

#Weight (for weighted regression)

data$weight <- sum(data$m_ijt)/(data$m_ijt)

#OLS

reg1 <- lm(weekwage ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
             Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 , data=data)

reg1_w <- lm(weekwage ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
               Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 , weights =weight,  data=data)

summary(reg1)

summary(reg1_w)

reg2 <- lm(yearwage ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
             Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 ,  data=data)

reg2_w <- lm(yearwage ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
               Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 , weights = weight, data=data)

summary(reg2)

summary(reg2_w)


reg3 <- lm(hrswork  ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
             Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 , data=data)

reg3_w <- lm(hrswork  ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
               Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 , weights=weight, data=data)

summary(reg3_w)

summary(reg3)

data$wkwork1 <- data$wkwork*100

reg4 <- lm(wkwork1 ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
             Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 , data=data)

reg4_w <- lm(wkwork1 ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
               Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 , weights=weight, data=data)

summary(reg4)

summary(reg4_w)

#IV regressions

summary(data$iv_m)

data$iv_m <- ifelse(data$iv_m == 0, 25, data$iv_m)

summary(data$iv_n)

data$iv_n <- ifelse(data$iv_n == 0, 296, data$iv_n)

data$IV <- data$iv_m/(data$iv_m+data$iv_n)

data$IV <- ifelse(data$IV == 0, 1, data$IV)

data$logIV <- log(data$IV)

#weekwage

ivmodel_1 <- ivreg(weekwage ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                     Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15|logIV+ factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                     Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 ,  data=data)

ivmodel_1_w <- ivreg(weekwage ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                       Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15|logIV + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                       Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 , weights=weight, data=data)

stargazer(ivmodel_1, type='text')

summary(ivmodel_1,  diagnostics = TRUE)

summary(ivmodel_1, vcov=sandwich,  diagnostics = TRUE)

summary(ivmodel_1_w, diagnostics = TRUE) 

summary(ivmodel_1_w, vcov=sandwich, diagnostics = TRUE) 

# yearwage

ivmodel_2 <- ivreg(yearwage ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                     Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15|logIV+ factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                     Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 ,  data=data)

ivmodel_2_w <- ivreg(yearwage ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                       Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15|logIV+ factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                       Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 , weights= weight, data=data)

stargazer(ivmodel_2, type='text')

summary(ivmodel_2, diagnostics = TRUE)

summary(ivmodel_2, vcov=sandwich, diagnostics = TRUE)

summary(ivmodel_2_w, diagnostics = TRUE) 

summary(ivmodel_2_w, vcov=sandwich, diagnostics = TRUE)

#hrswork
ivmodel_3 <- ivreg(hrswork ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                     Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15|logIV + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                     Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 , data=data)

ivmodel_3_w <- ivreg(hrswork ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                       Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15|logIV+ factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                       Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 , weights=weight, data=data)

stargazer(ivmodel_3, type='text')

summary(ivmodel_3,  diagnostics = TRUE)

summary(ivmodel_3, vcov=sandwich,  diagnostics = TRUE)

summary(ivmodel_3_w, vcov=sandwich, diagnostics = TRUE) 

summary(ivmodel_3_w, diagnostics = TRUE) 

#wkwork

data$wkwork1 <- data$wkwork*100

ivmodel_4 <- ivreg(wkwork1 ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                     Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15|logIV+ factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                     Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15, data=data)

ivmodel_4_w <- ivreg(wkwork1 ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                       Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15|logIV+ factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                       Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 , weights=weight, data=data)

stargazer(ivmodel_4, type='text')

summary(ivmodel_4, diagnostics = TRUE)

summary(ivmodel_4, vcov=sandwich, diagnostics = TRUE) 

summary(ivmodel_4_w, vcov=sandwich, diagnostics = TRUE) 

summary(ivmodel_4_w,  diagnostics = TRUE) 

# weak instrument test suggests whether the instrument is weak or not 
# significance of weak instrument test means the IV is not weak
# The weak instrument test encompasses an F-test for 1st stage regression
# Wu-Hausman test is for whether to use IV of OLS 
# significance means we should use IV as there is difference in IV and OLS coefficient

# 3 States

data_states <- fread("E:/Desktop(Current)/DACA/Data/New Data/Newest Data/DACA Eligible (past settlement IV)/All3 (from 2012)_Elig.csv")

#subset of data

data_states <- subset(data_states, YEAR>=2012)

# industry dummy

data_states$Ind_01 <- ifelse(data_states$industry==1, 1, 0)
data_states$Ind_02 <- ifelse(data_states$industry==2, 1, 0)
data_states$Ind_03 <- ifelse(data_states$industry==3, 1, 0)
data_states$Ind_04 <- ifelse(data_states$industry==4, 1, 0)
data_states$Ind_05 <- ifelse(data_states$industry==5, 1, 0)
data_states$Ind_06 <- ifelse(data_states$industry==6, 1, 0)
data_states$Ind_07 <- ifelse(data_states$industry==7, 1, 0)
data_states$Ind_08 <- ifelse(data_states$industry==8, 1, 0)
data_states$Ind_09 <- ifelse(data_states$industry==9, 1, 0)
data_states$Ind_10 <- ifelse(data_states$industry==10, 1, 0)
data_states$Ind_11 <- ifelse(data_states$industry==11, 1, 0)
data_states$Ind_12 <- ifelse(data_states$industry==12, 1, 0)
data_states$Ind_13 <- ifelse(data_states$industry==13, 1, 0)
data_states$Ind_14 <- ifelse(data_states$industry==14, 1, 0)
data_states$Ind_15 <- ifelse(data_states$industry==15, 1, 0)
data_states$Ind_16 <- ifelse(data_states$industry==16, 1, 0)

#schl dummy

data_states$schl_02 <- ifelse(data_states$schl==2, 1, 0)
data_states$schl_03 <- ifelse(data_states$schl==3, 1, 0)
data_states$schl_04 <- ifelse(data_states$schl==4, 1, 0)


#shock
data_states$shock <- data_states$m_ijt/(data_states$m_ijt+data_states$n_ijt)
data_states$logshock <- log(data_states$shock)

#Weight (for weighted regression)

data_states$weight <- sum(data_states$m_ijt)/(data_states$m_ijt)

# OLS (3 states)

reg1_states <- lm(weekwage ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                    Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 , data=data_states)

reg1_states_w <- lm(weekwage ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                      Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 , weights=weight, data=data_states)

summary(reg1_states)

summary(reg1_states_w)

reg2_states <- lm(yearwage ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                    Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 , data=data_states)

reg2_states_w <- lm(yearwage ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                      Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 , weights=weight, data=data_states)

summary(reg2_states)

summary(reg2_states_w)


reg3_states <- lm(hrswork  ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                    Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 , data=data_states)


reg3_states_w <- lm(hrswork  ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                      Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 , weights=weight, data=data_states)

summary(reg3_states)

summary(reg3_states_w)

data_states$wkwork1 <- data_states$wkwork*100

reg4_states <- lm(wkwork1 ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                    Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 , data=data_states)


reg4_states_w <- lm(wkwork1 ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                      Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 , weights=weight, data=data_states)

summary(reg4_states)

summary(reg4_states_w)

#IV (3 states)

summary(data_states$iv_m)

data_states$iv_m <- ifelse(data_states$iv_m == 0, 13, data_states$iv_m)

summary(data_states$iv_n)

data_states$iv_n <- ifelse(data_states$iv_n == 0, 78, data_states$iv_n)

data_states$IV <- data_states$iv_m/(data_states$iv_m+data_states$iv_n)

data_states$logIV <- log(data_states$IV)


#IV-weekwage (3 states)

ivmodel_1 <- ivreg(weekwage ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                     Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15|logIV+ factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                     Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 , data=data_states)


ivmodel_1_w <- ivreg(weekwage ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                       Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15|logIV+ factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                       Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 , weights=weight, data=data_states)

stargazer(ivmodel_1, type='text')

summary(ivmodel_1, diagnostics = TRUE)

summary(ivmodel_1, vcov=sandwich, diagnostics = TRUE)

summary(ivmodel_1_w, vcov=sandwich, diagnostics = TRUE) 

summary(ivmodel_1_w, diagnostics = TRUE) 


#IV-yearwage (3 states)

ivmodel_2 <- ivreg(yearwage ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                     Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15|logIV+ factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                     Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 , data=data_states)


ivmodel_2_w <- ivreg(yearwage ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                       Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15|logIV+ factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                       Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 , weights=weight, data=data_states)

stargazer(ivmodel_2, type='text')

summary(ivmodel_2, diagnostics = TRUE)

summary(ivmodel_2, vcov=sandwich, diagnostics = TRUE)

summary(ivmodel_2_w, vcov=sandwich, diagnostics = TRUE) 

summary(ivmodel_2_w, diagnostics = TRUE) 

#IV-hrswork (3 states)

ivmodel_3 <- ivreg(hrswork ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                     Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15|logIV+ factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                     Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 , data=data_states)

ivmodel_3_w <- ivreg(hrswork ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                       Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15|logIV+ factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                       Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15, weights=weight, data=data_states)


stargazer(ivmodel_3, type='text')

summary(ivmodel_3, diagnostics = TRUE)

summary(ivmodel_3, vcov=sandwich, diagnostics = TRUE)

summary(ivmodel_3_w, vcov=sandwich, diagnostics = TRUE) 

summary(ivmodel_3_w, diagnostics = TRUE) 

#IV-wkwork (3 states)

data_states$wkwork1 <- data_states$wkwork*100

ivmodel_4 <- ivreg(wkwork1 ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                     Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15|logIV+ factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                     Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 , data=data_states)

ivmodel_4_w <- ivreg(wkwork1 ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                       Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15|logIV+ factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                       Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 , weights=weight, data=data_states)

stargazer(ivmodel_4, type='text')

summary(ivmodel_4, diagnostics = TRUE)

summary(ivmodel_4, vcov=sandwich, diagnostics = TRUE)

summary(ivmodel_4_w, vcov=sandwich, diagnostics = TRUE) 

summary(ivmodel_4_w, diagnostics = TRUE) 


# Texas

data_tx <- fread("E:/Desktop(Current)/DACA/Data/New Data/Newest Data/DACA Eligible (past settlement IV)/Texas_Elig.csv")

#subset of data

data_tx <- subset(data_tx, YEAR>=2012)

# industry dummy

data_tx$Ind_01 <- ifelse(data_tx$industry==1, 1, 0)
data_tx$Ind_02 <- ifelse(data_tx$industry==2, 1, 0)
data_tx$Ind_03 <- ifelse(data_tx$industry==3, 1, 0)
data_tx$Ind_04 <- ifelse(data_tx$industry==4, 1, 0)
data_tx$Ind_05 <- ifelse(data_tx$industry==5, 1, 0)
data_tx$Ind_06 <- ifelse(data_tx$industry==6, 1, 0)
data_tx$Ind_07 <- ifelse(data_tx$industry==7, 1, 0)
data_tx$Ind_08 <- ifelse(data_tx$industry==8, 1, 0)
data_tx$Ind_09 <- ifelse(data_tx$industry==9, 1, 0)
data_tx$Ind_10 <- ifelse(data_tx$industry==10, 1, 0)
data_tx$Ind_11 <- ifelse(data_tx$industry==11, 1, 0)
data_tx$Ind_12 <- ifelse(data_tx$industry==12, 1, 0)
data_tx$Ind_13 <- ifelse(data_tx$industry==13, 1, 0)
data_tx$Ind_14 <- ifelse(data_tx$industry==14, 1, 0)
data_tx$Ind_15 <- ifelse(data_tx$industry==15, 1, 0)
data_tx$Ind_16 <- ifelse(data_tx$industry==16, 1, 0)

#schl dummy

data_tx$schl_02 <- ifelse(data_tx$schl==2, 1, 0)
data_tx$schl_03 <- ifelse(data_tx$schl==3, 1, 0)
data_tx$schl_04 <- ifelse(data_tx$schl==4, 1, 0)

#shock
data_tx$shock <- data_tx$m_ijt/(data_tx$m_ijt+data_tx$n_ijt)
data_tx$logshock <- log(data_tx$shock)

#Weight (for weighted regression)

data_tx$weight <- sum(data_tx$m_ijt)/(data_tx$m_ijt)

# OLS (Texas)

reg1_tx <- lm(weekwage ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 , data=data_tx)

reg1_tx_w <- lm(weekwage ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                  Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 , weights= weight, data=data_tx)

summary(reg1_tx)
summary(reg1_tx_w)

reg2_tx <- lm(yearwage ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 , data=data_tx)

reg2_tx_w <- lm(yearwage ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                  Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 , weights=weight, data=data_tx)

summary(reg2_tx)
summary(reg2_tx_w)

reg3_tx <- lm(hrswork  ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 , data=data_tx)

reg3_tx_w <- lm(hrswork  ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                  Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 , weights=weight, data=data_tx)

summary(reg3_tx)
summary(reg3_tx_w)

data_tx$wkwork1 <- data_tx$wkwork*100

reg4_tx <- lm(wkwork1 ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 , data=data_tx)


reg4_tx_w <- lm(wkwork1 ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                  Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 , weights=weight, data=data_tx)

summary(reg4_tx)

summary(reg4_tx_w)

#IV (Texas)

summary(data_tx$iv_m)

data_tx$iv_m <- ifelse(data_tx$iv_m == 0, 4, data_tx$iv_m)

summary(data_tx$iv_n)

data_tx$iv_n <- ifelse(data_tx$iv_n == 0, 38, data_tx$iv_n)

data_tx$IV <- data_tx$iv_m/(data_tx$iv_m+data_tx$iv_n)

data_tx$logIV <- log(data_tx$IV)


#IV-weekwage (Texas)

ivmodel_1 <- ivreg(weekwage ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                     Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15|logIV+ factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                     Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 , data=data_tx)


ivmodel_1_w <- ivreg(weekwage ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                       Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15|logIV+ factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                       Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 , weights=weight, data=data_tx)

stargazer(ivmodel_1, type='text')

summary(ivmodel_1, diagnostics= TRUE)

summary(ivmodel_1, vcov=sandwich, diagnostics= TRUE)

summary(ivmodel_1_w, vcov=sandwich, diagnostics = TRUE) 

summary(ivmodel_1_w, diagnostics = TRUE) 

#IV-yearwage (Texas)

ivmodel_2 <- ivreg(yearwage ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                     Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15|logIV+ factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                     Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 , data=data_tx)


ivmodel_2_w <- ivreg(yearwage ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                       Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15|logIV+ factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                       Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 , weights=weight, data=data_tx)

stargazer(ivmodel_2, type='text')

summary(ivmodel_2, diagnostics= TRUE)

summary(ivmodel_2, vcov=sandwich, diagnostics= TRUE)

summary(ivmodel_2_w, vcov=sandwich, diagnostics = TRUE) 

summary(ivmodel_2_w, diagnostics = TRUE) 

#IV-hrswork (Texas)

ivmodel_3 <- ivreg(hrswork ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                     Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15|logIV + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                     Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 , data=data_tx)

ivmodel_3_w <- ivreg(hrswork ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                       Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15|logIV+ factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                       Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 , weights=weight, data=data_tx)

stargazer(ivmodel_3, type='text')

summary(ivmodel_3, diagnostics= TRUE)

summary(ivmodel_3, vcov=sandwich, diagnostics= TRUE)

summary(ivmodel_3_w, vcov=sandwich, diagnostics = TRUE) 

summary(ivmodel_3_w, diagnostics = TRUE) 

#IV-wkwork (Texas)

data_tx$wkwork1 <- data_tx$wkwork*100

ivmodel_4 <- ivreg(wkwork1 ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                     Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15|logIV+ factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                     Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 , data=data_tx)

ivmodel_4_w <- ivreg(wkwork1 ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                       Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15|logIV+ factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                       Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 , weights=weight, data=data_tx)

stargazer(ivmodel_4, type='text')

summary(ivmodel_4, diagnostics= TRUE)

summary(ivmodel_4, vcov=sandwich, diagnostics= TRUE)

summary(ivmodel_4_w, vcov=sandwich, diagnostics = TRUE) 

summary(ivmodel_4_w, diagnostics = TRUE) 

# Florida

data_fl <- fread("E:/Desktop(Current)/DACA/Data/New Data/Newest Data/DACA Eligible (past settlement IV)/florida_Elig.csv")

#subset of data

data_fl <- subset(data_fl, YEAR>=2012)

# industry dummy

data_fl$Ind_01 <- ifelse(data_fl$industry==1, 1, 0)
data_fl$Ind_02 <- ifelse(data_fl$industry==2, 1, 0)
data_fl$Ind_03 <- ifelse(data_fl$industry==3, 1, 0)
data_fl$Ind_04 <- ifelse(data_fl$industry==4, 1, 0)
data_fl$Ind_05 <- ifelse(data_fl$industry==5, 1, 0)
data_fl$Ind_06 <- ifelse(data_fl$industry==6, 1, 0)
data_fl$Ind_07 <- ifelse(data_fl$industry==7, 1, 0)
data_fl$Ind_08 <- ifelse(data_fl$industry==8, 1, 0)
data_fl$Ind_09 <- ifelse(data_fl$industry==9, 1, 0)
data_fl$Ind_10 <- ifelse(data_fl$industry==10, 1, 0)
data_fl$Ind_11 <- ifelse(data_fl$industry==11, 1, 0)
data_fl$Ind_12 <- ifelse(data_fl$industry==12, 1, 0)
data_fl$Ind_13 <- ifelse(data_fl$industry==13, 1, 0)
data_fl$Ind_14 <- ifelse(data_fl$industry==14, 1, 0)
data_fl$Ind_15 <- ifelse(data_fl$industry==15, 1, 0)
data_fl$Ind_16 <- ifelse(data_fl$industry==16, 1, 0)

#schl dummy

data_fl$schl_02 <- ifelse(data_fl$schl==2, 1, 0)
data_fl$schl_03 <- ifelse(data_fl$schl==3, 1, 0)
data_fl$schl_04 <- ifelse(data_fl$schl==4, 1, 0)

#shock
data_fl$shock <- data_fl$m_ijt/(data_fl$m_ijt+data_fl$n_ijt)
data_fl$logshock <- log(data_fl$shock)

# Weights *for weighted regression)

data_fl$weight <- sum(data_fl$m_ijt)/(data_fl$m_ijt)

# OLS (Florida)

reg1_fl <- lm(weekwage ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 , data=data_fl)

reg1_fl_w <- lm(weekwage ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                  Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 , weights=weight, data=data_fl)

summary(reg1_fl)

summary(reg1_fl_w)

reg2_fl <- lm(yearwage ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 , data=data_fl)

reg2_fl_w <- lm(yearwage ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                  Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 , weights=weight,  data=data_fl)

summary(reg2_fl)

summary(reg2_fl_w)


reg3_fl <- lm(hrswork  ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 , data=data_fl)

reg3_fl_w <- lm(hrswork  ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                  Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 , weights=weight, data=data_fl)

summary(reg3_fl)

summary(reg3_fl_w)

data_fl$wkwork1 <- data_fl$wkwork*100

reg4_fl <- lm(wkwork1 ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 , data=data_fl)


reg4_fl_w <- lm(wkwork1 ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                  Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 , weights=weight, data=data_fl)

summary(reg4_fl)

summary(reg4_fl_w)

#IV (Florida)

summary(data_fl$iv_m)

data_fl$iv_m <- ifelse(data_fl$iv_m == 0, 8, data_fl$iv_m)

summary(data_fl$iv_n)

data_fl$iv_n <- ifelse(data_fl$iv_n == 0, 43, data_fl$iv_n)

data_fl$IV <- data_fl$iv_m/(data_fl$iv_m+data_fl$iv_n)

data_fl$IV <- ifelse(data_fl$IV == 0, 1, data_fl$IV)

data_fl$logIV <- log(data_fl$IV)

#IV2

data_fl$IV2 <- data_fl$iv_M_ijt/(data_fl$n_ijt)

data_fl$IV2 <- ifelse(data_fl$IV2 == 0, 1, data_fl$IV2)

data_fl$logIV2 <- log(data_fl$IV2)

#IV-weekwage (Florida)

ivmodel_1 <- ivreg(weekwage ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                     Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15|logIV+ factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                     Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 , data=data_fl)

ivmodel_1_w <- ivreg(weekwage ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                       Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15|logIV+ factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                       Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 , weights=weight,  data=data_fl)


stargazer(ivmodel_1, type='text')

summary(ivmodel_1, diagnostics= TRUE)

summary(ivmodel_1, vcov=sandwich, diagnostics= TRUE)

summary(ivmodel_1_w, vcov=sandwich, diagnostics = TRUE) 

summary(ivmodel_1_w, diagnostics = TRUE) 

#IV-yearwage (Florida)

ivmodel_2 <- ivreg(yearwage ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                     Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15|logIV+ factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                     Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 , data=data_fl)

ivmodel_2_w <- ivreg(yearwage ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                       Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15|logIV+ factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                       Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 , weights=weight, data=data_fl)

stargazer(ivmodel_2, type='text')

summary(ivmodel_2, diagnostics = TRUE)

summary(ivmodel_2,vcov=sandwich, diagnostics = TRUE)

summary(ivmodel_2_w, vcov=sandwich, diagnostics = TRUE) 

summary(ivmodel_2_w, diagnostics = TRUE) 

#IV-hrswork (FL)

ivmodel_3 <- ivreg(hrswork ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                     Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15|logIV+ factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                     Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 , data=data_fl)

ivmodel_3_w <- ivreg(hrswork ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                       Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15|logIV+ factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                       Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 , weights=weight, data=data_fl)

stargazer(ivmodel_3, type='text')

summary(ivmodel_3, diagnostics = TRUE)

summary(ivmodel_3,vcov=sandwich, diagnostics = TRUE)

summary(ivmodel_3_w, diagnostics = TRUE) 

summary(ivmodel_3_w, vcov=sandwich, diagnostics = TRUE) 

#IV-wkwork (FL)

data_fl$wkwork1 <- data_fl$wkwork*100

ivmodel_4 <- ivreg(wkwork1 ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                     Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15|logIV+ factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                     Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 , data=data_fl)

ivmodel_4_w <- ivreg(wkwork1 ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                       Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15|logIV+ factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                       Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 , weights=weight, data=data_fl)

stargazer(ivmodel_4, type='text')

summary(ivmodel_4, diagnostics = TRUE)

summary(ivmodel_4, vcov=sandwich, diagnostics = TRUE)

summary(ivmodel_4_w, vcov=sandwich, diagnostics = TRUE) 

summary(ivmodel_4_w, diagnostics = TRUE) 

# California

data_cali <- fread("E:/Desktop(Current)/DACA/Data/New Data/Newest Data/DACA Eligible (past settlement IV)/Original grouped data 2 cali_Elig.csv")

#subset of data

data_cali <- subset(data_cali, YEAR>=2012)

# industry dummy

data_cali$Ind_01 <- ifelse(data_cali$industry==1, 1, 0)
data_cali$Ind_02 <- ifelse(data_cali$industry==2, 1, 0)
data_cali$Ind_03 <- ifelse(data_cali$industry==3, 1, 0)
data_cali$Ind_04 <- ifelse(data_cali$industry==4, 1, 0)
data_cali$Ind_05 <- ifelse(data_cali$industry==5, 1, 0)
data_cali$Ind_06 <- ifelse(data_cali$industry==6, 1, 0)
data_cali$Ind_07 <- ifelse(data_cali$industry==7, 1, 0)
data_cali$Ind_08 <- ifelse(data_cali$industry==8, 1, 0)
data_cali$Ind_09 <- ifelse(data_cali$industry==9, 1, 0)
data_cali$Ind_10 <- ifelse(data_cali$industry==10, 1, 0)
data_cali$Ind_11 <- ifelse(data_cali$industry==11, 1, 0)
data_cali$Ind_12 <- ifelse(data_cali$industry==12, 1, 0)
data_cali$Ind_13 <- ifelse(data_cali$industry==13, 1, 0)
data_cali$Ind_14 <- ifelse(data_cali$industry==14, 1, 0)
data_cali$Ind_15 <- ifelse(data_cali$industry==15, 1, 0)
data_cali$Ind_16 <- ifelse(data_cali$industry==16, 1, 0)

#schl dummy

data_cali$schl_02 <- ifelse(data_cali$schl==2, 1, 0)
data_cali$schl_03 <- ifelse(data_cali$schl==3, 1, 0)
data_cali$schl_04 <- ifelse(data_cali$schl==4, 1, 0)


#shock
data_cali$shock <- data_cali$m_ijt/(data_cali$m_ijt+data_cali$n_ijt)
data_cali$logshock <- log(data_cali$shock)

#Weight (for weighted regression)

data_cali$weight <- sum(data_cali$m_ijt)/(data_cali$m_ijt)

data_cali[data_cali == "Inf"] <- "0" #(as some of the weight values were infinity)


#save the updated data for california (eligible)
write.csv(data_cali, "E:/Desktop(Current)/DACA/Data/New Data/Newest Data/DACA Eligible (past settlement IV)/cali_Elig(updated).csv")

#after imputing the 'inf' values, re-read the california data for eligibles

data_cali <- fread("E:/Desktop(Current)/DACA/Data/New Data/Newest Data/DACA Eligible (past settlement IV)/cali_Elig(updated).csv")

# OLS (California)

reg1_cali <- lm(weekwage ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                  Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 , data=data_cali)

reg1_cali_w <- lm(weekwage ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                    Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 , weights=weight, data=data_cali)

summary(reg1_cali)

summary(reg1_cali_w)

reg2_cali <- lm(yearwage ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                  Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 , data=data_cali)

reg2_cali_w <- lm(yearwage ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                    Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 , weights= weight, data=data_cali)

summary(reg2_cali)

summary(reg2_cali_w)


reg3_cali <- lm(hrswork  ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                  Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 , data=data_cali)

reg3_cali_w <- lm(hrswork  ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                    Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 , weights= weight, data=data_cali)

summary(reg3_cali)

summary(reg3_cali_w)

data_cali$wkwork1 <- data_cali$wkwork*100

reg4_cali <- lm(wkwork1 ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                  Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 , data=data_cali)


reg4_cali_w <- lm(wkwork1 ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                    Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 , weights=weight, data=data_cali)


summary(reg4_cali)

summary(reg4_cali_w)

#IV (California)

summary(data_cali$iv_n)

data_cali$iv_n <- ifelse(data_cali$iv_n == 0, 42, data_cali$iv_n)

summary(data_cali$iv_m)

data_cali$iv_m <- ifelse(data_cali$iv_m == 0, 9, data_cali$iv_m)


data_cali$IV <- data_cali$iv_m/(data_cali$iv_m+data_cali$iv_n)

summary(data_cali$IV)
data_cali$IV <- ifelse(data_cali$IV == 0, 0.18851, data_cali$IV)

data_cali$logIV <- log(data_cali$IV)


#IV2

data_cali$IV2 <- data_cali$iv_M_ijt/(data_cali$n_ijt)

data_cali$IV2 <- ifelse(data_cali$IV2 == 0, 1, data_cali$IV2)

data_cali$logIV2 <- log(data_cali$IV2)

#IV-weekwage (cali)

ivmodel_1 <- ivreg(weekwage ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                     Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15|logIV+ factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                     Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 , data=data_cali)


ivmodel_1_w <- ivreg(weekwage ~ logshock  |logIV, weights=weight, data=data_cali)

stargazer(ivmodel_1, type='text')

summary(ivmodel_1, diagnostics = TRUE)

summary(ivmodel_1, vcov=sandwich, diagnostics = TRUE)

summary(ivmodel_1_w, vcov=sandwich, diagnostics = TRUE) 

summary(ivmodel_1_w, diagnostics = TRUE) 

#IV-yearwage (CA)

ivmodel_2 <- ivreg(yearwage ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                     Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15|logIV+ factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                     Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 , data=data_cali)

ivmodel_2_w <- ivreg(yearwage ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                       Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15|logIV+ factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                       Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 , weights=weight, data=data_cali)

stargazer(ivmodel_2, type='text')

summary(ivmodel_2, diagnostics= TRUE)

summary(ivmodel_2, vcov=sandwich, diagnostics= TRUE)

summary(ivmodel_2_w, vcov=sandwich, diagnostics = TRUE) 

summary(ivmodel_2_w, diagnostics = TRUE) 

#IV-hrswork (CA)

ivmodel_3 <- ivreg(hrswork ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                     Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15|logIV+ factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                     Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 , data=data_cali)

ivmodel_3_w <- ivreg(hrswork ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                       Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15|logIV+ factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                       Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 , weights=weight, data=data_cali)


stargazer(ivmodel_3, type='text')

summary(ivmodel_3, diagnostics = TRUE)

summary(ivmodel_3, vcov=sandwich, diagnostics = TRUE) 

summary(ivmodel_3_w, vcov=sandwich, diagnostics = TRUE) 

summary(ivmodel_3_w,diagnostics = TRUE)

#IV-wkwork (CA)

data_cali$wkwork1 <- data_cali$wkwork*100

ivmodel_4 <- ivreg(wkwork1 ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                     Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15|logIV+ factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                     Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 , data=data_cali)

ivmodel_4_w <- ivreg(wkwork1 ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                       Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15|logIV+ factor(exptotal)+ factor(YEAR) + factor(schl) +Ind_01 + Ind_02 + Ind_03 + Ind_04 + Ind_05 + Ind_06 +
                       Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11 + Ind_12 + Ind_13 + Ind_14 + Ind_15 , weights=weight, data=data_cali)

stargazer(ivmodel_4, type='text')

summary(ivmodel_4, diagnostics = TRUE)

summary(ivmodel_4, vcov=sandwich, diagnostics = TRUE) 

summary(ivmodel_4_w, vcov=sandwich, diagnostics = TRUE) 

summary(ivmodel_4_w,diagnostics = TRUE) 
 