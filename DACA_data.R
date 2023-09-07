library(magrittr)
library(tidyverse)
library(dplyr)
library(data.table)

daca <- fread("usa_00052.csv")
daca <- as.data.table(daca)
daca <- daca[!(daca$INCWAGE == 999999 | daca$INCWAGE == 999998),]

daca$birthyear <- daca$YEAR - daca$AGE
daca$entryage <- daca$YRIMMIG - daca$birthyear
daca$schl <- fifelse(daca$EDUC <= 05, 1, fifelse(daca$EDUC == 06, 2, fifelse(daca$EDUC >= 07 
                                                                             & daca$EDUC < 10, 3, fifelse(daca$EDUC >= 10, 4, 0))))

test_subset <- subset(daca, schl==3)
summary(test_subset$EDUC)

daca <- daca[!(daca$schl==0),] #deleting schl=0 because these have no schooling information
daca$entry <- fifelse(daca$schl == 1, 17, fifelse(daca$schl == 2, 19, fifelse(daca$schl == 3, 
                                                                              21, fifelse(daca$schl == 4, 23, 0))))
nrow(subset(daca, entry ==0))

daca <- daca[!(daca$entry==0),]
daca$state <- daca$STATEFIP

daca$state <- fifelse(daca$state == 53, 03, fifelse(daca$state == 54, 07, fifelse(daca$state == 55, 
                                                                                  14, fifelse(daca$state == 56, 43, daca$state))))
daca$exp <- daca$AGE-daca$entry

nrow(subset(daca, exp==0))

#might want to calculate how many exp=0

daca$expgroup <- fifelse(daca$exp == 0, 1, fifelse(daca$exp > 0 & daca$exp <= 5, 2, fifelse(daca$exp >= 6 & 
                                                                                              daca$exp <= 10, 3, fifelse(daca$exp >= 11 & daca$exp <= 15, 4, fifelse(daca$exp >= 16 & 
                                                                                                                                                                       daca$exp <= 20, 5, fifelse(daca$exp >= 21 & daca$exp <= 25, 6, fifelse(daca$exp >= 26 & 
                                                                                                                                                                                                                                                daca$exp <= 30, 7, fifelse(daca$exp >= 31 & daca$exp <= 35, 8, fifelse(daca$exp >= 36 & 
                                                                                                                                                                                                                                                                                                                         daca$exp <= 40, 9, 10)))))))))
nrow(subset(daca, expgroup==0))

#daca <- daca[!(daca$YEAR==2006 | daca$YEAR==2005),]

daca$realincome <- fifelse(daca$YEAR == 2005, daca$INCWAGE*251.11/195.3, fifelse(daca$YEAR == 2006, 
                                                                                 daca$INCWAGE*251.11/201.6, fifelse(daca$YEAR == 2007, daca$INCWAGE*251.11/207.34, 
                                                                                                                    fifelse(daca$YEAR == 2008, daca$INCWAGE*251.11/215.30, fifelse(daca$YEAR == 2009, 
                                                                                                                                                                                   daca$INCWAGE*251.11/214.54, fifelse(daca$YEAR == 2010, daca$INCWAGE*251.11/218.06, 
                                                                                                                                                                                                                       fifelse(daca$YEAR == 2011, daca$INCWAGE*251.11/224.94, fifelse(daca$YEAR == 2012, 
                                                                                                                                                                                                                                                                                      daca$INCWAGE*251.11/229.59, fifelse(daca$YEAR == 2013, daca$INCWAGE*251.11/232.96, 
                                                                                                                                                                                                                                                                                                                          fifelse(daca$YEAR == 2014, daca$INCWAGE*251.11/236.74, fifelse(daca$YEAR == 2015, 
                                                                                                                                                                                                                                                                                                                                                                                         daca$INCWAGE*251.11/237.02, fifelse(daca$YEAR == 2016, daca$INCWAGE*251.11/240, 
                                                                                                                                                                                                                                                                                                                                                                                                                             fifelse(daca$YEAR == 2017, daca$INCWAGE*251.11/245.12, daca$INCWAGE)))))))))))))

#Source: https://www.bls.gov/regions/midwest/data/consumerpriceindexhistorical_us_table.pdf

daca$weekworked <- fifelse(daca$WKSWORK2 == 1, 6.5, fifelse(daca$WKSWORK2 == 2, 20, 
                                                            fifelse(daca$WKSWORK2 == 3, 33, fifelse(daca$WKSWORK2 == 4, 43.5, 
                                                                                                    fifelse(daca$WKSWORK2 == 5, 48.5, fifelse(daca$WKSWORK2 == 6, 51, 0))))))

nrow(subset(daca, weekworked==0))

#daca$weekwage <- daca$realincome/daca$weekworked

daca$weekwage <- fifelse(daca$weekworked!=0, daca$realincome/daca$weekworked, 0)

#daca <- daca[!(daca$weekwage==0),]
#daca$logweekwage <- log(daca$weekwage)

daca$logweekwage <- fifelse(daca$weekwage!=0, log(daca$weekwage), 0)

daca$fracwrkd <- daca$weekworked/52 
#daca$logrealinc <- log(daca$realincome)

daca$logrealinc <- fifelse(daca$realincome!=0, log(daca$realincome), 0)

daca$Birth81 <- fifelse(daca$birthyear > 1981, 1, 0)
daca$Birth78_81 <- fifelse(daca$birthyear <= 1981 & daca$birthyear > 1978, 1, 0)
daca$Birth75_78 <- fifelse(daca$birthyear <= 1978 & daca$birthyear > 1975, 1, 0)
daca$Birth72_75 <- fifelse(daca$birthyear <= 1975 & daca$birthyear > 1972, 1, 0)
daca$Birth69_72 <- fifelse(daca$birthyear <= 1972 & daca$birthyear > 1969, 1, 0)
daca$Birth69 <- fifelse(daca$birthyear <= 1969, 1, 0)

daca$Schl_1 <- fifelse(daca$schl==1, 1, 0)
daca$Schl_2 <- fifelse(daca$schl==2, 1, 0)
daca$Schl_3 <- fifelse(daca$schl==3, 1, 0)
daca$Schl_4 <- fifelse(daca$schl==4, 1, 0)

daca$Exp_1 <- fifelse(daca$expgroup==1, 1, 0)
daca$Exp_2 <- fifelse(daca$expgroup==2, 1, 0)
daca$Exp_3 <- fifelse(daca$expgroup==3, 1, 0)
daca$Exp_4 <- fifelse(daca$expgroup==4, 1, 0)
daca$Exp_5 <- fifelse(daca$expgroup==5, 1, 0)
daca$Exp_6 <- fifelse(daca$expgroup >= 6, 1, 0)


daca$industry <- fifelse(daca$IND >= 170 & daca$IND <= 290, 1, fifelse(daca$IND >= 8560 & 
                                                                         daca$IND <= 8690, 2, fifelse(daca$IND == 770, 3, fifelse(daca$IND >= 7860 & 
                                                                                                                                    daca$IND <= 8470, 4, fifelse(daca$IND >= 6870 & daca$IND <= 7190, 5, fifelse(
                                                                                                                                      daca$IND >= 6470 & daca$IND <= 6780, 6, fifelse(daca$IND >= 1070 & 
                                                                                                                                                                                        daca$IND <= 3990, 7, fifelse(daca$IND >= 370 & daca$IND <= 490, 8, fifelse(
                                                                                                                                                                                          daca$IND >= 7270 & daca$IND <= 7790, 9, fifelse(daca$IND >= 9370 & 
                                                                                                                                                                                                                                            daca$IND <= 9590, 10, fifelse(daca$IND >= 4670 & daca$IND <= 5790, 11, fifelse(
                                                                                                                                                                                                                                              daca$IND >= 6070 & daca$IND <= 6390, 12, fifelse(daca$IND >= 570 & 
                                                                                                                                                                                                                                                                                                 daca$IND <= 690, 13, fifelse(daca$IND >= 4070 & daca$IND <= 4590, 14, fifelse(
                                                                                                                                                                                                                                                                                                   daca$IND >= 8770 & daca$IND <= 9290, 15, fifelse(daca$IND >= 9670 & 
                                                                                                                                                                                                                                                                                                                                                      daca$IND <= 9870, 16, 17))))))))))))))))

#check how many industry=17

data$exptotal <- fifelse(data$Exp_1 == 1, 1, fifelse(data$Exp_2 == 1, 2, fifelse(data$Exp_3 == 1, 3, 
                                                                                 fifelse(data$Exp_4 == 1, 4, fifelse(data$Exp_5 == 1, 5, 6)))))

data$exptotal <- fifelse(data$Exp_1 == 1, 1, fifelse(data$Exp_2 == 1, 2, fifelse(data$Exp_3 == 1, 3, 
                                                                                 fifelse(data$Exp_4 == 1, 4, 5))))

daca$birthtotal <- fifelse(daca$Birth81 == 1, 1, fifelse(daca$Birth78_81 == 1, 2, fifelse(daca$Birth75_78 == 1, 3,
                                                                                          fifelse(daca$Birth72_75 == 1, 4, fifelse(daca$Birth69_72 == 1, 5, 6)))))

#yearsusa 

data$yearsusa <- fifelse(data$status_mexico==1, data$AGE, data$AGE-data$entryage)
data$SEX <- fifelse(data$SEX==2, 1, 0)

#mexican non citizens method to find DACA eligibles

#appears to be the correct method

daca$illegal_mexico <- fifelse(daca$CITIZEN==3 & daca$BPL==200, 1, 0)

daca$status_fs_mexico <- fifelse(daca$birthyear > 1981 & daca$schl >= 2 & daca$entryage < 16 & 
                                   daca$YRIMMIG < 2007, 5, fifelse(daca$birthyear <= 1981 | 
                                                                     daca$schl < 2 | daca$YRIMMIG >= 2007 | daca$entryage >= 16, 4, 0))

daca$status2_mexico <- daca$illegal_mexico*daca$status_fs_mexico

summary(daca$status2_mexico)
nrow(subset(daca, status2_mexico==5))

daca$legal <- 1-daca$illegal_mexico
sum(nrow(subset(daca, illegal_mexico==1)), nrow(subset(daca, legal==1)))

daca$citizen <- fifelse(daca$CITIZEN <= 1, 1, daca$CITIZEN)

daca$status3_mexico <- daca$legal*daca$citizen

daca$status_mexico <- fifelse(daca$legal==1, daca$status3_mexico, daca$status2_mexico)
nrow(subset(daca, status_mexico==1)) #natural born citizens
nrow(subset(daca, status_mexico==2)) #naturalized citizen
nrow(subset(daca, status_mexico==3)) #legal immigrants
nrow(subset(daca, status_mexico==4)) #daca ineligible
nrow(subset(daca, status_mexico==5)) #daca eligible

fwrite(daca, "daca_chap3.csv")

#regression
#if you want to include only those in the labor force, use this:

data <- daca

data <- data[(data$EMPSTAT==2 | data$EMPSTAT==1),]
#data <- data[(data$CLASSWKR==2),]
#data <- data[(data$industry<17),]

#data <- data[!(data$logrealinc==0),]
#data <- data[!(data$YEAR==2005 | data$YEAR==2006 | data$YEAR==2007),]

reg1 <- lm(logrealinc ~ treat_post + treat + post + factor(industry) + factor(schl) + Birth81 + 
             Birth78_81 + Birth75_78 + Birth72_75 + Birth69_72 + exp + SEX + yearsusa + factor(YEAR), 
           data=data)

library(fixest)
data$treat <- ifelse(data$status_mexico==5, 1, 0)
data$period <- data$YEAR-2004
reg3 <- feols(logrealinc ~ factor(industry)+factor(schl)+ exp + SEX + yearsusa + 
                i(treat, period), data=data)
reg3 <- feols(logrealinc ~ factor(industry)+factor(schl)+ exp + SEX + yearsusa + 
                i(treat, period) | period, data=data)
reg3 <- lm(logrealinc ~ factor(industry)+factor(schl)+exp+SEX+yearsusa+i(treat, period) | period, data=data)

data$treat <- ifelse(data$status_mexico==5, 1, 0)
data$post <- ifelse(data$YEAR>2012, 1, 0)

data[,c("COUNTYFIP", "SPLOC", "BPLD", "EDUCD", "EMPSTATD", "CLASSWKRD", "OCCSOC", "illegal_mexico", 
        "status_fs_mexico", "status2_mexico", "status3_mexico", "citizen", "exptotal", 
        "Exp_1", "Exp_2", "Exp_3", "Exp_4", "Exp_5", "Exp_6", "Schl_1", "Schl_2", "Schl_3", "Schl_4",
        "weekwage", "weekworked", "realincome", "expgroup", "state", "INCWAGE", "IND", "EDUC")] <- list(NULL)

data$treat_post <- data$treat*data$post

#getting data by cohort

data <- fread("daca_chap3.csv")

#iv for ineligibles

data %>% group_by(YEAR, schl, birthtotal, exptotal, industry) %>%
  summarize(n_ijt=sum(status_mexico<5), #rest of the population
            m_ijt=sum(status_mexico==5), #daca eligible population
            yearwage <- mean(logrealinc[status_mexico<5], na.rm=TRUE),
            weekwage <- mean(logweekwage[status_mexico<5], na.rm=TRUE),
            wkwork <- mean(fracwrkd[status_mexico<5], na.rm=TRUE),
            hrswork <- mean(UHRSWORK[status_mexico<5], na.rm=TRUE),
            
            iv_n_ijt=sum(status_mexico==1), #rest of the population
            iv_m_ijt=sum(status_mexico>1), #immigrant population)
            
            skr_iv_n_ijt=sum(status_mexico<5 & EMPSTAT==2),
            skr_iv_m_ijt=sum(status_mexico==5 & EMPSTAT==2)) -> data1  

#doing the same for eligibles

data %>% group_by(YEAR, schl, birthtotal, exptotal, industry) %>%
  summarize(n_ijt=sum(status_mexico<5), #rest of the population
            m_ijt=sum(status_mexico==5), #daca eligible population
            yearwage <- mean(logrealinc[status_mexico==5], na.rm=TRUE),
            weekwage <- mean(logweekwage[status_mexico==5], na.rm=TRUE),
            wkwork <- mean(fracwrkd[status_mexico==5], na.rm=TRUE),
            hrswork <- mean(UHRSWORK[status_mexico==5], na.rm=TRUE),
            
            iv_n_ijt=sum(status_mexico==1), #rest of the population
            iv_m_ijt=sum(status_mexico>1), #immigrant population)
            
            skr_iv_n_ijt=sum(status_mexico<5 & EMPSTAT==2),
            skr_iv_m_ijt=sum(status_mexico==5 & EMPSTAT==2)) -> data1  

write.csv(data1, file="Original grouped data test.csv")
data1 <- data1[!(data1$m_ijt==0),]
write.csv(data1, file="Original grouped data 2 test.csv")
#remove industry 17
#remove exptotal 1

#pseudo panel

#includes unemployed too

data %>% group_by(YEAR, status_mexico, birthtotal, schl) %>%
  summarize(yearwage <- mean(logrealinc),
            weekwage <- mean(logweekwage),
            wkwork <- mean(fracwrkd),
            hrswork <- mean(UHRSWORK),
            
            birth81 <- sum(Birth81)/n(),
            birth78_81 <- sum(Birth78_81)/n(),
            birth75_78 <- sum(Birth75_78)/n(),
            birth72_75 <- sum(Birth72_75)/n(),
            birth69_72 <- sum(Birth69_72)/n(),
            birth69 <- sum(Birth69)/n(),
            mean_age <- mean(AGE),
            frac_women <- mean(SEX),
            mean_yearsusa1 <- mean(yearsusa[status_mexico>1], na.rm=TRUE),
            mean_yearsusa2 <- mean(yearsusa),
            
            schl_1 <- sum(Schl_1)/n(),
            schl_2 <- sum(Schl_2)/n(),
            schl_3 <- sum(Schl_3)/n(),
            schl_4 <- sum(Schl_4)/n(),
            mean_schl <- mean(schl),
            
            exp_1 <- sum(Exp_1)/n(),
            exp_2 <- sum(Exp_2)/n(),
            exp_3 <- sum(Exp_3)/n(),
            exp_4 <- sum(Exp_4)/n(),
            exp_5 <- sum(Exp_5)/n(),
            exp_6 <- sum(Exp_6)/n(),
            exp_mean <- mean(exp)) -> data_panel

data_panel$eligible <- ifelse(data_panel$status_mexico==5, 1, 0)
data_panel$UNIQUEID <- rep(1:99, times=14)

write.csv(data_panel, file="pseudo_panel.csv")

#skr iv #iv for ineligibles

data$empstat <- fifelse(data$EMPSTAT==2, 1, 0)
data$status_mexico_unemp <- data$status_mexico*data$empstat

data %>% group_by(YEAR, schl, birthtotal, exptotal) %>%
  summarize(n_ijt=sum(status_mexico<5), #rest of the population
            m_ijt=sum(status_mexico==5), #daca eligible population
            yearwage <- mean(logrealinc[status_mexico<5], na.rm=TRUE),
            weekwage <- mean(logweekwage[status_mexico<5], na.rm=TRUE),
            wkwork <- mean(fracwrkd[status_mexico<5], na.rm=TRUE),
            hrswork <- mean(UHRSWORK[status_mexico<5], na.rm=TRUE),
            
            iv_n_ijt=sum(status_mexico_unemp<5), #rest of the population
            iv_m_ijt=sum(status_mexico_unemp==5)) -> data1_all  #immigrant population)

write.csv(data1_all, file="data_all original.csv")
data1_all <- data1_all[!(data1_all$m_ijt==0),]
write.csv(data1_all, file="data_all.csv")
remove(data1_all)

#industry groups

data$empstat <- fifelse(data$EMPSTAT==2, 1, 0)
data$status_mexico_unemp <- data$status_mexico*data$empstat

data_ind1 <- data[industry==1]

data_ind1 %>% group_by(YEAR, schl, birthtotal, exptotal) %>%
  summarize(n_ijt=sum(status_mexico<5), #rest of the population
            m_ijt=sum(status_mexico==5), #daca eligible population
            yearwage <- mean(logrealinc[status_mexico<5], na.rm=TRUE),
            weekwage <- mean(logweekwage[status_mexico<5], na.rm=TRUE),
            wkwork <- mean(fracwrkd[status_mexico<5], na.rm=TRUE),
            hrswork <- mean(UHRSWORK[status_mexico<5], na.rm=TRUE),
            
            iv_n_ijt=sum(status_mexico==1), #rest of the population
            iv_m_ijt=sum(status_mexico>1), #immigrant population)
            
            skr_iv_n_ijt=sum(status_mexico_unemp<5), #rest of the population
            skr_iv_m_ijt=sum(status_mexico_unemp==5)) -> data1_ind1  #immigrant population)

write.csv(data1_ind1, file="Ind01 original.csv")
data1_ind1 <- data1_ind1[!(data1_ind1$m_ijt==0),]
write.csv(data1_ind1, file="Ind01.csv")
remove(data1_ind1)

data_ind2 <- data[industry==2]

data_ind2 %>% group_by(YEAR, schl, birthtotal, exptotal) %>%
  summarize(n_ijt=sum(status_mexico<5), #rest of the population
            m_ijt=sum(status_mexico==5), #daca eligible population
            yearwage <- mean(logrealinc[status_mexico<5], na.rm=TRUE),
            weekwage <- mean(logweekwage[status_mexico<5], na.rm=TRUE),
            wkwork <- mean(fracwrkd[status_mexico<5], na.rm=TRUE),
            hrswork <- mean(UHRSWORK[status_mexico<5], na.rm=TRUE),
            
            iv_n_ijt=sum(status_mexico==1), #rest of the population
            iv_m_ijt=sum(status_mexico>1), #immigrant population)
            
            skr_iv_n_ijt=sum(status_mexico_unemp<5), #rest of the population
            skr_iv_m_ijt=sum(status_mexico_unemp==5)) -> data1_ind2  #immigrant population)

write.csv(data1_ind2, file="Ind02 original.csv")
data1_ind2 <- data1_ind2[!(data1_ind2$m_ijt==0),]
write.csv(data1_ind2, file="Ind02.csv")
remove(data1_ind2)

data_ind3 <- data[industry==3]

data_ind3 %>% group_by(YEAR, schl, birthtotal, exptotal) %>%
  summarize(n_ijt=sum(status_mexico<5), #rest of the population
            m_ijt=sum(status_mexico==5), #daca eligible population
            yearwage <- mean(logrealinc[status_mexico<5], na.rm=TRUE),
            weekwage <- mean(logweekwage[status_mexico<5], na.rm=TRUE),
            wkwork <- mean(fracwrkd[status_mexico<5], na.rm=TRUE),
            hrswork <- mean(UHRSWORK[status_mexico<5], na.rm=TRUE),
            
            iv_n_ijt=sum(status_mexico==1), #rest of the population
            iv_m_ijt=sum(status_mexico>1), #immigrant population)
            
            skr_iv_n_ijt=sum(status_mexico_unemp<5), #rest of the population
            skr_iv_m_ijt=sum(status_mexico_unemp==5)) -> data1_ind3  #immigrant population)

write.csv(data1_ind3, file="Ind03 original.csv")
data1_ind3 <- data1_ind3[!(data1_ind3$m_ijt==0),]
write.csv(data1_ind3, file="Ind03.csv")
remove(data1_ind3)

data_ind4 <- data[industry==4]

data_ind4 %>% group_by(YEAR, schl, birthtotal, exptotal) %>%
  summarize(n_ijt=sum(status_mexico<5), #rest of the population
            m_ijt=sum(status_mexico==5), #daca eligible population
            yearwage <- mean(logrealinc[status_mexico<5], na.rm=TRUE),
            weekwage <- mean(logweekwage[status_mexico<5], na.rm=TRUE),
            wkwork <- mean(fracwrkd[status_mexico<5], na.rm=TRUE),
            hrswork <- mean(UHRSWORK[status_mexico<5], na.rm=TRUE),
            
            iv_n_ijt=sum(status_mexico==1), #rest of the population
            iv_m_ijt=sum(status_mexico>1), #immigrant population)
            
            skr_iv_n_ijt=sum(status_mexico_unemp<5), #rest of the population
            skr_iv_m_ijt=sum(status_mexico_unemp==5)) -> data1_ind4  #immigrant population)

write.csv(data1_ind4, file="Ind04 original.csv")
data1_ind4 <- data1_ind4[!(data1_ind4$m_ijt==0),]
write.csv(data1_ind4, file="Ind04.csv")
remove(data1_ind4)

data_ind5 <- data[industry==5]

data_ind5 %>% group_by(YEAR, schl, birthtotal, exptotal) %>%
  summarize(n_ijt=sum(status_mexico<5), #rest of the population
            m_ijt=sum(status_mexico==5), #daca eligible population
            yearwage <- mean(logrealinc[status_mexico<5], na.rm=TRUE),
            weekwage <- mean(logweekwage[status_mexico<5], na.rm=TRUE),
            wkwork <- mean(fracwrkd[status_mexico<5], na.rm=TRUE),
            hrswork <- mean(UHRSWORK[status_mexico<5], na.rm=TRUE),
            
            iv_n_ijt=sum(status_mexico==1), #rest of the population
            iv_m_ijt=sum(status_mexico>1), #immigrant population)
            
            skr_iv_n_ijt=sum(status_mexico_unemp<5), #rest of the population
            skr_iv_m_ijt=sum(status_mexico_unemp==5)) -> data1_ind5  #immigrant population)

write.csv(data1_ind5, file="Ind05 original.csv")
data1_ind5 <- data1_ind5[!(data1_ind5$m_ijt==0),]
write.csv(data1_ind5, file="Ind05.csv")
remove(data1_ind5)

data_ind6 <- data[industry==6]

data_ind6 %>% group_by(YEAR, schl, birthtotal, exptotal) %>%
  summarize(n_ijt=sum(status_mexico<5), #rest of the population
            m_ijt=sum(status_mexico==5), #daca eligible population
            yearwage <- mean(logrealinc[status_mexico<5], na.rm=TRUE),
            weekwage <- mean(logweekwage[status_mexico<5], na.rm=TRUE),
            wkwork <- mean(fracwrkd[status_mexico<5], na.rm=TRUE),
            hrswork <- mean(UHRSWORK[status_mexico<5], na.rm=TRUE),
            
            iv_n_ijt=sum(status_mexico==1), #rest of the population
            iv_m_ijt=sum(status_mexico>1), #immigrant population)
            
            skr_iv_n_ijt=sum(status_mexico_unemp<5), #rest of the population
            skr_iv_m_ijt=sum(status_mexico_unemp==5)) -> data1_ind6  #immigrant population)

write.csv(data1_ind6, file="Ind06 original.csv")
data1_ind6 <- data1_ind6[!(data1_ind6$m_ijt==0),]
write.csv(data1_ind6, file="Ind06.csv")
remove(data1_ind6)

data_ind7 <- data[industry==7]

data_ind7 %>% group_by(YEAR, schl, birthtotal, exptotal) %>%
  summarize(n_ijt=sum(status_mexico<5), #rest of the population
            m_ijt=sum(status_mexico==5), #daca eligible population
            yearwage <- mean(logrealinc[status_mexico<5], na.rm=TRUE),
            weekwage <- mean(logweekwage[status_mexico<5], na.rm=TRUE),
            wkwork <- mean(fracwrkd[status_mexico<5], na.rm=TRUE),
            hrswork <- mean(UHRSWORK[status_mexico<5], na.rm=TRUE),
            
            iv_n_ijt=sum(status_mexico==1), #rest of the population
            iv_m_ijt=sum(status_mexico>1), #immigrant population)
            
            skr_iv_n_ijt=sum(status_mexico_unemp<5), #rest of the population
            skr_iv_m_ijt=sum(status_mexico_unemp==5)) -> data1_ind7  #immigrant population)

write.csv(data1_ind7, file="Ind07 original.csv")
data1_ind7 <- data1_ind7[!(data1_ind7$m_ijt==0),]
write.csv(data1_ind7, file="Ind07.csv")
remove(data1_ind7)

data_ind8 <- data[industry==8]

data_ind8 %>% group_by(YEAR, schl, birthtotal, exptotal) %>%
  summarize(n_ijt=sum(status_mexico<5), #rest of the population
            m_ijt=sum(status_mexico==5), #daca eligible population
            yearwage <- mean(logrealinc[status_mexico<5], na.rm=TRUE),
            weekwage <- mean(logweekwage[status_mexico<5], na.rm=TRUE),
            wkwork <- mean(fracwrkd[status_mexico<5], na.rm=TRUE),
            hrswork <- mean(UHRSWORK[status_mexico<5], na.rm=TRUE),
            
            iv_n_ijt=sum(status_mexico==1), #rest of the population
            iv_m_ijt=sum(status_mexico>1), #immigrant population)
            
            skr_iv_n_ijt=sum(status_mexico_unemp<5), #rest of the population
            skr_iv_m_ijt=sum(status_mexico_unemp==5)) -> data1_ind8  #immigrant population)

write.csv(data1_ind8, file="Ind08 original.csv")
data1_ind8 <- data1_ind8[!(data1_ind8$m_ijt==0),]
write.csv(data1_ind8, file="Ind08.csv")
remove(data1_ind8)

data_ind9 <- data[industry==9]

data_ind9 %>% group_by(YEAR, schl, birthtotal, exptotal) %>%
  summarize(n_ijt=sum(status_mexico<5), #rest of the population
            m_ijt=sum(status_mexico==5), #daca eligible population
            yearwage <- mean(logrealinc[status_mexico<5], na.rm=TRUE),
            weekwage <- mean(logweekwage[status_mexico<5], na.rm=TRUE),
            wkwork <- mean(fracwrkd[status_mexico<5], na.rm=TRUE),
            hrswork <- mean(UHRSWORK[status_mexico<5], na.rm=TRUE),
            
            iv_n_ijt=sum(status_mexico==1), #rest of the population
            iv_m_ijt=sum(status_mexico>1), #immigrant population)
            
            skr_iv_n_ijt=sum(status_mexico_unemp<5), #rest of the population
            skr_iv_m_ijt=sum(status_mexico_unemp==5)) -> data1_ind9  #immigrant population)

write.csv(data1_ind9, file="Ind09 original.csv")
data1_ind9 <- data1_ind9[!(data1_ind9$m_ijt==0),]
write.csv(data1_ind9, file="Ind09.csv")
remove(data1_ind9)

data_ind10 <- data[industry==10]

data_ind10 %>% group_by(YEAR, schl, birthtotal, exptotal) %>%
  summarize(n_ijt=sum(status_mexico<5), #rest of the population
            m_ijt=sum(status_mexico==5), #daca eligible population
            yearwage <- mean(logrealinc[status_mexico<5], na.rm=TRUE),
            weekwage <- mean(logweekwage[status_mexico<5], na.rm=TRUE),
            wkwork <- mean(fracwrkd[status_mexico<5], na.rm=TRUE),
            hrswork <- mean(UHRSWORK[status_mexico<5], na.rm=TRUE),
            
            iv_n_ijt=sum(status_mexico==1), #rest of the population
            iv_m_ijt=sum(status_mexico>1), #immigrant population)
            
            skr_iv_n_ijt=sum(status_mexico_unemp<5), #rest of the population
            skr_iv_m_ijt=sum(status_mexico_unemp==5)) -> data1_ind10  #immigrant population)

write.csv(data1_ind10, file="Ind10 original.csv")
data1_ind10 <- data1_ind10[!(data1_ind10$m_ijt==0),]
write.csv(data1_ind10, file="Ind10.csv")
remove(data1_ind10)

data_ind11 <- data[industry==11]

data_ind11 %>% group_by(YEAR, schl, birthtotal, exptotal) %>%
  summarize(n_ijt=sum(status_mexico<5), #rest of the population
            m_ijt=sum(status_mexico==5), #daca eligible population
            yearwage <- mean(logrealinc[status_mexico<5], na.rm=TRUE),
            weekwage <- mean(logweekwage[status_mexico<5], na.rm=TRUE),
            wkwork <- mean(fracwrkd[status_mexico<5], na.rm=TRUE),
            hrswork <- mean(UHRSWORK[status_mexico<5], na.rm=TRUE),
            
            iv_n_ijt=sum(status_mexico==1), #rest of the population
            iv_m_ijt=sum(status_mexico>1), #immigrant population)
            
            skr_iv_n_ijt=sum(status_mexico_unemp<5), #rest of the population
            skr_iv_m_ijt=sum(status_mexico_unemp==5)) -> data1_ind11  #immigrant population)

write.csv(data1_ind11, file="Ind11 original.csv")
data1_ind11 <- data1_ind11[!(data1_ind11$m_ijt==0),]
write.csv(data1_ind11, file="Ind11.csv")
remove(data1_ind11)

data_ind12 <- data[industry==12]

data_ind12 %>% group_by(YEAR, schl, birthtotal, exptotal) %>%
  summarize(n_ijt=sum(status_mexico<5), #rest of the population
            m_ijt=sum(status_mexico==5), #daca eligible population
            yearwage <- mean(logrealinc[status_mexico<5], na.rm=TRUE),
            weekwage <- mean(logweekwage[status_mexico<5], na.rm=TRUE),
            wkwork <- mean(fracwrkd[status_mexico<5], na.rm=TRUE),
            hrswork <- mean(UHRSWORK[status_mexico<5], na.rm=TRUE),
            
            iv_n_ijt=sum(status_mexico==1), #rest of the population
            iv_m_ijt=sum(status_mexico>1), #immigrant population)
            
            skr_iv_n_ijt=sum(status_mexico_unemp<5), #rest of the population
            skr_iv_m_ijt=sum(status_mexico_unemp==5)) -> data1_ind12  #immigrant population)

write.csv(data1_ind12, file="Ind12 original.csv")
data1_ind12 <- data1_ind12[!(data1_ind12$m_ijt==0),]
write.csv(data1_ind12, file="Ind12.csv")
remove(data1_ind12)

data_ind13 <- data[industry==13]

data_ind13 %>% group_by(YEAR, schl, birthtotal, exptotal) %>%
  summarize(n_ijt=sum(status_mexico<5), #rest of the population
            m_ijt=sum(status_mexico==5), #daca eligible population
            yearwage <- mean(logrealinc[status_mexico<5], na.rm=TRUE),
            weekwage <- mean(logweekwage[status_mexico<5], na.rm=TRUE),
            wkwork <- mean(fracwrkd[status_mexico<5], na.rm=TRUE),
            hrswork <- mean(UHRSWORK[status_mexico<5], na.rm=TRUE),
            
            iv_n_ijt=sum(status_mexico==1), #rest of the population
            iv_m_ijt=sum(status_mexico>1), #immigrant population)
            
            skr_iv_n_ijt=sum(status_mexico_unemp<5), #rest of the population
            skr_iv_m_ijt=sum(status_mexico_unemp==5)) -> data1_ind13  #immigrant population)

write.csv(data1_ind13, file="Ind13 original.csv")
data1_ind13 <- data1_ind13[!(data1_ind13$m_ijt==0),]
write.csv(data1_ind13, file="Ind13.csv")
remove(data1_ind13)

data_ind14 <- data[industry==14]

data_ind14 %>% group_by(YEAR, schl, birthtotal, exptotal) %>%
  summarize(n_ijt=sum(status_mexico<5), #rest of the population
            m_ijt=sum(status_mexico==5), #daca eligible population
            yearwage <- mean(logrealinc[status_mexico<5], na.rm=TRUE),
            weekwage <- mean(logweekwage[status_mexico<5], na.rm=TRUE),
            wkwork <- mean(fracwrkd[status_mexico<5], na.rm=TRUE),
            hrswork <- mean(UHRSWORK[status_mexico<5], na.rm=TRUE),
            
            iv_n_ijt=sum(status_mexico==1), #rest of the population
            iv_m_ijt=sum(status_mexico>1), #immigrant population)
            
            skr_iv_n_ijt=sum(status_mexico_unemp<5), #rest of the population
            skr_iv_m_ijt=sum(status_mexico_unemp==5)) -> data1_ind14  #immigrant population)

write.csv(data1_ind14, file="Ind14 original.csv")
data1_ind14 <- data1_ind14[!(data1_ind14$m_ijt==0),]
write.csv(data1_ind14, file="Ind14.csv")
remove(data1_ind14)

data_ind15 <- data[industry==15]

data_ind15 %>% group_by(YEAR, schl, birthtotal, exptotal) %>%
  summarize(n_ijt=sum(status_mexico<5), #rest of the population
            m_ijt=sum(status_mexico==5), #daca eligible population
            yearwage <- mean(logrealinc[status_mexico<5], na.rm=TRUE),
            weekwage <- mean(logweekwage[status_mexico<5], na.rm=TRUE),
            wkwork <- mean(fracwrkd[status_mexico<5], na.rm=TRUE),
            hrswork <- mean(UHRSWORK[status_mexico<5], na.rm=TRUE),
            
            iv_n_ijt=sum(status_mexico==1), #rest of the population
            iv_m_ijt=sum(status_mexico>1), #immigrant population)
            
            skr_iv_n_ijt=sum(status_mexico_unemp<5), #rest of the population
            skr_iv_m_ijt=sum(status_mexico_unemp==5)) -> data1_ind15  #immigrant population)

write.csv(data1_ind15, file="Ind15 original.csv")
data1_ind15 <- data1_ind15[!(data1_ind15$m_ijt==0),]
write.csv(data1_ind15, file="Ind15.csv")
remove(data1_ind15)

data_ind16 <- data[industry==16]

data_ind16 %>% group_by(YEAR, schl, birthtotal, exptotal) %>%
  summarize(n_ijt=sum(status_mexico<5), #rest of the population
            m_ijt=sum(status_mexico==5), #daca eligible population
            yearwage <- mean(logrealinc[status_mexico<5], na.rm=TRUE),
            weekwage <- mean(logweekwage[status_mexico<5], na.rm=TRUE),
            wkwork <- mean(fracwrkd[status_mexico<5], na.rm=TRUE),
            hrswork <- mean(UHRSWORK[status_mexico<5], na.rm=TRUE),
            
            iv_n_ijt=sum(status_mexico==1), #rest of the population
            iv_m_ijt=sum(status_mexico>1), #immigrant population)
            
            skr_iv_n_ijt=sum(status_mexico_unemp<5), #rest of the population
            skr_iv_m_ijt=sum(status_mexico_unemp==5)) -> data1_ind16  #immigrant population)

write.csv(data1_ind16, file="Ind16 original.csv")
data1_ind16 <- data1_ind16[!(data1_ind16$m_ijt==0),]
write.csv(data1_ind16, file="Ind16.csv")
remove(data1_ind16)

data_ind17 <- data[industry==17]

data_ind17 %>% group_by(YEAR, schl, birthtotal, exptotal) %>%
  summarize(n_ijt=sum(status_mexico<5), #rest of the population
            m_ijt=sum(status_mexico==5), #daca eligible population
            yearwage <- mean(logrealinc[status_mexico<5], na.rm=TRUE),
            weekwage <- mean(logweekwage[status_mexico<5], na.rm=TRUE),
            wkwork <- mean(fracwrkd[status_mexico<5], na.rm=TRUE),
            hrswork <- mean(UHRSWORK[status_mexico<5], na.rm=TRUE),
            
            iv_n_ijt=sum(status_mexico==1), #rest of the population
            iv_m_ijt=sum(status_mexico>1), #immigrant population)
            
            skr_iv_n_ijt=sum(status_mexico_unemp<5), #rest of the population
            skr_iv_m_ijt=sum(status_mexico_unemp==5)) -> data1_ind17  #immigrant population)

write.csv(data1_ind17, file="Ind17 original.csv")
data1_ind17 <- data1_ind17[!(data1_ind17$m_ijt==0),]
write.csv(data1_ind17, file="Ind17.csv")
remove(data1_ind17)

#schooling group wise data = 4 groups

data_schl1 <- data[schl==1]

data_schl1 %>% group_by(YEAR, birthtotal, exptotal, industry) %>%
  summarize(n_ijt=sum(status_mexico<5), #rest of the population
            m_ijt=sum(status_mexico==5), #daca eligible population
            yearwage <- mean(logrealinc[status_mexico<5], na.rm=TRUE),
            weekwage <- mean(logweekwage[status_mexico<5], na.rm=TRUE),
            wkwork <- mean(fracwrkd[status_mexico<5], na.rm=TRUE),
            hrswork <- mean(UHRSWORK[status_mexico<5], na.rm=TRUE),
            
            iv_n_ijt=sum(status_mexico==1), #rest of the population
            iv_m_ijt=sum(status_mexico>1), #immigrant population)
            
            skr_iv_n_ijt=sum(status_mexico_unemp<5), #rest of the population
            skr_iv_m_ijt=sum(status_mexico_unemp==5)) -> data1_schl1  #immigrant population)

write.csv(data1_schl1, file="Schl1 original.csv")
data1_schl1 <- data1_schl1[!(data1_schl1$m_ijt==0),]
write.csv(data1_schl1, file="Schl1.csv")
remove(data1_schl1)

data_schl2 <- data[schl==2]

data_schl2 %>% group_by(YEAR, birthtotal, exptotal, industry) %>%
  summarize(n_ijt=sum(status_mexico<5), #rest of the population
            m_ijt=sum(status_mexico==5), #daca eligible population
            yearwage <- mean(logrealinc[status_mexico<5], na.rm=TRUE),
            weekwage <- mean(logweekwage[status_mexico<5], na.rm=TRUE),
            wkwork <- mean(fracwrkd[status_mexico<5], na.rm=TRUE),
            hrswork <- mean(UHRSWORK[status_mexico<5], na.rm=TRUE),
            
            iv_n_ijt=sum(status_mexico==1), #rest of the population
            iv_m_ijt=sum(status_mexico>1), #immigrant population)
            
            skr_iv_n_ijt=sum(status_mexico_unemp<5), #rest of the population
            skr_iv_m_ijt=sum(status_mexico_unemp==5)) -> data1_schl2  #immigrant population)

write.csv(data1_schl2, file="Schl2 original.csv")
data1_schl2 <- data1_schl2[!(data1_schl2$m_ijt==0),]
write.csv(data1_schl2, file="Schl2.csv")
remove(data1_schl2)

data_schl3 <- data[schl==3]

data_schl3 %>% group_by(YEAR, birthtotal, exptotal, industry) %>%
  summarize(n_ijt=sum(status_mexico<5), #rest of the population
            m_ijt=sum(status_mexico==5), #daca eligible population
            yearwage <- mean(logrealinc[status_mexico<5], na.rm=TRUE),
            weekwage <- mean(logweekwage[status_mexico<5], na.rm=TRUE),
            wkwork <- mean(fracwrkd[status_mexico<5], na.rm=TRUE),
            hrswork <- mean(UHRSWORK[status_mexico<5], na.rm=TRUE),
            
            iv_n_ijt=sum(status_mexico==1), #rest of the population
            iv_m_ijt=sum(status_mexico>1), #immigrant population)
            
            skr_iv_n_ijt=sum(status_mexico_unemp<5), #rest of the population
            skr_iv_m_ijt=sum(status_mexico_unemp==5)) -> data1_schl3  #immigrant population)

write.csv(data1_schl3, file="Schl3 original.csv")
data1_schl3 <- data1_schl3[!(data1_schl3$m_ijt==0),]
write.csv(data1_schl3, file="Schl3.csv")
remove(data1_schl3)

data_schl4 <- data[schl==4]

data_schl4 %>% group_by(YEAR, birthtotal, exptotal, industry) %>%
  summarize(n_ijt=sum(status_mexico<5), #rest of the population
            m_ijt=sum(status_mexico==5), #daca eligible population
            yearwage <- mean(logrealinc[status_mexico<5], na.rm=TRUE),
            weekwage <- mean(logweekwage[status_mexico<5], na.rm=TRUE),
            wkwork <- mean(fracwrkd[status_mexico<5], na.rm=TRUE),
            hrswork <- mean(UHRSWORK[status_mexico<5], na.rm=TRUE),
            
            iv_n_ijt=sum(status_mexico==1), #rest of the population
            iv_m_ijt=sum(status_mexico>1), #immigrant population)
            
            skr_iv_n_ijt=sum(status_mexico_unemp<5), #rest of the population
            skr_iv_m_ijt=sum(status_mexico_unemp==5)) -> data1_schl4  #immigrant population)

write.csv(data1_schl4, file="Schl4 original.csv")
data1_schl4 <- data1_schl4[!(data1_schl4$m_ijt==0),]
write.csv(data1_schl4, file="Schl4.csv")
remove(data1_schl4)

#exp group = 5

data_exp1 <- data[exptotal==1]

data_exp1 %>% group_by(YEAR, birthtotal, schl, industry) %>%
  summarize(n_ijt=sum(status_mexico<5), #rest of the population
            m_ijt=sum(status_mexico==5), #daca eligible population
            yearwage <- mean(logrealinc[status_mexico<5], na.rm=TRUE),
            weekwage <- mean(logweekwage[status_mexico<5], na.rm=TRUE),
            wkwork <- mean(fracwrkd[status_mexico<5], na.rm=TRUE),
            hrswork <- mean(UHRSWORK[status_mexico<5], na.rm=TRUE),
            
            iv_n_ijt=sum(status_mexico==1), #rest of the population
            iv_m_ijt=sum(status_mexico>1), #immigrant population)
            
            skr_iv_n_ijt=sum(status_mexico_unemp<5), #rest of the population
            skr_iv_m_ijt=sum(status_mexico_unemp==5)) -> data1_exp1  #immigrant population)

write.csv(data1_exp1, file="Exp1 original.csv")
data1_exp1 <- data1_exp1[!(data1_exp1$m_ijt==0),]
write.csv(data1_exp1, file="Exp1.csv")
remove(data1_exp1)

data_exp2 <- data[exptotal==2]

data_exp2 %>% group_by(YEAR, birthtotal, schl, industry) %>%
  summarize(n_ijt=sum(status_mexico<5), #rest of the population
            m_ijt=sum(status_mexico==5), #daca eligible population
            yearwage <- mean(logrealinc[status_mexico<5], na.rm=TRUE),
            weekwage <- mean(logweekwage[status_mexico<5], na.rm=TRUE),
            wkwork <- mean(fracwrkd[status_mexico<5], na.rm=TRUE),
            hrswork <- mean(UHRSWORK[status_mexico<5], na.rm=TRUE),
            
            iv_n_ijt=sum(status_mexico==1), #rest of the population
            iv_m_ijt=sum(status_mexico>1), #immigrant population)
            
            skr_iv_n_ijt=sum(status_mexico_unemp<5), #rest of the population
            skr_iv_m_ijt=sum(status_mexico_unemp==5)) -> data1_exp2  #immigrant population)

write.csv(data1_exp2, file="Exp2 original.csv")
data1_exp2 <- data1_exp2[!(data1_exp2$m_ijt==0),]
write.csv(data1_exp2, file="Exp2.csv")
remove(data1_exp2)

data_exp3 <- data[exptotal==3]

data_exp3 %>% group_by(YEAR, birthtotal, schl, industry) %>%
  summarize(n_ijt=sum(status_mexico<5), #rest of the population
            m_ijt=sum(status_mexico==5), #daca eligible population
            yearwage <- mean(logrealinc[status_mexico<5], na.rm=TRUE),
            weekwage <- mean(logweekwage[status_mexico<5], na.rm=TRUE),
            wkwork <- mean(fracwrkd[status_mexico<5], na.rm=TRUE),
            hrswork <- mean(UHRSWORK[status_mexico<5], na.rm=TRUE),
            
            iv_n_ijt=sum(status_mexico==1), #rest of the population
            iv_m_ijt=sum(status_mexico>1), #immigrant population)
            
            skr_iv_n_ijt=sum(status_mexico_unemp<5), #rest of the population
            skr_iv_m_ijt=sum(status_mexico_unemp==5)) -> data1_exp3  #immigrant population)

write.csv(data1_exp3, file="Exp3 original.csv")
data1_exp3 <- data1_exp3[!(data1_exp3$m_ijt==0),]
write.csv(data1_exp3, file="Exp3.csv")
remove(data1_exp3)

data_exp4 <- data[exptotal==4]

data_exp4 %>% group_by(YEAR, birthtotal, schl, industry) %>%
  summarize(n_ijt=sum(status_mexico<5), #rest of the population
            m_ijt=sum(status_mexico==5), #daca eligible population
            yearwage <- mean(logrealinc[status_mexico<5], na.rm=TRUE),
            weekwage <- mean(logweekwage[status_mexico<5], na.rm=TRUE),
            wkwork <- mean(fracwrkd[status_mexico<5], na.rm=TRUE),
            hrswork <- mean(UHRSWORK[status_mexico<5], na.rm=TRUE),
            
            iv_n_ijt=sum(status_mexico==1), #rest of the population
            iv_m_ijt=sum(status_mexico>1), #immigrant population)
            
            skr_iv_n_ijt=sum(status_mexico_unemp<5), #rest of the population
            skr_iv_m_ijt=sum(status_mexico_unemp==5)) -> data1_exp4  #immigrant population)

write.csv(data1_exp4, file="Exp4 original.csv")
data1_exp4 <- data1_exp4[!(data1_exp4$m_ijt==0),]
write.csv(data1_exp4, file="Exp4.csv")
remove(data1_exp4)

data_exp5 <- data[exptotal==5]

data_exp5 %>% group_by(YEAR, birthtotal, schl, industry) %>%
  summarize(n_ijt=sum(status_mexico<5), #rest of the population
            m_ijt=sum(status_mexico==5), #daca eligible population
            yearwage <- mean(logrealinc[status_mexico<5], na.rm=TRUE),
            weekwage <- mean(logweekwage[status_mexico<5], na.rm=TRUE),
            wkwork <- mean(fracwrkd[status_mexico<5], na.rm=TRUE),
            hrswork <- mean(UHRSWORK[status_mexico<5], na.rm=TRUE),
            
            iv_n_ijt=sum(status_mexico==1), #rest of the population
            iv_m_ijt=sum(status_mexico>1), #immigrant population)
            
            skr_iv_n_ijt=sum(status_mexico_unemp<5), #rest of the population
            skr_iv_m_ijt=sum(status_mexico_unemp==5)) -> data1_exp5  #immigrant population)

write.csv(data1_exp5, file="Exp5 original.csv")
data1_exp5 <- data1_exp5[!(data1_exp5$m_ijt==0),]
write.csv(data1_exp5, file="Exp5.csv")
remove(data1_exp5)

#statewise data

#all 3 states #iv for ineligibles

data_all3 <- subset(data, STATEFIP==12 | STATEFIP==06 | STATEFIP==48)

data_all3 %>% group_by(YEAR, schl, birthtotal, exptotal, industry) %>%
  summarize(n_ijt=sum(status_mexico<5), #rest of the population
            m_ijt=sum(status_mexico==5), #daca eligible population
            yearwage <- mean(logrealinc[status_mexico<5], na.rm=TRUE),
            weekwage <- mean(logweekwage[status_mexico<5], na.rm=TRUE),
            wkwork <- mean(fracwrkd[status_mexico<5], na.rm=TRUE),
            hrswork <- mean(UHRSWORK[status_mexico<5], na.rm=TRUE),
            
            iv_n_ijt=sum(status_mexico==1), #rest of the population
            iv_m_ijt=sum(status_mexico>1), #immigrant population)
            
            skr_iv_n_ijt=sum(status_mexico<5 & EMPSTAT==2),
            skr_iv_m_ijt=sum(status_mexico==5 & EMPSTAT==2)) -> data_all31  

write.csv(data_all31, file="Original grouped data all3.csv")
data_all31 <- data_all31[!(data_all31$m_ijt==0),]
write.csv(data_all31, file="Original grouped data 2 all3.csv")

#skr iv

data$empstat <- fifelse(data$EMPSTAT==2, 1, 0)
data$status_mexico_unemp <- data$status_mexico*data$empstat

data_all3 %>% group_by(YEAR, schl, birthtotal, exptotal) %>%
  summarize(n_ijt=sum(status_mexico<5), #rest of the population
            m_ijt=sum(status_mexico==5), #daca eligible population
            yearwage <- mean(logrealinc[status_mexico<5], na.rm=TRUE),
            weekwage <- mean(logweekwage[status_mexico<5], na.rm=TRUE),
            wkwork <- mean(fracwrkd[status_mexico<5], na.rm=TRUE),
            hrswork <- mean(UHRSWORK[status_mexico<5], na.rm=TRUE),
            
            iv_n_ijt=sum(status_mexico_unemp<5), #rest of the population
            iv_m_ijt=sum(status_mexico_unemp==5)) -> data_all31_all  #immigrant population)

write.csv(data_all31_all, file="data_all all3 original.csv")
data_all31_all <- data_all31_all[!(data_all31_all$m_ijt==0),]
write.csv(data_all31_all, file="data_all all3.csv")


#plots

library(tidyverse)
library(ggplot2)
library(foreign)
library(haven)

data %>%
  group_by(YEAR,eligible) %>%
  summarize(logweekwage=mean(logweekwage)) -> sumdata

ggplot() + 
  geom_line(data=sumdata,aes(x=YEAR,y=logweekwage,group=treat, color=as.factor(treat)),
            size=2) + 
  geom_vline(xintercept = 2012)

#custom plots

se <- function(x) sqrt(var(x)/length(x))

data %>%
  group_by(YEAR,treat) %>%
  summarize(logweekwage_mean=mean(logweekwage),
            logweekwage_se=se(logweekwage)) -> grouped_data

false_vals <- grouped_data %>% filter(treat==0)
true_vals <- grouped_data %>% filter(treat==1)

diff <- data.frame(Year=unique(grouped_data$YEAR), mean_diff=false_vals$logweekwage_mean - true_vals$logweekwage_mean,
                   se=sqrt((false_vals$logweekwage_se)^2+(true_vals$logweekwage_se)^2),
                   ci=qnorm(0.95)*sqrt((false_vals$logweekwage_se)^2+(true_vals$logweekwage_se)^2))

ggplot(diff, aes(x = Year, y = mean_diff)) +
  geom_line(alpha = 1, linetype = "solid", size=0.75) +
  geom_errorbar(width=0.15, size=1, aes(ymin = mean_diff-ci, ymax = mean_diff+ci)) +
  geom_point(shape=21, size=2, fill="gray") +
  geom_rect(aes(xmin=2012, xmax=2013, ymin=0, ymax=Inf), alpha=0.05) +
  labs(x="Year", y="Difference between eligible and ineligible")

#new matching

library(dplyr)
library(zoo)

data <- read.csv("Original grouped data 2 all3.csv")

newnames <- c("X", "YEAR", "schl", "birthtotal", "exptotal", "industry", "n_ijt", "m_ijt", "yearwage", 
              "weekwage", "wkwork", "hrswork", "iv_n_ijt", "iv_m_ijt", "skr_n", "skr_m")
names(data) <- newnames
data$iv <- data$iv_m_ijt/(data$iv_m_ijt + data$iv_n_ijt)
data$logIV <- log(data$iv)
data$concat <- as.numeric(paste(data$schl, data$birthtotal, data$exptotal, data$industry, sep=""))

data %>% group_by(YEAR, concat) %>%
  summarize(iv = mean(logIV, na.rm=TRUE)) -> data1 

#data1$year <- data1$YEAR-2004
#data1$concat2 <- as.numeric(paste(data1$concat, data1$year, sep=""))
data1 <- arrange(data1, concat, YEAR)

data1$iv_new <- rollmean(data1$iv, 5, na.pad=TRUE, align="right")

#data1 <- data1 %>% mutate(moving_average = round(rollmeanr(iv, 3, fill = NA)))
#data1 <- data1 %>% mutate(moving_average = rollmeanr(iv, 3, fill = NA))

data1$year <- data1$YEAR-2004
data1$concat2 <- as.numeric(paste(data1$year, data1$concat, sep=""))

data <- read.csv("Original grouped data 2.csv")
newnames <- c("X", "YEAR", "schl", "birthtotal", "exptotal", "industry", "n_ijt", "m_ijt", "yearwage", 
              "weekwage", "wkwork", "hrswork", "iv_n_ijt", "iv_m_ijt", "skr_n", "skr_m")
names(data) <- newnames

data$year <- data$YEAR-2004
data$concat2 <- as.numeric(paste(data$year, data$schl, data$birthtotal, data$exptotal, data$industry, sep=""))

data2 <- merge(data, data1, by.x="concat2", by.y="concat2")
data2 <- subset(data2, select = -c(concat2, year.x, YEAR.y, concat, year.y))

data2$YEAR <- data2$YEAR.x
data <- subset(data2, YEAR >= 2009)
data <- subset(data, select = -c(YEAR.x))

rep <- median(data$weekwage)
data$weekwage <- ifelse(data$weekwage==0, rep, data$weekwage)

#regression tests

#shock
data$shock <- data$m_ijt/(data$m_ijt+data$n_ijt)
data$logshock <- log(data$shock)

#Weight (for weighted regression)

data$weight <- sum(data$n_ijt)/(data$n_ijt)

#IV (California)

#data$logIV <- log(data$iv_new)

#IV Regression -weekwage (cali)

ivmodel_1 <- ivreg(log(weekwage) ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) + factor(industry) | iv+ factor(exptotal)+ factor(YEAR) + factor(schl) +
                     factor(industry), data=data)


ivmodel_1_w <- ivreg(log(weekwage) ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) + factor(industry) | iv+ factor(exptotal)+ factor(YEAR) + factor(schl) +
                       factor(industry), weights=weight, data=data)

stargazer(ivmodel_1, type='text')

summary(ivmodel_1, diagnostics = TRUE)

summary(ivmodel_1_w, vcov=sandwich, diagnostics = TRUE)

summary(ivmodel_1_w, diagnostics = TRUE)

#regression for endogeneity

library(dplyr)
library(zoo)

data <- read.csv("Original grouped data 2 all3.csv")

newnames <- c("X", "YEAR", "schl", "birthtotal", "exptotal", "industry", "n_ijt", "m_ijt", "yearwage", 
              "weekwage", "wkwork", "hrswork", "iv_n_ijt", "iv_m_ijt", "skr_n", "skr_m")
names(data) <- newnames

data <- subset(data, YEAR <= 2011)

data$iv <- data$iv_m_ijt/(data$iv_m_ijt + data$iv_n_ijt)
data$logIV <- log(data$iv)
data$concat <- as.numeric(paste(data$schl, data$birthtotal, data$exptotal, data$industry, sep=""))

data %>% group_by(concat) %>%
  summarize(iv = mean(logIV, na.rm=TRUE)) -> data1 

data <- read.csv("Original grouped data 2 all3.csv")
newnames <- c("X", "YEAR", "schl", "birthtotal", "exptotal", "industry", "n_ijt", "m_ijt", "yearwage", 
              "weekwage", "wkwork", "hrswork", "iv_n_ijt", "iv_m_ijt", "skr_n", "skr_m")
names(data) <- newnames

data$concat <- as.numeric(paste(data$schl, data$birthtotal, data$exptotal, data$industry, sep=""))

data2 <- merge(data, data1, by.x="concat", by.y="concat")
data2 <- subset(data2, select = -c(concat))

data <- subset(data2, YEAR >= 2012)

rep <- median(data$weekwage)
data$weekwage <- ifelse(data$weekwage==0, rep, data$weekwage)

#regression for data sent by skr

data <- read.csv("Original grouped data 2.csv")

newnames <- c("X", "YEAR", "schl", "birthtotal", "exptotal", "industry", "n_ijt", "m_ijt", "yearwage", 
              "weekwage", "wkwork", "hrswork", "iv_n_ijt", "iv_m_ijt", "skr_n", "skr_m")
names(data) <- newnames

#shock
data$shock <- data$m_ijt/(data$m_ijt+data$n_ijt)
data$logshock <- log(data$shock)

#Weight (for weighted regression)

data$weight <- sum(data$n_ijt)/(data$n_ijt)

#IV (California)

data$IV <- data$iv_m_ijt_new/(data$iv_m_ijt_new+data$iv_n_ijt_new)
data$IV <- data$iv_m_ijt/(data$iv_m_ijt+data$iv_n_ijt)
data$logIV <- log(data$IV)

#IV Regression -weekwage (cali)

ivmodel_1 <- ivreg(weekwage ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) + factor(industry) | logIV+ factor(exptotal)+ factor(YEAR) + factor(schl) +
                     factor(industry), data=data)


ivmodel_1_w <- ivreg(weekwage ~ logshock + factor(exptotal)+ factor(YEAR) + factor(schl) + factor(industry) | logIV+ factor(exptotal)+ factor(YEAR) + factor(schl) +
                       factor(industry), weights=weight, data=data)

stargazer(ivmodel_1, type='text')

summary(ivmodel_1, diagnostics = TRUE)

summary(ivmodel_1_w, vcov=sandwich, diagnostics = TRUE)

summary(ivmodel_1_w, diagnostics = TRUE) 

#matching with old instructions

data1 <- read.csv("Original grouped data 2 texas_pre.csv")
data2 <- read.csv("Original grouped data 2 texas_post.csv")
data3 <- merge(data2, data1, by.x="concat", by.y="concat")

data <- read.csv("Original grouped data 2.csv")

