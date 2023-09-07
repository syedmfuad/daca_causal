memory.limit(size=56000)

install.packages("data.table")

library(data.table)

library(fixest)

data<- fread("E:/Shakil/Data/data_daca_var.csv")

data[,c("STATEFIP", "COUNTYFIP", "SPLOC", "AGE", "BPL", "BPLD", "CITIZEN", "YRIMMIG", "EDUC", "EDUCD", 
"EMPSTAT", "EMPSTATD", "CLASSWKR", "CLASSWKRD", "OCCSOC", "WKSWORK2", "INCWAGE", "birthyear", "entryage", "entry", 
"illegal_mexico", "Birth81", "Birth78_81", "Birth75_78", "Birth72_75", "Birth69_72", "Birth69", "legal", "status_fs_mexico", 
"status2_mexico", "status3_mexico", "citizen", "exptotal", "Exp_1", "Exp_2", "Exp_3", "Exp_4", "Exp_5", "Exp_6", "Schl_1", 
"Schl_2", "Schl_3", "Schl_4", "weekwage", "weekworked", "realincome", "expgroup", "state", "IND")] <- list(NULL)

# fixest logweekwage

est_logweekwage = feols(logweekwage ~ i(treat, YEAR) + exp+ SEX + yearsusa + Ind_01 + Ind_02 + Ind_03+ Ind_04 + Ind_05 + Ind_06+ Ind_07 + Ind_08 + Ind_09+ Ind_10  + Ind_11+ Ind_12 + Ind_13 + Ind_14 + Ind_15 + factor(schl)  + factor(birthtotal) + factor(YEAR), data= data) 

summary(est_logweekwage)

coefplot(est_logweekwage)

# fixest logrealinc

est_logrealinc = feols(logrealinc ~ i(treat, YEAR) + exp+ SEX + yearsusa + Ind_01 + Ind_02 + Ind_03+ Ind_04 + Ind_05 + Ind_06+ Ind_07 + Ind_08 + Ind_09+ Ind_10  + Ind_11+ Ind_12 + Ind_13 + Ind_14 + Ind_15 + factor(schl)  + factor(birthtotal) + factor(YEAR), data= data) 

summary(est_logrealinc)

coefplot(est_logrealinc)

# fixest UHRSWORK

summary(est_UHRSWORK)

coefplot(est_UHRSWORK)

# fixest fracwrkd

est_fracwrkd = feols(fracwrkd ~ i(treat, YEAR) + exp+ SEX + yearsusa + Ind_01 + Ind_02 + Ind_03+ Ind_04 + Ind_05 + Ind_06+ Ind_07 + Ind_08 + Ind_09+ Ind_10  + Ind_11+ Ind_12 + Ind_13 + Ind_14 + Ind_15 + factor(schl)  + factor(birthtotal) + factor(YEAR), data= data) 

summary(est_fracwrkd)

coefplot(est_fracwrkd)

data <- data[(data$EMPSTAT==1 | data$EMPSTAT==2),]
data$yearsusa <- fifelse(data$status_mexico==1, data$AGE, data$AGE-data$entryage)
data$SEX <- fifelse(data$SEX==2, 1, 0)

data$treat <- ifelse(data$status_mexico==5, 1, 0)
data$post <- ifelse(data$YEAR>2012, 1, 0)
data$treat_post <- data$treat*data$post

colnames(data)

install.packages("speedglm")

library(speedglm)

#overall (logweekwage)

reg1_nocovariates <- lm(logweekwage ~ treat_post + treat + post, data=data)

reg1 <- lm(logweekwage ~ treat_post + treat + post + exp+ SEX + yearsusa+ Ind_01 + Ind_02 + Ind_03+ Ind_04 + Ind_05 + Ind_06+ Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11+ Ind_12 + Ind_13 + Ind_14 + Ind_15 + Ind_16 + factor(schl)  + factor(birthtotal) + factor(YEAR), data=data)

summary(reg1)

data$Year2009 <- ifelse(data$YEAR==2009, 1, 0)
data$Year2010 <- ifelse(data$YEAR==2010, 1, 0)
data$Year2011 <- ifelse(data$YEAR==2011, 1, 0)

data$treat_2009 <- data$treat*data$Year2009
data$treat_2010 <- data$treat*data$Year2010
data$treat_2011 <- data$treat*data$Year2011

reg_p <- lm(logweekwage ~ treat_post +treat_2010+treat_2011 + treat + post + exp+ SEX + yearsusa+ Ind_01 + Ind_02 + Ind_03+ Ind_04 + Ind_05 + Ind_06+ Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11+ Ind_12 + Ind_13 + Ind_14 + Ind_15 +  factor(schl)  + factor(birthtotal) + factor(YEAR), data=data)

summary(reg_p)

library(ggplot2)

library(tidyverse)

data %>% 
  group_by(YEAR,treat) %>% 
  summarize(logrealinc=mean(logrealinc)) -> sumdata

ggplot() + #geom_line(data=data,aes(x=YEAR,y=logweekwage,group= eligible, color=as.factor(eligible)),
  #size=1,alpha=0.25)+  # plot the individual lines
  geom_line(data=sumdata,aes(x=YEAR,y=logrealinc,group=treat, color=as.factor(treat)),
            size=2) + # plot the averages for each group
  geom_vline(xintercept = 2012)+  # intervention point
  
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=90, hjust=1))+
  scale_x_continuous(breaks=seq(from = 2005, to = 2018)) +
  scale_color_manual(values=c("red","blue"), # label our groups
                     labels=c("Control Average","Treatment Average"))

#mean difference plot

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
  labs(x="Year", y="LogWeekWage Difference(ineligible - eligible)")

#overall (logrealinc)

reg2_nocovariates <- lm(logrealinc ~ treat_post + treat + post, data=data)

reg2 <- lm(logrealinc ~ treat_post + treat + post + exp+ SEX + yearsusa+ Ind_01 + Ind_02 + Ind_03+ Ind_04 + Ind_05 + Ind_06+ Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11+ Ind_12 + Ind_13 + Ind_14 + Ind_15 + Ind_16 + factor(schl)  + factor(birthtotal) + factor(YEAR), data=data)

summary(reg2_nocovariates)
summary(reg2)

reg_logrealinc_p <- lm(logrealinc ~ treat_post  +treat_2010+treat_2011 + treat + post + exp+ SEX + yearsusa+ Ind_01 + Ind_02 + Ind_03+ Ind_04 + Ind_05 + Ind_06+ Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11+ Ind_12 + Ind_13 + Ind_14 + Ind_15  + factor(schl)  + factor(birthtotal) + factor(YEAR), data=data)

summary(reg_logrealinc_p)

data_states %>% 
  group_by(YEAR,treat) %>% 
  summarize(logweekwage=mean(logweekwage)) -> sumdata

ggplot() + #geom_line(data=data,aes(x=YEAR,y=logweekwage,group= eligible, color=as.factor(eligible)),
  #size=1,alpha=0.25)+  # plot the individual lines
  geom_line(data=sumdata,aes(x=YEAR,y=logweekwage,group=treat, color=as.factor(treat)),
            size=2) + # plot the averages for each group
  geom_vline(xintercept = 2012)+  # intervention point
  
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=90, hjust=1))+
  scale_x_continuous(breaks=seq(from = 2005, to = 2018)) +
  scale_color_manual(values=c("red","blue"), # label our groups
                     labels=c("Control Average","Treatment Average"))

#mean difference

se <- function(x) sqrt(var(x)/length(x))

data %>%
  group_by(YEAR,treat) %>%
  summarize(logrealinc_mean=mean(logrealinc),
            logrealinc_se=se(logrealinc)) -> grouped_data

false_vals <- grouped_data %>% filter(treat==0)
true_vals <- grouped_data %>% filter(treat==1)

diff <- data.frame(Year=unique(grouped_data$YEAR), mean_diff=false_vals$logrealinc_mean - true_vals$logrealinc_mean,
                   se=sqrt((false_vals$logrealinc_se)^2+(true_vals$logrealinc_se)^2),
                   ci=qnorm(0.95)*sqrt((false_vals$logrealinc_se)^2+(true_vals$logrealinc_se)^2))

ggplot(diff, aes(x = Year, y = mean_diff)) +
  geom_line(alpha = 1, linetype = "solid", size=0.75) +
  geom_errorbar(width=0.15, size=1, aes(ymin = mean_diff-ci, ymax = mean_diff+ci)) +
  geom_point(shape=21, size=2, fill="gray") +
  geom_rect(aes(xmin=2012, xmax=2013, ymin=0, ymax=Inf), alpha=0.05) +
  labs(x="Year", y="LogRealinc Difference(ineligible - eligible)")

#overall (UHRSWORK)

reg3_nocovar <- lm(UHRSWORK ~ treat_post + treat + post , data=data)

summary(reg3_nocovar)

reg3 <- lm(UHRSWORK  ~ treat_post + treat + post + exp+ SEX + yearsusa+ Ind_01 + Ind_02 + Ind_03+ Ind_04 + Ind_05 + Ind_06+ Ind_07 + Ind_08 + Ind_09 + Ind_11+ Ind_12 + Ind_13 + Ind_14 + Ind_15  + factor(schl)  + factor(birthtotal) + factor(YEAR), data=data)

summary(reg3)

reg3_P <- lm(UHRSWORK  ~ treat_post  +treat_2010+treat_2011 + treat + post + exp+ SEX + yearsusa+ Ind_01 + Ind_02 + Ind_03+ Ind_04 + Ind_05 + Ind_06+ Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11+ Ind_12 + Ind_13 + Ind_14 + Ind_15 + Ind_16 + factor(schl)  + factor(birthtotal) + factor(YEAR), data=data)

summary(reg3_P)

#overall (fracwrkd)

reg4_nocovar <- lm(fracwrkd ~ treat_post + treat + post , data=data)

summary (reg4_nocovar)

reg4 <- lm(fracwrkd ~ treat_post + treat + post + exp+ SEX + yearsusa + Ind_01 + Ind_02 + Ind_03+ Ind_04 + Ind_05 + Ind_06+ Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11+ Ind_12 + Ind_13 + Ind_14 + Ind_15 + Ind_16 + factor(schl)  + factor(birthtotal) + factor(YEAR), data=data)

summary(reg4)

reg4_P <- lm(fracwrkd ~ treat_post  +treat_2010+treat_2011 + treat + post + exp+ SEX + yearsusa+ Ind_01 + Ind_02 + Ind_03+ Ind_04 + Ind_05 + Ind_06+ Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11+ Ind_12 + Ind_13 + Ind_14 + Ind_15 + Ind_16 + factor(schl)  + factor(birthtotal) + factor(YEAR), data=data)

summary(reg4_P)

summary(reg_p_fracwrkd)

library(tidyverse)
library(ggplot2)
library(foreign)
library(haven)

data %>% 
  group_by(YEAR,treat) %>% 
  summarize(fracwrkd=mean(fracwrkd)) -> sumdata

ggplot() + #geom_line(data=data,aes(x=YEAR,y=logweekwage,group= eligible, color=as.factor(eligible)),
  #size=1,alpha=0.25)+  # plot the individual lines
  geom_line(data=sumdata,aes(x=YEAR,y=fracwrkd,group=treat, color=as.factor(treat)),
            size=2) + # plot the averages for each group
  geom_vline(xintercept = 2012)+  # intervention point
  
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=90, hjust=1))+
  scale_x_continuous(breaks=seq(from = 2005, to = 2018)) +
  scale_color_manual(values=c("red","blue"), # label our groups
                     labels=c("Control Average","Treatment Average"))

# mean difference

data %>%
  group_by(YEAR,treat) %>%
  summarize(fracwrkd_mean=mean(fracwrkd),
            fracwrkd_se=se(fracwrkd)) -> grouped_data

false_vals <- grouped_data %>% filter(treat==0)
true_vals <- grouped_data %>% filter(treat==1)

diff <- data.frame(Year=unique(grouped_data$YEAR), mean_diff=false_vals$fracwrkd_mean - true_vals$fracwrkd_mean,
                   se=sqrt((false_vals$fracwrkd_se)^2+(true_vals$fracwrkd_se)^2),
                   ci=qnorm(0.95)*sqrt((false_vals$fracwrkd_se)^2+(true_vals$fracwrkd_se)^2))

ggplot(diff, aes(x = Year, y = mean_diff)) +
  geom_line(alpha = 1, linetype = "solid", size=0.75) +
  geom_errorbar(width=0.15, size=1, aes(ymin = mean_diff-ci, ymax = mean_diff+ci)) +
  geom_point(shape=21, size=2, fill="gray") +
  geom_rect(aes(xmin=2012, xmax=2013, ymin=0, ymax=Inf), alpha=0.05) +
  labs(x="Year", y="FracWorked Difference(ineligible - eligible)")

# texas (only for event-study graph)

data_texas <- fread("E:/R(analysis)_Other Laptop/data_texas_vars.csv")

data_texas$Year2006 <- ifelse(data_texas$YEAR==2006, 1, 0) #this one gives 1 if year=2008 and 0 otherwise
data_texas$Year2007 <- ifelse(data_texas$YEAR==2007, 1, 0)
data_texas$Year2008 <- ifelse(data_texas$YEAR==2008, 1, 0) #this one gives 1 if year=2008 and 0 otherwise
data_texas$Year2009 <- ifelse(data_texas$YEAR==2009, 1, 0)
data_texas$Year2010 <- ifelse(data_texas$YEAR==2010, 1, 0)
data_texas$Year2011 <- ifelse(data_texas$YEAR==2011, 1, 0)
data_texas$Year2012 <- ifelse(data_texas$YEAR==2012, 1, 0)
data_texas$Year2013 <- ifelse(data_texas$YEAR==2013, 1, 0)
data_texas$Year2014 <- ifelse(data_texas$YEAR==2014, 1, 0)
data_texas$Year2015 <- ifelse(data_texas$YEAR==2015, 1, 0)
data_texas$Year2016 <- ifelse(data_texas$YEAR==2016, 1, 0)
data_texas$Year2017 <- ifelse(data_texas$YEAR==2017, 1, 0)
data_texas$Year2018 <- ifelse(data_texas$YEAR==2018, 1, 0)

library(fixest)

# fixest logweekwage

est_logweekwage = feols(logweekwage~  i(treat, YEAR)+treat + exp+ SEX + yearsusa + Ind_01 + Ind_02 + Ind_03+ Ind_04 + Ind_05 + Ind_06+ Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11+ Ind_12 + Ind_13 + Ind_14 + Ind_15 + Schl_2 + Schl_3 + Schl_4 + Birth81 + Birth78_81 + Birth75_78 + Birth72_75 + Birth69_72 +  Year2006 + Year2007+Year2008 + Year2009 + Year2010 +Year2011+ Year2012 + Year2013 + Year2014 + Year2015 + Year2016 + Year2017 +
                          Year2018,  data=data_texas) 

summary(est_logweekwage)

coefplot(est_logweekwage)

# fixest logrealinc

est_logrealinc = feols(logrealinc~  i(treat, YEAR) +treat+ exp+ SEX + yearsusa + Ind_01 + Ind_02 + Ind_03+ Ind_04 + Ind_05 + Ind_06+ Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11+ Ind_12 + Ind_13 + Ind_14 + Ind_15 + Schl_2 + Schl_3 + Schl_4 + Birth81 + Birth78_81 + Birth75_78 + Birth72_75 + Birth69_72 +  Year2006 + Year2007+Year2008 + Year2009 + Year2010 +Year2011+ Year2012 + Year2013 + Year2014 + Year2015 + Year2016 + Year2017 +
                         Year2018, data=data_texas) 

summary(est_logrealinc)

coefplot(est_logrealinc)

# fixest UHRSWORK

est_UHRSWORK = feols(UHRSWORK~  i(treat, YEAR) + exp+ SEX + yearsusa + Ind_01 + Ind_02 + Ind_03+ Ind_04 + Ind_05 + Ind_06+ Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11+ Ind_12 + Ind_13 + Ind_14 + Ind_15 + factor(schl)  + factor(birthtotal)+ Year2006 + Year2007+Year2008 + Year2009 + Year2010 +Year2011+ Year2012 + Year2013 + Year2014 + Year2015 + Year2016 + Year2017 +
                       Year2018, data=data_texas) 

summary(est_UHRSWORK)

coefplot(est_UHRSWORK)

# fixest fracwrkd

est_fracwrkd = feols(fracwrkd~  i(treat, YEAR) + exp+ SEX + yearsusa + Ind_01 + Ind_02 + Ind_03+ Ind_04 + Ind_05 + Ind_06+ Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11+ Ind_12 + Ind_13 + Ind_14 + Ind_15 + factor(schl)  + factor(birthtotal) + factor(YEAR), data=data_texas) 

summary(est_fracwrkd)

coefplot(est_fracwrkd)

data_texas[,c("STATEFIP", "COUNTYFIP", "SPLOC", "AGE", "BPL", "BPLD", "CITIZEN", "YRIMMIG", "EDUC", "EDUCD", "EMPSTAT", 
"EMPSTATD", "CLASSWKR", "CLASSWKRD", "OCCSOC", "WKSWORK2", "INCWAGE", "birthyear", "entryage", "entry", "illegal_mexico", 
"Birth81", "Birth78_81", "Birth75_78", "Birth72_75", "Birth69_72", "Birth69", "legal", "status_fs_mexico", "status2_mexico", 
"status3_mexico", "citizen", "exptotal", "Exp_1", "Exp_2", "Exp_3", "Exp_4", "Exp_5", "Exp_6", "Schl_1", "Schl_2", "Schl_3", 
"Schl_4", "weekwage", "weekworked", "realincome", "expgroup", "state", "IND")] <- list(NULL)

# florida (only for event-study graph)

data_fl <- fread("E:/Shakil/R(analysis)/data_florida_vars.csv")

# fixest logweekwage

est_logweekwage = feols(logweekwage~  i(treat, YEAR) + exp+ SEX + yearsusa + Ind_01 + Ind_02 + Ind_03+ Ind_04 + Ind_05 + Ind_06+ Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11+ Ind_12 + Ind_13 + Ind_14 + Ind_15 + factor(schl)  + factor(birthtotal) + factor(YEAR), data=data_fl) 

summary(est_logweekwage)

coefplot(est_logweekwage)

# fixest logrealinc

est_logrealinc = feols(logrealinc~  i(treat, YEAR) + exp+ SEX + yearsusa + Ind_01 + Ind_02 + Ind_03+ Ind_04 + Ind_05 + Ind_06+ Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11+ Ind_12 + Ind_13 + Ind_14 + Ind_15 + factor(schl)  + factor(birthtotal) + factor(YEAR), data=data_fl) 

summary(est_logrealinc)

coefplot(est_logrealinc)

# fixest UHRSWORK

est_UHRSWORK = feols(UHRSWORK~  i(treat, YEAR) + exp+ SEX + yearsusa + Ind_01 + Ind_02 + Ind_03+ Ind_04 + Ind_05 + Ind_06+ Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11+ Ind_12 + Ind_13 + Ind_14 + Ind_15 + factor(schl)  + factor(birthtotal) + factor(YEAR), data=data_fl) 

summary(est_UHRSWORK)

coefplot(est_UHRSWORK)

# fixest fracwrkd

est_fracwrkd = feols(fracwrkd~  i(treat, YEAR) + exp+ SEX + yearsusa + Ind_01 + Ind_02 + Ind_03+ Ind_04 + Ind_05 + Ind_06+ Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11+ Ind_12 + Ind_13 + Ind_14 + Ind_15 + factor(schl)  + factor(birthtotal) + factor(YEAR), data=data_fl) 

summary(est_fracwrkd)

coefplot(est_fracwrkd)

# Cali (only for event-study graph)

data_cali <- fread("E:/Shakil/R(analysis)/data_cali_vars.csv")

# fixest logweekwage

est_logweekwage = feols(logweekwage~  i(treat, YEAR) + exp+ SEX + yearsusa + Ind_01 + Ind_02 + Ind_03+ Ind_04 + Ind_05 + Ind_06+ Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11+ Ind_12 + Ind_13 + Ind_14 + Ind_15 + factor(schl)  + factor(birthtotal) + factor(YEAR), data=data_cali) 

summary(est_logweekwage)

coefplot(est_logweekwage)

# fixest logrealinc

est_logrealinc = feols(logrealinc~  i(treat, YEAR) + exp+ SEX + yearsusa + Ind_01 + Ind_02 + Ind_03+ Ind_04 + Ind_05 + Ind_06+ Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11+ Ind_12 + Ind_13 + Ind_14 + Ind_15 + factor(schl)  + factor(birthtotal) + factor(YEAR), data=data_cali) 

summary(est_logrealinc)

coefplot(est_logrealinc)

# fixest UHRSWORK

est_UHRSWORK = feols(UHRSWORK~  i(treat, YEAR) + exp+ SEX + yearsusa + Ind_01 + Ind_02 + Ind_03+ Ind_04 + Ind_05 + Ind_06+ Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11+ Ind_12 + Ind_13 + Ind_14 + Ind_15 + factor(schl)  + factor(birthtotal) + factor(YEAR), data=data_cali) 

summary(est_UHRSWORK)

coefplot(est_UHRSWORK)

# fixest fracwrkd

est_fracwrkd = feols(fracwrkd~  i(treat, YEAR) + exp+ SEX + yearsusa + Ind_01 + Ind_02 + Ind_03+ Ind_04 + Ind_05 + Ind_06+ Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11+ Ind_12 + Ind_13 + Ind_14 + Ind_15 + factor(schl)  + factor(birthtotal) + factor(YEAR), data=data_cali) 

summary(est_fracwrkd)

coefplot(est_fracwrkd)

# 3 states (only for event-study graph)

data_states <- fread("E:/Shakil/R(analysis)/data_states_vars.csv")

# fixest logweekwage

est_logweekwage = feols(logweekwage~  i(treat, YEAR) + exp+ SEX + yearsusa + Ind_01 + Ind_02 + Ind_03+ Ind_04 + Ind_05 + Ind_06+ Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11+ Ind_12 + Ind_13 + Ind_14 + Ind_15 + factor(schl)  + factor(birthtotal) + factor(YEAR), data=data_states) 

summary(est_logweekwage)

coefplot(est_logweekwage)

# fixest logrealinc

est_logrealinc = feols(logrealinc~  i(treat, YEAR) + exp+ SEX + yearsusa + Ind_01 + Ind_02 + Ind_03+ Ind_04 + Ind_05 + Ind_06+ Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11+ Ind_12 + Ind_13 + Ind_14 + Ind_15 + factor(schl)  + factor(birthtotal) + factor(YEAR), data=data_states) 

summary(est_logrealinc)

coefplot(est_logrealinc)

# fixest UHRSWORK

est_UHRSWORK = feols(UHRSWORK~  i(treat, YEAR) + exp+ SEX + yearsusa + Ind_01 + Ind_02 + Ind_03+ Ind_04 + Ind_05 + Ind_06+ Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11+ Ind_12 + Ind_13 + Ind_14 + Ind_15 + factor(schl)  + factor(birthtotal) + factor(YEAR), data=data_states) 

summary(est_UHRSWORK)

coefplot(est_UHRSWORK)

# fixest fracwrkd

est_fracwrkd = feols(fracwrkd~  i(treat, YEAR) + exp+ SEX + yearsusa + Ind_01 + Ind_02 + Ind_03+ Ind_04 + Ind_05 + Ind_06+ Ind_07 + Ind_08 + Ind_09 + Ind_10 + Ind_11+ Ind_12 + Ind_13 + Ind_14 + Ind_15 + factor(schl)  + factor(birthtotal) + factor(YEAR), data=data_states) 

summary(est_fracwrkd)

coefplot(est_fracwrkd)

# 3 states

data_states <- subset(data, STATEFIP==48 | STATEFIP==06 | STATEFIP==12)

# logweekwage (states)
reg_states1 <- lm(weekwage ~ treat_post + treat + post + factor(industry) + factor(schl) + Exp_2 + Exp_3 + Exp_4 + Exp_5 + Exp_6 + factor(birthtotal) + factor(YEAR), data=data_states)

summary(reg_states1)

reg_p <- lm(logweekwage ~ treat_post +treat_2009 +treat_2010+treat_2011 + treat + post + factor(industry) + factor(schl) + Exp_2 + Exp_3 + Exp_4 + Exp_5 + Exp_6 + factor(birthtotal) + factor(YEAR), data=data_states)

summary(reg_p)

data_states %>% 
  group_by(YEAR,treat) %>% 
  summarize(logweekwage=mean(logweekwage)) -> sumdata

ggplot() + #geom_line(data=data,aes(x=YEAR,y=logweekwage,group= eligible, color=as.factor(eligible)),
  #size=1,alpha=0.25)+  # plot the individual lines
  geom_line(data=sumdata,aes(x=YEAR,y=logweekwage,group=treat, color=as.factor(treat)),
            size=2) + # plot the averages for each group
  geom_vline(xintercept = 2012)+  # intervention point
  
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=90, hjust=1))+
  scale_x_continuous(breaks=seq(from = 2005, to = 2018)) +
  scale_color_manual(values=c("red","blue"), # label our groups
                     labels=c("Control Average","Treatment Average"))

# logrealinc (states)

reg_states2 <- lm(logrealinc ~ treat_post + treat + post + factor(industry) + factor(schl) + Exp_2 + Exp_3 + Exp_4 + Exp_5 + Exp_6 + factor(birthtotal) + factor(YEAR), data=data_states)

summary(reg_states2)

reg_p_logrealinc <- lm(logrealinc ~ treat_post +treat_2009 +treat_2010+treat_2011 + treat + post + factor(industry) + factor(schl) + Exp_2 + Exp_3 + Exp_4 + Exp_5 + Exp_6 + factor(birthtotal) + factor(YEAR), data=data_states)

summary(reg_p_logrealinc)

data_states %>% 
  group_by(YEAR,treat) %>% 
  summarize(logrealinc=mean(logrealinc)) -> sumdata

ggplot() + #geom_line(data=data,aes(x=YEAR,y=logweekwage,group= eligible, color=as.factor(eligible)),
  #size=1,alpha=0.25)+  # plot the individual lines
  geom_line(data=sumdata,aes(x=YEAR,y=logrealinc,group=treat, color=as.factor(treat)),
            size=2) + # plot the averages for each group
  geom_vline(xintercept = 2012)+  # intervention point
  
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=90, hjust=1))+
  scale_x_continuous(breaks=seq(from = 2005, to = 2018)) +
  scale_color_manual(values=c("red","blue"), # label our groups
                     labels=c("Control Average","Treatment Average"))

# fracwrkd (states)

reg_states3 <- lm(fracwrkd ~ treat_post + treat + post + factor(industry) + factor(schl) + Exp_2 + Exp_3 + Exp_4 + Exp_5 + Exp_6 + factor(birthtotal) + factor(YEAR), data=data_states)

summary(reg_states3)

reg_p_fracwrkd <- lm(fracwrkd ~ treat_post +treat_2009 +treat_2010+treat_2011 + treat + post + factor(industry) + factor(schl) + Exp_2 + Exp_3 + Exp_4 + Exp_5 + Exp_6 + factor(birthtotal) + factor(YEAR), data=data_states)

summary(reg_p_fracwrkd)

data_states %>% 
  group_by(YEAR,treat) %>% 
  summarize(fracwrkd=mean(fracwrkd)) -> sumdata

ggplot() + #geom_line(data=data,aes(x=YEAR,y=logweekwage,group= eligible, color=as.factor(eligible)),
  #size=1,alpha=0.25)+  # plot the individual lines
  geom_line(data=sumdata,aes(x=YEAR,y=fracwrkd,group=treat, color=as.factor(treat)),
            size=2) + # plot the averages for each group
  geom_vline(xintercept = 2012)+  # intervention point
  
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=90, hjust=1))+
  scale_x_continuous(breaks=seq(from = 2005, to = 2018)) +
  scale_color_manual(values=c("red","blue"), # label our groups
                     labels=c("Control Average","Treatment Average"))

library(gplots)
plotmeans(fracwrkd ~ YEAR, data = sumdata, frame = FALSE)
geom_vline(YEAR = 2012)
