library(dplyr)
library(stringr)
library(ggplot2)

df1 <- read.csv("data2.csv",as.is = c(1:5),na.strings = '')


df2 <- df1[-1]
glimpse(df2)
summary(df2)
head(df2)
apply(df2,2,table)

vec1 <- c("age","gender","location","profession","income_per_month","covid_affected","vaccinated_no_of_doses",
          "lockdown_effect_finance","lockdown_effect_physically","lockdown_effect_mentally",
          "comorbidity","exercise_freq","view_on_exercise","traveling_freq_work",
          "traveling_freq_emergency","traveling_freq_vacation","covid_precautions",
          "covid_effect_on_work","profession_change","focus_on_physical_health",
          "focus_on_mental_health","focus_on_finance")
ncol(df2)
colnames(df2) <- vec1
glimpse(df2)
df2$age <- as.integer(df2$age)
table(df2$age)


df3 <- df2%>%
  mutate_at(c(3,4),tolower)
glimpse(df3)
mapply(levels,df3)



levels(df3$vaccinated_no_of_doses) <- c(0,2,1)

table(df3$location)
table(df3$profession)
glimpse(df3)
table(df3$covid_affected)
df3$covid_affected1 = df3$covid_affected
levels(df3$covid_affected1) <- c(0,0,NA,1)
df3$covid_affected1


df3$profession1 <- df3$profession

table(df3$profession1)


summary(df3$age)
df3$profession1[which(str_detect(df3$profession1,c("student|nursing|studying")))] = "student"
df3$profession1[which(str_detect(df3$profession1,c("home|house|retired|retd|graduate|no")))] = "unemployed"
df3$profession1[which(str_detect(df3$profession1,c("student|unemployed"),negate = T))] = "employed" 
table(df3$profession1)

df3 <- df3[complete.cases(df3$age),]

age_gr1 <- 17:25
age_gr2 <- 26:35
age_gr3 <- 36:45
age_gr4 <- 46:55
age_gr5 <- 56:65

group_div <- function(x){
  if(x %in% age_gr1){
    paste0("17-25")
  }else if(x %in% age_gr2){
    paste0("26-35")
  }else if(x %in% age_gr3){
    paste0("36-45")
  }else if(x %in% age_gr4){
    paste0("46-55")
  }else if(is.na(x)){
    print(NA)
  }else{
    "56-65"
  }    
}
df3$age_grp <- sapply(df3$age,group_div)





##################Q1
#age-wise effect of lockdown financially

age_count <- df3 %>% 
  group_by(age_grp) %>% 
  summarise(count= n())

age_count

df_age <- df3 %>% 
  group_by(age_grp,lockdown_effect_finance) %>% 
  summarise(count = n())
df_age

tab <- data.frame(table(df_age$age_grp))
tab

df_age$total <- rep(age_count$count,times=tab$Freq)
df_age$freqden <- df_age$count/df_age$total
names(df3)
df3$lockdown_effect_finance


ggplot(data=df_age ,aes(x= age_grp,y= freqden ,fill= lockdown_effect_finance ))+
  geom_bar(position="dodge",stat="identity",width =0.5)+coord_polar()
labs(y="Count",x="Age group",title="Effect of Lockdown Financially",
     fill= "Effect of lockdown Financially", subtitle = "Age wise",fill="Frequency density")


#####################################
#Income group wise effect of lock down financially

df_fin1 <- subset(df3,profession1 != "student",select = c("income_per_month","lockdown_effect_finance"))

igrp_count <- df_fin1 %>% 
  group_by(income_per_month) %>% 
  summarise(count= n())


df_fin <- df_fin1 %>% 
  group_by(income_per_month,lockdown_effect_finance) %>% 
  summarise(count = n())

tab2 <- data.frame(table(df_fin$income_per_month))


df_fin$total <- rep(igrp_count$count,times=tab2$Freq)
df_fin$freqden <- df_fin$count/df_fin$total

ggplot(data=df_fin ,aes(x= income_per_month,y= freqden ,fill= lockdown_effect_finance ))+
  geom_bar(position="dodge",stat="identity",width =0.5)+
  labs(y="count",x="Income Group",title="Effect of Lockdown Financially",
       fill= "Effect of lockdown Financially", subtitle = "income group wise",fill="Frequency density")


#####################################################
#Change of profession in every income group


df_fin2 <- subset(df3,profession1 != "student",select = c("income_per_month","profession_change"))

igrp_count <- df_fin2 %>% 
  group_by(income_per_month) %>% 
  summarise(count= n())


df_fina <- df_fin2 %>% 
  group_by(income_per_month,profession_change) %>% 
  summarise(count = n())

tab2 <- data.frame(table(df_fina$income_per_month))


df_fina$total <- rep(igrp_count$count,times=tab2$Freq)
df_fina$freqden <- df_fina$count/df_fina$total

ggplot(data=df_fina ,aes(x= income_per_month,y= freqden ,fill= profession_change ))+
  geom_bar(position="dodge",stat="identity",width =0.5)+
  labs(y="count",x="Income Group",title="change of profession",
       fill= "Change of Profession", subtitle = "income group wise",fill="Frequency density")
          
          
          
  
##############################################################
#change in work life in different age groups


df_fin3 <- subset(df3,profession1 != "student",select = c("income_per_month","covid_effect_on_work"))

igrp_count <- df_fin3 %>% 
  group_by(income_per_month) %>% 
  summarise(count= n())


df_finan <- df_fin3 %>% 
  group_by(income_per_month,covid_effect_on_work) %>% 
  summarise(count = n())

tab2 <- data.frame(table(df_finan$income_per_month))

df_finan$total <- rep(igrp_count$count,times=tab2$Freq)
df_finan$freqden <- df_finan$count/df_finan$total

ggplot(data=df_finan ,aes(x= income_per_month,y= freqden ,fill= as.factor(covid_effect_on_work) ))+
  geom_bar(position="dodge",stat="identity",width =0.5)+
  labs(y="count",x="Income Group",title="change in worklife",
       fill= "Change in worklife\n(not affected to very affected)", subtitle = "income group wise",fill="Frequency density")

###########################################################
#focus of people in different income groups after lockdown


df_fin4 <- subset(df3,profession1 != "student",select = c("income_per_month","focus_on_finance"))

igrp_count <- df_fin4 %>% 
  group_by(income_per_month) %>% 
  summarise(count= n())

df_fin_focus <- df_fin4 %>% 
  group_by(income_per_month,focus_on_finance) %>% 
  summarise(count = n())

tab2 <- data.frame(table(df_fin_focus$income_per_month))


df_fin_focus$total <- rep(igrp_count$count,times=tab2$Freq)
df_fin_focus$freqden <- df_fin_focus$count/df_fin_focus$total

ggplot(data=df_fin_focus ,aes(x= income_per_month,y= freqden ,fill= focus_on_finance ))+
  geom_bar(position="dodge",stat="identity",width =0.5)+
  labs(y="count",x="Income Group",title="Focus on Finance after lockdown",
       fill= "Focus on Finance", subtitle = "income group wise",fill="Frequency density")


          
          
          
          
