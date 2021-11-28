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
     fill= "Effect of lockdown Financially", subtitle = "Age wise")


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
       fill= "Effect of lockdown Financially", subtitle = "income group wise")


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
       fill= "Change of Profession", subtitle = "income group wise")
          
          
          
  
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
       fill= "Change in worklife\n(not affected to very affected)", subtitle = "income group wise")

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
       fill= "Focus on Finance", subtitle = "income group wise")


############################################################
#people who has co morbidity how their physical health is affected

                

df_covid <- subset(df3,covid_affected1 == 1 , select = c(comorbidity,lockdown_effect_physically))
df_covid <- df_covid[complete.cases(df_covid),]


comorb_count <- df_covid %>% 
  group_by(comorbidity) %>% 
  summarise(count= n())

df_covid1 <- df_covid %>% 
  group_by(comorbidity,lockdown_effect_physically) %>% 
  summarise(count = n())

tab2 <- data.frame(table(df_covid1$comorbidity))

df_covid1$total <- rep(comorb_count$count,times = c(4,2))
df_covid1$freqden <- df_covid1$count/df_covid1$total

ggplot(data=df_covid1 ,aes(x= comorbidity,y= freqden ,fill= lockdown_effect_physically ))+
  geom_bar(position="dodge",stat="identity",width =0.5)+
  labs(y="count",x="Comorbidity",title="Effect of Covid on Physical Health",
       fill= "Effect of Covid on Physical Health", subtitle = "Covid Affected population")          
          

##################################################################
#people infected with covid vs change of profession

df_covidp <- subset(df3,select = c("covid_affected","profession_change"))
df_covidp <- df_covidp[complete.cases(df_covidp),]


infected_count <- df_covidp %>% 
  group_by(covid_affected) %>% 
  summarise(count= n())


df_covid2 <- df_covidp %>% 
  group_by(covid_affected,profession_change) %>% 
  summarise(count = n())


tab2 <- data.frame(table(df_covid2$covid_affected))

df_covid2$total <- rep(infected_count$count,times = tab2$Freq)
df_covid2$freqden <- df_covid2$count/df_covid2$total


ggplot(data=df_covid2 ,aes(x= covid_affected,y= freqden ,fill= profession_change ))+
  geom_bar(position="dodge",stat="identity",width =0.5)+
  labs(y="count",x="covid infection",title="Effect of Covid on profession",
       fill= "change of profession")+
  theme(axis.text.x = element_text(angle = 90))

          
###############################################################################

##Lockdown effect on different age group
lckd$age <- cut(lckd$age,5)
levels(lckd$age) <- c("(17,27]", "(27,36]", "(36,46]", "(46, 55]", "(55,65]")

temp <- lckd[!is.na(lckd$age),]
lckd_age_physically <- temp %>% group_by(age, lockdown_effect_physically) %>% summarise(count = table(lockdown_effect_physically))
ggplot(lckd_age_physically)+
  geom_bar(aes(x = age, y = count, fill = lockdown_effect_physically), stat = "Identity", position = "dodge")+
  theme(axis.text.x = element_text(angle = 90))
#geom_text(aes(x = Age., y = count, label = count), position = position_dodge(width = 0.9))

lckd_age_financially <- temp %>% group_by(age, lockdown_effect_finance) %>% summarise(count = table(lockdown_effect_finance))
ggplot(lckd_age_financially)+
  geom_bar(aes(x = age, y = count, fill = lockdown_effect_finance), stat = "Identity", position = "dodge")+
  theme(axis.text.x = element_text(angle = 90))

lckd_age_mentally <- temp %>% group_by(age, lockdown_effect_mentally) %>% summarise(count = table(lockdown_effect_mentally))
ggplot(lckd_age_mentally)+
  geom_bar(aes(x = age, y = count, fill = lockdown_effect_mentally), stat = "Identity", position = "dodge")+
  theme(axis.text.x = element_text(angle = 90))


############################################################################
##lockdown financial effect on different income group
I1 <- select(df2, income_per_month, lockdown_effect_finance) 

I1_ <- I1 %>% group_by(income_per_month, lockdown_effect_finance) %>% summarise(count = table(lockdown_effect_finance))

ggplot(I1_) +
  geom_bar(aes(x = income_per_month, y = count, fill = lockdown_effect_finance), stat = "Identity", position = "dodge")+
  theme(axis.text.x = element_text(angle = 90))

################################################################################

################################################################################
          
## focus of people on different factors after covid age wise
focus_area <- select(df2, age, focus_on_physical_health, focus_on_mental_health, focus_on_finance)
focus_area$age <- cut(focus_area$age,5)
levels(focus_area$age) <- c("(17,27]", "(27,36]", "(36,46]", "(46, 55]", "(55,65]")

focus_area <- focus_area[!is.na(focus_area$age),]

##Focus physical health
focus_physical_health <- focus_area %>% group_by(age, focus_on_physical_health) %>% summarise(count = table(focus_on_physical_health))
focus_physical_health

ggplot(focus_physical_health) +
  geom_bar(aes(x = age, y = count, fill = focus_on_physical_health), stat = "Identity", position = "fill") +
  scale_y_continuous(labels = scales::percent_format())

##Focus Mental health
focus_mental_health <- focus_area %>% group_by(age, focus_on_mental_health) %>% summarise(count = table(focus_on_mental_health))
focus_mental_health

ggplot(focus_mental_health) +
  geom_bar(aes(x = age, y = count, fill = focus_on_mental_health), stat = "Identity", position = "fill")+
  scale_y_continuous(labels = scales::percent_format())

##Focus finance
focus_finance <- focus_area %>% group_by(age, focus_on_finance) %>% summarise(count = table(focus_on_finance))
focus_finance

ggplot(focus_finance) +
  geom_bar(aes(x = age, y = count, fill = focus_on_finance), stat = "Identity", position = "fill") +
  scale_y_continuous(labels = scales::percent_format())
##################################################################################################

##Focus_income_wise_post_covid
focus_income <- select(df2, income_per_month, focus_on_finance, focus_on_mental_health, focus_on_physical_health)
focus_income

##Mental health focus
mental_focus_income_wise <- focus_income %>% group_by(income_per_month, focus_on_mental_health) %>% summarise(count = table(focus_on_mental_health))
mental_focus_income_wise

ggplot(mental_focus_income_wise) +
  geom_bar(aes(x = income_per_month, y = count, fill = focus_on_mental_health), stat = "Identity", position = "fill") +
  scale_y_continuous(labels = scales::percent_format())

##Physical health focus
physical_focus_income_wise <- focus_income %>% group_by(income_per_month, focus_on_physical_health) %>% summarise(count = table(focus_on_physical_health))
physical_focus_income_wise

ggplot(physical_focus_income_wise) +
  geom_bar(aes(x = income_per_month, y = count, fill = focus_on_physical_health), stat = "Identity", position = "fill") +
  scale_y_continuous(labels = scales::percent_format())

##Finance focus
finance_focus_income_wise <- focus_income %>% group_by(income_per_month, focus_on_finance) %>% summarise(count = table(focus_on_finance))
finance_focus_income_wise

ggplot(finance_focus_income_wise) +
  geom_bar(aes(x = income_per_month, y = count, fill = focus_on_finance), stat = "Identity", position = "fill") +
  scale_y_continuous(labels = scales::percent_format())

#################################################################################################################

##vaccinated people vs traveling people
Vaccinated_traveling_people <- select(df2, vaccinated_no_of_doses, traveling_freq_work, traveling_freq_emergency, traveling_freq_vacation)
Vaccinated_traveling_people

##People traveling for work
work_traveling_people <- Vaccinated_traveling_people %>% group_by(vaccinated_no_of_doses, traveling_freq_work) %>% summarise(Vaccinated_vs_work_travel = table(traveling_freq_work))
work_traveling_people                                                                                                                        

ggplot(work_traveling_people) +
  geom_bar(aes(x = vaccinated_no_of_doses, y = Vaccinated_vs_work_travel, fill = traveling_freq_work), stat = "Identity", position = "fill") +
  scale_y_continuous(labels = scales::percent_format())

##People traveling for emergency
emergency_traveling_people <- Vaccinated_traveling_people %>% group_by(vaccinated_no_of_doses, traveling_freq_emergency) %>% summarise(Vaccinated_vs_emergency_travel = table(traveling_freq_emergency))
emergency_traveling_people

ggplot(emergency_traveling_people) +
  geom_bar(aes(x = vaccinated_no_of_doses, y = Vaccinated_vs_emergency_travel, fill = traveling_freq_emergency), stat = "Identity", position = "fill") +
  scale_y_continuous(labels = scales::percent_format())

##People traveling for vacation
vacation_traveling_people <- Vaccinated_traveling_people %>% group_by(vaccinated_no_of_doses, traveling_freq_vacation) %>% summarise(Vaccinated_vs_vacation_travel = table(traveling_freq_vacation))
vacation_traveling_people

ggplot(vacation_traveling_people) +
  geom_bar(aes(x = vaccinated_no_of_doses, y = Vaccinated_vs_vacation_travel, fill = traveling_freq_vacation), stat = "Identity", position = "fill") +
  scale_y_continuous(labels = scales::percent_format())

##########################################################################################################################################################

##People who dont exercise vs Their views on impact of exercise on mental health
exercise_mental_health <- df2 %>% group_by(exercise_freq, view_on_exercise) %>% summarise(Exercise_vs_effect_on_mental_health = table(view_on_exercise))
exercise_mental_health

view_of_people_impact_of_exercise <- exercise_mental_health[exercise_mental_health$exercise_freq == "Not at all",]
view_of_people_impact_of_exercise

percent <- round(100*view_of_people_who_dont_exercise$Exercise_vs_effect_on_mental_health/sum(view_of_people_who_dont_exercise$Exercise_vs_effect_on_mental_health),1)
percent <- paste0(percent, "%")

pie(view_of_people_who_dont_exercise$Exercise_vs_effect_on_mental_health, percent, col = c("Red", "Green", "Blue"), main = "% of people who dont exercise at all")
legend("topright", legend = c("May have an imapct", "doesnt have any impact", "yes it helps in mental health betterment"), cex = 0.8, fill = c("Red", "Green", "Blue"))

####################################################################################################################

##Vaccination vs Precautions people are taking
Vaccination_precautions_taken <- df2 %>% group_by(vaccinated_no_of_doses, covid_precautions) %>% summarise(people_vaccinated_vs_precautions = table(covid_precautions))
Vaccination_precautions_taken

ggplot(Vaccination_precautions_taken) +
  geom_bar(aes(x = vaccinated_no_of_doses, y = people_vaccinated_vs_precautions, fill = covid_precautions), stat = "Identity", position = "fill")+
  scale_y_continuous(labels = scales::percent_format())+
  theme(axis.text.x = element_text(angle = 90))
          
###########################################################################################################################

##Travel for emergency,work, vacation vs age
age_travel <- select(df2, age, traveling_freq_work, traveling_freq_emergency, traveling_freq_vacation)
age_travel

age_travel$age <- cut(age_travel$age,5)
levels(age_travel$age) <- c("(17,27]", "(27,36]", "(36,46]", "(46, 55]", "(55,65]")

age_travel <- age_travel[!is.na(age_travel$age),]

##Travel for work vs age
age_travel_work <- age_travel %>% group_by(age, traveling_freq_work) %>% summarise(Age_wise_work_travel = table(traveling_freq_work))
age_travel_work

ggplot(age_travel_work)+
  geom_bar(aes(x = age, y = Age_wise_work_travel, fill = traveling_freq_work), stat = "Identity", position = "fill")+
  scale_y_continuous(labels = scales::percent_format())+
  labs(x = "Age groups", y = "% of people traveling for work", title = "People traveling for work Vs. Age group")

##Travel emergency vs age
age_travel_emergency <- age_travel %>% group_by(age, traveling_freq_emergency) %>% summarise(Age_wise_emergency_travel = table(traveling_freq_emergency))
age_travel_emergency

ggplot(age_travel_emergency)+
  geom_bar(aes(x = age, y = Age_wise_emergency_travel, fill = traveling_freq_emergency), stat = "Identity", position = "fill")+
  scale_y_continuous(labels = scales::percent_format())+
  labs(x = "Age groups", y = "% of people traveling for emergency", title = "People traveling for emergency Vs. Age group")

##Travel vacation vs age 
age_travel_vacation <- age_travel %>% group_by(age, traveling_freq_vacation) %>% summarise(Age_wise_vacation_travel = table(traveling_freq_vacation))
age_travel_vacation

age_travel_vacation$percentage <- c("28.57%", "71.42%", "19.04%", "80.95%", "18.75%", "81.25%", "28.89%", "71.11%", "26.67%", "73.33%")

ggplot(age_travel_vacation)+
  geom_bar(aes(x = age, y = Age_wise_vacation_travel, fill = traveling_freq_vacation), stat = "Identity", position = "fill")+
  scale_y_continuous(labels = scales::percent_format())+
  labs(x = "Age groups", y = "% of people traveling for vacation", title = "People traveling for vacation Vs. Age group")
  #geom_text(aes(x = age, y = Age_wise_vacation_travel, fill = traveling_freq_vacation), label = age_travel_vacation$percentage, size = 4, position = position_stack(vjust = 1.1))

####################################################################################

##Travel for emergency,work, vacation vs income
income_travel <- select(df2, income_per_month, traveling_freq_work, traveling_freq_emergency, traveling_freq_vacation)
income_travel

##Travel for work vs income_per_month
income_travel_work <- income_travel %>% group_by(income_per_month, traveling_freq_work) %>% summarise(income_wise_work_travel = table(traveling_freq_work))
income_travel_work

ggplot(income_travel_work)+
  geom_bar(aes(x = income_per_month, y = income_wise_work_travel, fill = traveling_freq_work), stat = "Identity", position = "fill")+
  scale_y_continuous(labels = scales::percent_format())+
  labs(x = "income groups", y = "% of people traveling for work", title = "People traveling for work Vs. different income group")

##Travel emergency vs income_per_month
income_travel_emergency <- income_travel %>% group_by(income_per_month, traveling_freq_emergency) %>% summarise(income_wise_emergency_travel = table(traveling_freq_emergency))
income_travel_emergency

ggplot(income_travel_emergency)+
  geom_bar(aes(x = income_per_month, y = income_wise_emergency_travel, fill = traveling_freq_emergency), stat = "Identity", position = "fill")+
  scale_y_continuous(labels = scales::percent_format())+
  labs(x = "income groups", y = "% of people traveling for emergency", title = "People traveling for emergency Vs. different income group")

##Travel vacation vs income_per_month
income_travel_vacation <- income_travel %>% group_by(income_per_month, traveling_freq_vacation) %>% summarise(income_wise_vacation_travel = table(traveling_freq_vacation))
income_travel_vacation

ggplot(income_travel_vacation)+
  geom_bar(aes(x = income_per_month, y = income_wise_vacation_travel, fill = traveling_freq_vacation), stat = "Identity", position = "fill")+
  scale_y_continuous(labels = scales::percent_format())+
  labs(x = "Income groups", y = "% of people traveling for vacation", title = "People traveling for vacation Vs. different income group")


#####################################################################################################

##change of profession with mentally and finance focus
change_profession_mentally_finance_focus <- select(df2, profession_change, focus_on_mental_health, focus_on_finance)
change_profession_mentally_finance_focus

##change of profession and- finance focus
change_profession_finance_focus <- change_profession_mentally_finance_focus %>% group_by(profession_change, focus_on_finance) %>% summarise(count = table(focus_on_finance))
change_profession_finance_focus

ggplot(change_profession_finance_focus)+
  geom_bar(aes(x = profession_change, y = count, fill = focus_on_finance), stat = "identity", position = "fill")+
  scale_y_continuous(labels = scales::percent_format())+
  labs(x = "Profession change", y = "% of people focussing on finance with or without profession change", title = "People who changed their profession or not vs. focus on finance")

##change of profession and mentally focus
change_profession_mental_focus <- change_profession_mentally_finance_focus %>% group_by(profession_change, focus_on_mental_health) %>% summarise(count = table(focus_on_mental_health))
change_profession_mental_focus

ggplot(change_profession_mental_focus)+
  geom_bar(aes(x = profession_change, y = count, fill = focus_on_mental_health), stat = "identity", position = "fill")+
  scale_y_continuous(labels = scales::percent_format())+
  labs(x = "Profession change", y = "% of people focussing on mental health with or without profession change", title = "People who changed their profession or not vs. focus on mental health")

#################################################################################################################

## covid affected and precautions taken
covid_affected_precautions <- df2 %>% group_by(covid_affected, covid_precautions) %>% summarise(Count_of_precautions_taken = table(covid_precautions))
covid_affected_precautions

ggplot(covid_affected_precautions)+
  geom_bar(aes(x = covid_affected, y = Count_of_precautions_taken, fill = covid_precautions), stat = "identity", position = "fill")+
  scale_y_continuous(labels = scales::percent_format())+
  labs(x = "Covid affected or not", y = "Covid Precautions taken", title = "Covid affected vs precautions taken")+
  theme(axis.text.x = element_text(angle = 90))

###################################################################################################################################

##Exercise duration and precautions taken
Exercise_duration_precautions <- df2 %>% group_by(exercise_freq, covid_precautions) %>% summarise(count = table(covid_precautions))
Exercise_duration_precautions

ggplot(Exercise_duration_precautions)+
  geom_bar(aes(x = exercise_freq, y = count, fill = covid_precautions), stat = "identity", position = "fill")+
  scale_y_continuous(labels = scales::percent_format())+
  labs(x = "Frequency of exercise", y = "Covid Precautions taken", title = "Covid affected vs precautions taken")+
  theme(axis.text.x = element_text(angle = 90))
