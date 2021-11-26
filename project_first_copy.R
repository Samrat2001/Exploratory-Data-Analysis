library(dplyr)
library(stringr)


df1 <- read.csv("data2.csv",as.is = c(1:5),na.strings = '')
#View(df1)
?read.csv
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
df2$age <- as.numeric(df2$age)
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


df3$profession1[which(str_detect(df3$profession1,c("student|nursing|studying")))] = "student"
df3$profession1[which(str_detect(df3$profession1,c("govt|goverment|employee|service|government")))] = "Employee"
table(df3$profession1)

