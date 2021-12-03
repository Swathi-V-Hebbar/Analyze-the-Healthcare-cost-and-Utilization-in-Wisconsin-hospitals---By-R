#install.packages("readxl")
library(readxl)
Healthcare<- read_excel("C://Users//admin//Desktop//R//R_class//HospitalCosts.xlsx")
View(Healthcare)
str(Healthcare)

Healthcare$AGE <- as.factor(Healthcare$AGE)
Healthcare$FEMALE <- as.factor(Healthcare$FEMALE)
 # Healthcare$RACE <- as.numeric(Healthcare$RACE)
Healthcare$APRDRG <- as.factor(Healthcare$APRDRG)
str(Healthcare)

library(dplyr)
Healthcare<-arrange(Healthcare,AGE)

Healthcare <- na.omit(Healthcare)
sum(is.na(Healthcare))

# apply(Healthcare,2,FUN = function(x){sum(is.na(x))})
# Healthcare=Healthcare[(is.na(Healthcare$RACE))==F,]

 table(Healthcare$AGE)
 table(Healthcare$FEMALE)
 table(Healthcare$LOS)
 Healthcare$LOS[Healthcare$LOS>4] <- 4
 table(Healthcare$LOS)
 table(Healthcare$RACE)
 Healthcare$RACE[Healthcare$RACE>2] <- 2
 table(Healthcare$RACE)
 table(Healthcare$APRDRG)
 summary(Healthcare$TOTCHG)

 Healthcare <- filter(Healthcare,Healthcare$LOS>0)

 # summary(Healthcare)

 
 
 # plot(Healthcare$LOS)
  #qplot(data = Healthcare, LOS) + ylab("Number of people")
 # plot(Healthcare$AGE,Healthcare$LOS)
 library(magrittr)
#library(dplyr)
# library(ggplot2)
table(Age=Healthcare$AGE,Length_of_stay=Healthcare$LOS)
# plot(Healthcare$AGE,Healthcare$LOS)
Los_report <- Healthcare %>% group_by(Age=Healthcare$AGE) %>% summarise(Total_lOS=sum(LOS))
View(Los_report)
# barplot(Los_report$Age,Los_report$Total_lOS)
# str(Los_report)
# class(Los_report$Total_los)
# ggplot(Healthcare,aes(AGE,LOS)) + geom_bar()

# l1_df<- as.data.frame(L1)
# View(l1_df)
# plot(l1_df)
# ggplot(l1_df,aes(Age,Freq)) +
#         geom_bar()
# cbind(L1[,2])
# class(cbind(L1[,2]))
# Length_of_stay <- Healthcare %>% group_by(Healthcare$AGE,Healthcare$LOS) %>% summarise(C=sum(LOS))
Age_Max_report_mean <- Healthcare %>% group_by(Age=Healthcare$AGE) %>% summarise(Total_charges=mean(TOTCHG))
Age_Max_report_mean <- Age_Max_report_mean %>% arrange(desc(Total_charges))
plot(Age_Max_report_mean)
View(Age_Max_report_mean)


Age_Max_report_sum <- Healthcare %>% group_by(Age=Healthcare$AGE) %>% summarise(Total_charges=sum(TOTCHG))
Age_Max_report_sum <- Age_Max_report_sum %>% arrange(desc(Total_charges))
View(Age_Max_report_sum)
plot(Age_Max_report_sum)




Expensive <- Healthcare %>% filter(TOTCHG==max(Healthcare$TOTCHG))
Expensive[,5:6]

# table(Healthcare$APRDRG)

Diag_max_report <- Healthcare %>% group_by(Diagnosis_group=Healthcare$APRDRG ) %>% summarise(Mean_charge=mean(TOTCHG))
Diag_max_report <- Diag_max_report %>% arrange(desc(Mean_charge))
View(Diag_max_report)
plot(Diag_max_report)





table(Healthcare$APRDRG,Healthcare$LOS)
los_report <- Healthcare %>% filter(Healthcare$LOS==max(Healthcare$LOS))
los_report <- los_report %>% group_by(aprdrg=los_report$APRDRG) %>% summarise(count=n())
los_report %>% filter(count==max(los_report$count))

View(los_report)

# library(ggplot2)
# plot(Healthcare$APRDRG)
#  qplot(data = Healthcare, APRDRG) + ylab("Number of people")

# 
# table(Healthcare$APRDRG,Healthcare$LOS)
# str(Healthcare)

# table(Healthcare$RACE)
lm(TOTCHG~RACE, Healthcare)->mod1
summary(mod1)


Healthcare$AGE <- as.numeric(Healthcare$AGE)
lm(TOTCHG~AGE + FEMALE,Healthcare) -> mod2
summary(mod2)


lm(LOS ~ AGE + FEMALE + RACE, Healthcare) -> mod3
summary(mod3)


Healthcare$APRDRG <- as.numeric(Healthcare$APRDRG)
lm(TOTCHG ~ . , Healthcare) -> mod4
summary(mod4)

# plot(Healthcare$RACE,Healthcare$TOTCHG)
# 
# 
# 
# table(Healthcare$RACE)
# Diag_max_report %>% filter(mean_charge==max(Diag_max_report$mean_charge))
# 
# 
# # str(Healthcare)
# table(Healthcare$TOTCHG,Healthcare$LOS)
# Diag_report <- Healthcare %>% group_by(Healthcare$APRDRG) %>% summarise()
# 
# 
# 
