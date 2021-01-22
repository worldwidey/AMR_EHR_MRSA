rm(list=ls())
rm(list="")
setwd("C:/Users/inyoungjun/Dropbox (Personal)/[research]AMR_iy/Finals")


library(sqldf)
library(tidyr)
library(dplyr)
library(tidyverse)
library(data.table)


`%!in%` = Negate(`%in%`)
daydiff <- function(a,b,c,d){
  result<-(a-b)*365+c-d
  return(result)
} #a:Later Year, b:Former Year, c:Later Date, d:Former Date

########################################################################
Cohort<-readRDS("Cohort.RDS")#140 obs of 3 variables

#####demographics
Age<-readRDS("C:/Users/inyoungjun/Dropbox (Personal)/[research]AMR_iy/Finals/Variable/Age.RDS")
Sex<-readRDS("C:/Users/inyoungjun/Dropbox (Personal)/[research]AMR_iy/Finals/Variable/Sex.RDS")
Race<-readRDS("C:/Users/inyoungjun/Dropbox (Personal)/[research]AMR_iy/Finals/Variable/Race.RDS")
Ethnicity<-readRDS("C:/Users/inyoungjun/Dropbox (Personal)/[research]AMR_iy/Finals/Variable/Ethnicity.RDS")

#####variables
Community<-readRDS("C:/Users/inyoungjun/Dropbox (Personal)/[research]AMR_iy/Finals/Variable/Community.RDS")
Community<-Community %>% select(PID,CA)
#patient had Admission = 0 OR Admission = 1 but the date between diagnosis and admission is within 2 days.

Concomitant<-readRDS("C:/Users/inyoungjun/Dropbox (Personal)/[research]AMR_iy/Finals/Variable/Concomitant.RDS")

WIC<-readRDS("C:/Users/inyoungjun/Dropbox (Personal)/[research]AMR_iy/Finals/Variable/WIC.RDS")
WIC <- WIC %>% rename(PID=id)
ICU<-readRDS("C:/Users/inyoungjun/Dropbox (Personal)/[research]AMR_iy/Finals/Variable/ICU.RDS")
#Consider admission=1 if the BM date is Within admission(+3)-discharge date.

Definite<-readRDS("C:/Users/inyoungjun/Dropbox (Personal)/[research]AMR_iy/Finals/Variable/Definite.RDS")

Platelet<-readRDS("C:/Users/inyoungjun/Dropbox (Personal)/[research]AMR_iy/Finals/Variable/Platelet.RDS")
C_reactive<-readRDS("C:/Users/inyoungjun/Dropbox (Personal)/[research]AMR_iy/Finals/Variable/C_reactive.RDS")

PBS<-readRDS("C:/Users/inyoungjun/Dropbox (Personal)/[research]AMR_iy/Finals/Variable/PBS.RDS")
hvisa<-readRDS("C:/Users/inyoungjun/Dropbox (Personal)/[research]AMR_iy/Finals/Variable/hvisa.RDS")
hvisa<-hvisa %>% rename(PID=Deidentified_Patient_ID)

#####Outcome
Mortal30_v2<-readRDS("C:/Users/inyoungjun/Dropbox (Personal)/[research]AMR_iy/Finals/Variable/Mortal30_v2.RDS")
Mortal30_v2<-Mortal30_v2 %>% select(PID,Mortal30)

Data<- Cohort %>% 
       left_join(Age, group_by = PID) %>% 
       left_join(Sex, group_by = PID) %>% 
       left_join(Race, group_by = PID) %>% 
       left_join(Ethnicity, group_by = PID) %>% 
       left_join(Community, group_by = PID) %>% 
       left_join(Concomitant, group_by = PID) %>% 
       left_join(WIC, group_by = PID) %>% 
       left_join(ICU, group_by = PID) %>% 
       left_join(Definite, group_by = PID) %>% 
       left_join(Platelet, group_by = PID) %>% 
       left_join(C_reactive, group_by = PID) %>% 
       left_join(PBS, group_by = PID) %>% 
       left_join(hvisa, group_by = PID) %>% 
       left_join(Mortal30_v2,group_by = PID)

head(Data)
str(Data)

Data_v1 <- Data %>% 
                mutate(Definite_Regimen=ifelse(Daptomycin==1,"Daptomycin",ifelse(Vancomycin==1,"Vancomycin","Other"))) %>% 
                select(PID, BM_Age, Sex, Race, Ethnicity, CA, bonejoint,pnuemonia, wscore, ICU_BM, Avg_PL, Avg_CR, PBS, Definite_Regimen,hvisa,Mortal30)    
head(Data_v1)
Data_v1$CA<-as.factor(Data_v1$CA)
Data_v1$ICU_BM<-as.factor(Data_v1$ICU_BM)
Data_v1$hvisa<-as.factor(Data_v1$hvisa)
Data_v1$Mortal30<-as.factor(Data_v1$Mortal30)

head(Data_v1)
saveRDS(Data_v1,"Data_v1.RDS")
#summary(Data_v1) #Table for frequency, %, NA

table(Data_v1$CA)
21/(21+117)#0.1521739
Data_v2<-Data_v1 %>% 
         mutate(CA2=ifelse(is.na(CA),0.1521739,CA),
                   Avg_PL2=ifelse(is.na(Avg_PL),mean(Avg_PL,na.rm=TRUE),Avg_PL),
                   Avg_CR2=ifelse(is.na(Avg_CR),mean(Avg_CR,na.rm=TRUE),Avg_CR),
                   PBS2=ifelse(is.na(PBS),mean(PBS,na.rm=TRUE),PBS))
summary(Data_v2)
saveRDS(Data_v2,"Data_v2.RDS")
Data_v2<-readRDS("Data_v2.RDS")
is.na(Data_v2) #Data_v2 includes Data_v2 and CA2, Avg_PL2, Avg_CR2, PBS2

####Modeling with our dataframe####
Data_v2<-readRDS("Data_v2.RDS")
#table(Data_v2$Mortal30) #0: 67, 1: 10, 2:63
head(Data_v4)

factor_variable<-c("Sex","Race","CA","bonejoint","pnuemonia","ICU_BM","Definite_Regimen","Definite_Regimen2","hvisa","Mortality30_onset","Mortality180_discharge")
Data_v4<- Data_v2 %>%
            left_join(Outcome, by=c("PID"="PID")) %>% 
            mutate(Mortality30_onset=replace_na(ifelse(Death_onset<=30,1,0),0),
                   Mortality30_discharge=replace_na(ifelse(Death_discharge<=30,1,0),0),
                   Mortality180_onset=replace_na(ifelse(Death_onset<=180,1,0),0),
                   Mortality180_discharge=replace_na(ifelse(Death_discharge<=180,1,0),0)) %>%
            select(PID, BM_Age, Sex, Race, CA, bonejoint, pnuemonia,wscore,ICU_BM,Avg_PL2,Avg_CR2,PBS2,Definite_Regimen,hvisa,
                   Mortality30_onset,Mortality30_discharge,Mortality180_onset,Mortality180_discharge) %>% 
            mutate(Definite_Regimen2=ifelse(Definite_Regimen=="Vancomycin",1,0)) %>% 
            mutate_at(factor_variable,funs(factor(.)))


table(Data_v4$bonejoint)
table(Outcome$Death_discharge)
table(Data_v4$Mortality180_discharge)

table(Data_v4$Definite_Regimen)#0:49, 1:1, 2:90
table(Data_v4$Definite_Regimen2)#0:50, 2:90
xtabs(~Mortality180_onset+bonejoint,data=Data_v4)

###Make OR table####
ORtable=function(x,digits=3){
  suppressMessages(a<-confint(x))
  result=data.frame(exp(coef(x)),exp(a))
  result=round(result,digits)
  result=cbind(result,round(summary(x)$coefficient[,4],3))
  colnames(result)=c("OR","2.5%","97.5%","p")
  result
}

model1<-glm(Mortality30_onset ~ BM_Age+CA+bonejoint+pnuemonia+wscore+ICU_BM+PBS2+Avg_PL2+Avg_CR2+Definite_Regimen2+hvisa,
            family=binomial,data=Data_v4)

model2<-glm(Mortality30_discharge ~ BM_Age+CA+bonejoint+pnuemonia+wscore+ICU_BM+PBS2+Avg_PL2+Avg_CR2+Definite_Regimen2+hvisa,
            family=binomial,data=Data_v4)

model3<-glm(Mortality180_onset ~ BM_Age+CA+bonejoint+pnuemonia+wscore+ICU_BM+PBS2+Avg_PL2+Avg_CR2+Definite_Regimen2+hvisa,
            family=binomial,data=Data_v4)

model4<-glm(Mortality180_discharge ~ BM_Age+CA+bonejoint+pnuemonia+wscore+ICU_BM+PBS2+Avg_PL2+Avg_CR2+Definite_Regimen2+hvisa,
            family=binomial,data=Data_v4)

ORtable(model3)
ORtable(model4)
table(Data_v4$Mortality30_onset)
table(Data_v4$Mortality30_discharge)

model_ORtable<-rbind(ORtable(model1),ORtable(model2),ORtable(model3),ORtable(model4))
write.csv(model_ORtable,"model_ORtable2.csv")

#####Following Yang's Coefficient#####
ODDS<-c(O_AGE=1.030, O_CA=2.324, O_Bone=4.834, O_Pnue=1.436, O_WIC=1.108, O_ICU=2.227, O_PBS=1.298, O_PL=0.996, O_CR=1.007, O_Dapto=0.128, O_glyco=0.238, O_hvisa=1.698)
logODDS<-as.data.frame(log(ODDS)) 

Score1<-Data_v2$BM_Age*logODDS[1,]+
        Data_v2$CA2*logODDS[2,]+
        ifelse(Data_v2$bonejoint=="1",logODDS[3,],0)+
        ifelse(Data_v2$pnuemonia=="1",logODDS[4,],0)+
        Data_v2$wscore*logODDS[5,]+
        ifelse(Data_v2$ICU_BM=="1",logODDS[6,],0)+
        Data_v2$PBS2*logODDS[7,]+
        Data_v2$Avg_PL2*logODDS[8,]+
        Data_v2$Avg_CR2*logODDS[9,]+
        ifelse(Data_v2$Definite_Regimen=="Daptomycin",logODDS[10,],
               ifelse(Data_v2$Definite_Regimen=="Vancomycin",logODDS[11,],0))+
        ifelse(Data_v2$hvisa=="1",logODDS[12,],0)

  
Prob1<-1/(1+exp(-Score1))
Binary_Prob1<-ifelse(Prob1>=0.5,1,0)


Data_v3<- Data_v2 %>% 
          mutate(Score1 = Score1, 
                 Prob1 = Prob1, 
                 Binary_Prob1 = Binary_Prob1,
                 Mortality30 = ifelse(Mortal30==1,1,0))

table(Data_v3$Mortality30)

TP<-Data_v3[Data_v3$Mortal30==1,]
TN<-Data_v3[Data_v3$Mortal30==0,]

table(TN$Binary_Prob1)
table(Binary_Prob1)
which(Binary_Prob1==0)
#65, 76, 126
Cohort[c(65,76,126),]
Data_v2[c(65,76,126),]

saveRDS(Data_v3,"Data_v3.RDS")










######Table 1######

summary(Data_v1)
write.csv(summary(Data),"Data_Summary.csv")



#Age, WIC, Platelet, C_reactive, PBS

mean(Data$BM_Age,na.rm=TRUE)
mean(Data$wscore,na.rm=TRUE)
median(Data$wscore,na.rm=TRUE)
mean(Data$Avg_PL,na.rm = TRUE)
mean(Data$Avg_CR,na.rm = TRUE)
mean(Data$PBS,na.rm=TRUE)
median(Data$PBS,na.rm=TRUE)
summary(Data$Avg_CR)
sd(Data$BM_Age,na.rm=TRUE)
sd(Data$wscore,na.rm=TRUE)
sd(Data$Avg_PL,na.rm=TRUE)
sd(Data$Avg_CR,na.rm=TRUE)
sd(Data$PBS,na.rm=TRUE)

summary_sex <- Data %>%
  group_by(Sex) %>%
  summarise(n = n()) %>%
  mutate(Sex_percent = n / sum(n))

summary_race <- Data %>%
  group_by(Race) %>%
  summarise(n = n()) %>%
  mutate(Race_percent = n / sum(n))

summary_ethnicity <- Data %>%
  group_by(Ethnicity) %>%
  summarise(n = n()) %>%
  mutate(Ethnicity_percent = n / sum(n))

summary_CA <- Data %>%
#  filter(!is.na(CA)) %>% 
  group_by(CA) %>%
  summarise(n = n()) %>%
  mutate(CA_percent = n / sum(n))

summary_bone <- Data %>%
  #  filter(!is.na(CA)) %>% 
  group_by(bonejoint) %>%
  summarise(n = n()) %>%
  mutate(bone_percent = n / sum(n))

summary_skin <- Data %>%
  #  filter(!is.na(CA)) %>% 
  group_by(skinsoft) %>%
  summarise(n = n()) %>%
  mutate(skin_percent = n / sum(n))

summary_pnuemonia <- Data %>%
  #  filter(!is.na(CA)) %>% 
  group_by(pnuemonia) %>%
  summarise(n = n()) %>%
  mutate(pnuemonia_percent = n / sum(n))

summary_ICU <- Data %>%
  group_by(ICU_BM) %>%
  summarise(n = n()) %>%
  mutate(ICU_percent = n / sum(n))

summary_Daptomycin <- Data %>%
  group_by(Daptomycin) %>%
  summarise(n = n()) %>%
  mutate(Daptomycin_percent = n / sum(n))

summary_Vancomycin <- Data %>%
  group_by(Vancomycin) %>%
  summarise(n = n()) %>%
  mutate(Vancomycin_percent = n / sum(n))

summrary_Linezolid <-Data %>%
  group_by(Linezolid) %>%
  summarise(n = n()) %>%
  mutate(Linezolid_percent = n / sum(n))

summrary_hvisa <-Data %>%
  group_by(hvisa) %>%
  summarise(n = n()) %>%
  mutate(hvisa_percent = n / sum(n))

49/140

