#rm(list=ls())
rm(list="l_BM")
#setwd("C:/Users/jiy_/Dropbox (Personal)/[research]AMR_iy/Finals")
setwd("C:/Users/inyoungjun/Dropbox (Personal)/[research]AMR_iy/Finals")
#whole population number between 2011 and 2019
#whole<-dnew %>% select(PID, D_Year) %>% filter(D_Year>=2011)
#length(unique(whole$PID))
#Total 58461 Patients between 2011 and 2019

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

#####################################

#Shannan's Data #gram-positive label
shannan<-read.csv("C:/Users/jiy_/Dropbox (Personal)/[research]AMR_iy/Shannan/MRSApop_ABgram_5.5.2020.csv") #gram-positive/source of infection


#STEP 1 MRSA-RVS : ID_vanco_rvs2(Including MIC2, Intermediate, Resistant) 
#Vancomycin Intermediate/ Resistant
a<-readRDS("C:/Users/inyoungjun/Dropbox (Personal)/[research]AMR_iy/RDS/a.RDS")
a_vanco<-a %>% filter(tolower(antibiotic) %like% "vanco")
a_vanco$SENSITIVITY_VALUE <-as.numeric(a_vanco$SENSITIVITY_VALUE)
hist(a_vanco$SENSITIVITY_VALUE)
summary(a_vanco$SENSITIVITY_VALUE)
#write.csv(a_vanco, "a_vanco.csv", row.names=FALSE)


length(unique(a_vanco$Deidentified_Patient_ID[a_vanco$Year<=2019|a_vanco$Year>=2011]))

a_vanco_rvs<-a_vanco %>% filter(tolower(suscept) %in% c("intermediate","resistant")) %>% filter(organism %!in% c("ESCHERICHIA COLI","KLEBSIELLA PNEUMONIAE","PSEUDOMONAS AERUGINOSA")) 
#-1 because it is gram-negative.
table(a_vanco_rvs$suscept)

ID_vanco_rvs<-a_vanco_rvs %>% select(Deidentified_Patient_ID) %>% mutate(PID=Deidentified_Patient_ID) %>% distinct(PID)

#2886 Unique Patient
saveRDS(ID_vanco_rvs,"ID_vanco_rvs.RDS")
ID_vanco_rvs<-readRDS("ID_vanco_rvs.RDS")


# Including MIC=2 as well
a_vanco_mic2<-a %>% filter(tolower(antibiotic) %like% "vanco") %>% filter(SENSITIVITY_VALUE==2)
a_vanco_mic2_v2<-a_vanco_mic2 %>% filter(organism %!in% c("ESCHERICHIA COLI","KLEBSIELLA PNEUMONIAE","PSEUDOMONAS AERUGINOSA")) 
ID_vanco_mic2<-a_vanco_mic2 %>% select(Deidentified_Patient_ID) %>% mutate(PID=Deidentified_Patient_ID) %>% distinct(PID)#176

ID_vanco_rvs2<- rbind(ID_vanco_rvs,ID_vanco_mic2) %>% distinct(PID)
head(ID_vanco_rvs2)
2866+176#3042
#3033 patients are defined as 'Reduced Vancomycin" (regardless of Year)
saveRDS(ID_vanco_rvs2,"ID_vanco_rvs2.RDS")
ID_vanco_rvs2<-readRDS("ID_vanco_rvs2.RDS")
##################################################################

#STEP 2 Bacteremia due to MRSA :BM_MRSA_ID 
#dnew.RDS is converted ICD-code dataset of diagnoses.csv (solved comma problem as well)
dnew<-readRDS("dnew.RDS")
dnew$Bacteremia<-0
dnew$Bacteremia[dnew$ICD_Code2=="03810"]<-1
dnew$Bacteremia[dnew$ICD_Code2=="03812"]<-1
dnew$Bacteremia[dnew$ICD_Code2=="7907"]<-1
dnew$Bacteremia[dnew$ICD_Code2=="99591"]<-1
table(dnew$Bacteremia)

dnew$MRSA<-0
dnew$MRSA[dnew$ICD_Code2=="03812"]<-1
dnew$MRSA[dnew$ICD_Code2=="04112"]<-1
length(unique(dnew$PID[dnew$ICD_Code2=="03812"]))#664
length(unique(dnew$PID[dnew$ICD_Code2=="04112"]))#5322

D_03812<-dnew %>% filter(D_Year>=2011) %>% filter(ICD_Code2=="03812") %>% select(PID) %>% distinct(PID)#479
D_04112<-dnew %>% filter(D_Year>=2011) %>% filter(ICD_Code2=="04112") %>% select(PID) %>% distinct(PID)#4356

#dnew<-dnew %>% rename(PID=Deidentified_Patient_ID, D_Year=Year, D_Date=Date_of_Service)
#saveRDS(dnew,"dnew.RDS")


BM_ID <-dnew %>% filter(D_Year>=2011) %>% filter(Bacteremia==1) %>% arrange(PID,D_Year,D_Date) %>%  distinct(PID)#11909
#MRSA_ID <-dnew %>% filter(MRSA==1) %>% arrange(PID,D_Year,D_Date) %>% distinct(PID)#4356

BM_MRSA_ID_1<-BM_ID %>% filter(PID %in% D_03812$PID) #479 #every patient is considered as Bacteremia due to MRSa
BM_MRSA_ID_2<-BM_ID %>% filter(PID %in% D_04112$PID) #2174 #among those, patient whose MRSA record is before/after the Bacteremia within 7 days

#Let's calculate day difference for those 2174 patients

BM_MRSA_2<-dnew %>% filter(PID %in% BM_MRSA_ID_2$PID) %>% filter(Bacteremia==1|MRSA==1) %>% arrange(PID, D_Year,D_Date)
length(unique(BM_MRSA_2$PID))#2174

#first BM record after 2011
BM_first_2<-BM_MRSA_2 %>% filter(Bacteremia==1) %>% filter(D_Year>=2011) %>% arrange(PID, D_Year,D_Date) %>% distinct(PID,.keep_all = TRUE) %>% mutate(BM_Year = D_Year, BM_Date = D_Date) #2174
head(BM_first)

MRSA_first<-BM_MRSA_2 %>% filter(MRSA==1) %>% arrange(PID, D_Year,D_Date) %>% distinct(PID,.keep_all = TRUE) %>% mutate(M_Year = D_Year, M_Date = D_Date) #2174
head(MRSA_first)

Order<-sqldf("SELECT BM_first_2.PID AS PID, BM_first_2.BM_Year, BM_first_2.BM_Date, MRSA_first.M_Year, MRSA_first.M_Date FROM BM_first_2 JOIN MRSA_first ON BM_first_2.PID == MRSA_first.PID") #2174

Order<-Order %>% mutate(Daydiff_BM_M=daydiff(BM_Year,M_Year,BM_Date,M_Date)) %>% filter(Daydiff_BM_M>=-7&Daydiff_BM_M<=7) %>% select(PID) 
#876 (deleted records if MRSA and Bacteremia happens more than a week later; a week difference)

#We can construct Bacteremia due to MRSA by combining
#1)D_03812 population and 2)Order population(who satisfied order condition between BM and MRSA)

head(D_03812)
head(Order)

BM_MRSA<-rbind(D_03812,Order)
BM_MRSA_ID<-BM_MRSA %>% distinct(PID) %>% arrange(PID) #1205
head(BM_MRSA_ID)
saveRDS(BM_MRSA_ID,"BM_MRSA_ID.RDS") #1205
# combining 1) and 2)

# #Using Intermediate/Resistant
#BM_MRSA_RVS_ID<-BM_MRSA_ID %>% filter(PID %in% ID_vanco_rvs$PID) #169
# head(BM_MRSA_RVS_ID)
# # We have 178 unique patients who had Bacteremia due to MRSA with reduced vancomycin susceptibility


length(unique(ID_vanco_rvs2$PID))#3033

#Using Intermediate/Resistant + MIC=2
BM_MRSA_RVS_ID2<-BM_MRSA_ID %>% filter(PID %in% ID_vanco_rvs2$PID) #178
length(BM_MRSA_RVS_ID2$PID) #178
# We have 178 unique patients who had Bacteremia due to MRSA with reduced vancomycin susceptibility
saveRDS(BM_MRSA_RVS_ID2,"BM_MRSA_RVS_ID2.RDS")


#STEP 3
#check it is their first peisodes by including only patients who had at least a year of medical history.
BM_MRSA_RVS_ID2<-readRDS("BM_MRSA_RVS_ID2.RDS")#178

#Make BM_MRSA_RVS's first BM date variable
BM_MRSA_RVS<-dnew %>% filter(PID %in% BM_MRSA_RVS_ID2$PID) #Disease record of 178 study cohort population
length(unique(BM_MRSA_RVS$PID))#178 unique patients


###################################################178 UNIQUE PATIENTS...
#I THINK IT WOULD BE BETTER TO GO USING MIC = 2 POPULATION IF THE SIZE IS NOT THAT MUCH DIFFERENT (n=176)
#But when I checked, it only returned 12 obs. So I stick to the original sight. (MIC=2 + Intermedieate + Resistant)
#BM_MRSA_RVS_MIC2<-BM_MRSA_ID %>% filter(PID %in% ID_vanco_mic2$PID) #12


First_BM<-BM_MRSA_RVS %>% filter(D_Year>=2011) %>% filter(Bacteremia==1) %>% arrange(PID, D_Year,D_Date) %>% distinct(PID,.keep_all = TRUE) %>% mutate(BM_Year = D_Year, BM_Date = D_Date) %>% select(PID,BM_Year,BM_Date)#178
head(First_BM)#This include patient's first Bacteremia date after 2011.


###########check with d data#############


dnew_BM<-sqldf("SELECT dnew_cohort.PID, dnew_cohort.D_Year, dnew_cohort.D_Date, dnew_cohort.ICD_Code2, dnew_cohort.ICD_Description, First_BM.BM_Year,First_BM.BM_Date FROM dnew_cohort LEFT JOIN First_BM ON dnew_cohort.PID==First_BM.PID")

head(dnew_BM)
#Let's check if the patient has any diagnosis record a year ago from their first Bacteremia diagnosis date
dnew_oneyear<-dnew_BM %>% mutate(Diff_year=daydiff(BM_Year,D_Year,BM_Date,D_Date)) %>% filter(Diff_year>=365)
length(unique(dnew_oneyear$PID))#139
#139 patient who had record of Bacteremia diagnosis date had one year prior medical history
#We need to check for 39(=178-139) patients if they had any other medical history from different dataset.
#178-139 = 39
not_one<- dnew_BM %>% select(PID) %>% filter(PID %!in% dnew_oneyear$PID) %>% distinct(PID)#39


###########check with p data#############
#p<-readRDS("C:/Users/jiy_/Dropbox (Personal)/[research]AMR_iy/RDS/p.RDS")
#p<-p %>% rename(PID=Deidentified_Patient_ID, P_Year=Year, P_Date=Date_of_Service)
#saveRDS(p,"p.RDS")
p<-readRDS("p.RDS")
p_cohort<-p %>% subset(PID %in% BM_MRSA_RVS$PID)#include all disease information of cohort (n=178)
length(unique(p_cohort$PID))#178
p_BM<-sqldf("SELECT p_cohort.PID, p_cohort.P_YEAR, p_cohort.P_Date, First_BM.BM_Year,First_BM.BM_Date FROM p_cohort LEFT JOIN First_BM ON p_cohort.PID==First_BM.PID")
p_oneyear<-p_BM %>% mutate(Diff_year=daydiff(BM_Year,P_Year,BM_Date,P_Date)) %>% filter(Diff_year>=365)
length(unique(p_oneyear$PID))#132
not_one1<- p_BM %>% select(PID) %>% filter(PID %!in% p_oneyear$PID) %>% distinct(PID)#46


###########check with l data#############
#l<-readRDS("C:/Users/jiy_/Dropbox (Personal)/[research]AMR_iy/RDS/l.RDS")
# l<-l %>% rename(PID=Deidentified_Patient_ID, L_Year=Year, L_Date=Date_of_Service)
# saveRDS(l,"l.RDS")
l<-readRDS("l.RDS")
l_cohort<-l %>% subset(PID %in% BM_MRSA_RVS$PID)#include all disease information of cohort (n=178)
length(unique(l_cohort$PID))#178

l_BM<-sqldf("SELECT l_cohort.PID, l_cohort.L_YEAR, l_cohort.L_Date, First_BM.BM_Year,First_BM.BM_Date FROM l_cohort LEFT JOIN First_BM ON l_cohort.PID==First_BM.PID")
l_oneyear<-l_BM %>% mutate(Diff_year=daydiff(BM_Year,L_Year,BM_Date,L_Date)) %>% filter(Diff_year>=365)
length(unique(l_oneyear$PID))#130
not_one2<- l_BM %>% select(PID) %>% filter(PID %!in% l_oneyear$PID) %>% distinct(PID)#48

###########check with m data#############
# m<-readRDS("C:/Users/jiy_/Dropbox (Personal)/[research]AMR_iy/RDS/m.RDS")
# head(m)
# m<-m %>% rename(PID=Deidentified_Patient_ID, M_Year=Year, M_Date=Med_Order_Date_of_Service)
# saveRDS(m,"m.RDS")
m<-readRDS("m.RDS")
m_cohort<-m %>% subset(PID %in% BM_MRSA_RVS$PID)#include all disease information of cohort (n=178)
length(unique(m_cohort$PID))#176

m_BM<-sqldf("SELECT m_cohort.PID, m_cohort.M_YEAR, m_cohort.M_Date, First_BM.BM_Year,First_BM.BM_Date FROM m_cohort LEFT JOIN First_BM ON m_cohort.PID==First_BM.PID")
m_oneyear<-m_BM %>% mutate(Diff_year=daydiff(BM_Year,M_Year,BM_Date,M_Date)) %>% filter(Diff_year>=365)
length(unique(m_oneyear$PID))#57
not_one3<- m_BM %>% select(PID) %>% filter(PID %!in% m_oneyear$PID) %>% distinct(PID)#119


inter1<-intersect(not_one, not_one1)
inter2<-intersect(inter1, not_one2)
inter3<-intersect(inter2, not_one3)
#Total 38 obs should be excluded because they don't have any medical history a prior to first BM diagnosis date.
Inclusion<-First_BM %>% filter(PID %!in% inter3$PID)#140 obs


#########################################################
#Total 140 patients are defined as our study cohort
#########################################################
#Data is stored in "Cohort"
Cohort<-Inclusion
saveRDS(Cohort,"Cohort.RDS")






