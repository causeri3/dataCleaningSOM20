#############GET PACKAGES#####################################################################################################################
install.packages("devtools")
#"Rtools google
#https://github.com/caldwellst/som_mcna_19
devtools::install_github("impact-initiatives/cleaninginspectoR")
devtools::install_github("impact-initiatives/clog")
install.packages("writexl")
install.packages("readxl")
install.packages("reshape2")
install.packages("dplyr")

library(devtools)
library(cleaninginspectoR)
library(writexl)
library(reshape2)
library(dplyr)
library(readxl)
library(clog)
library(tibble)


#add date for export later
today <- Sys.Date()
today<-format(today, format="_%Y_%b_%d")

#################IMPORT DATA SET################################################################################################################
#Import data set as csv-file, exported from the Excel file 
som<-read.csv(file="C:/Users/Vanessa Causemann/Desktop/REACH/Data/MyOutputs/_2020_Oct_06weighted_data.csv", head=T, dec=".", sep=",")
#som<-read.csv(file="~/Desktop/REACH/Data/SOM_MSNA2020_Merged_2020-09-02_v7_clean_data.csv", head=T, dec=".", sep=",")
#som<-read.csv(file="~/Desktop/REACH/Data/SOM_MSNA2020_Merged_2020-09-02_v7_clean-data.csv", head=T, dec=".", sep=",")
#som<-read.csv(file="~/Desktop/REACH/Data/SOM_MSNA2020_2020_Sep_08.csv", head=T, dec=".", sep=",")
#som<-read.csv(file="~/Desktop/REACH/Data/SOM_MSNA2020_Merged_V5_2020_Sep_02_clean_data.csv", head=T, dec=".", sep=",")
#som<-read.csv(file="~/Desktop/REACH/Data/SOM_MSNA2020_Merged_V5_2020_Sep_02_deleted_surveys.csv", head=T, dec=".", sep=",")
#som0<-read.csv(file="~/Desktop/REACH/Data/SOM_MSNA2020_Merged_V5_2020_Sep_02_clean_data.csv", head=T, dec=".", sep=",")
#som1<-read.csv(file="~/Desktop/REACH/Data/SOM_MSNA2020_Merged_V5_2020_Sep_01_clean_data.csv", head=T, dec=".", sep=",")
#som2<-read.csv(file="~/Desktop/REACH/Data/SOM_MSNA2020_Merged_2020-08-30_v4_clean_data_incl_callbacks_2020_Sep_01.csv", head=T, dec=".", sep=",")
#som3<-read.csv(file="~/Desktop/REACH/Data/SOM_MSNA2020_Merged_2020-08-30_v4_clean_data.csv", head=T, dec=".", sep=",")
#som4<-read.xlsx("~/Desktop/REACH/Data/SOM_MSNA2020_Merged_V5_2020_Sep_01_clean_data.xlsx", skipEmptyCols = TRUE, na.strings = "", sheet = 1)
#som3<-read.csv(file="C:/Users/Vanessa Causemann/Desktop/REACH/Data/SOM_MSNA2020_Merged_2020-08-30_v4_clean_data.csv", head=T, dec=".", sep=",")


#create new data frames without NA's in order to sum up later in inconsistencies (run before changing dates)

som_nona<-som
som_nona[is.na(som_nona)]=0

#convert dates
#som$start<-as.Date(som$start-1, origin = '1900-01-01')
#som$end<-as.Date(som$end-1, origin = '1900-01-01')
#06:00 equals 0.25 (a quarter through the day), therefore *24*60 -> minutes
som$today<-as.Date(som$today-1, origin = '1900-01-01')
som$left_aoo<-as.Date(som$left_aoo-1, origin = '1900-01-01')
som$arrived_current<-as.Date(som$arrived_current-1, origin = '1900-01-01')

##################OUTLIERS ETC#################################################################################################################
#data frame inspect shows possible mistakes
inspect<-inspect_all(som)

#add uuid's as column
inspect$uuid<-som$X_uuid[inspect$index]


#export as Excel
write_xlsx(inspect, paste0("C:/Users/Vanessa Causemann/Desktop/REACH/Data/myOutputs/OutliersEtc",today,".xlsx"))

#make new dataframe prep for merge later

blubb<-paste(inspect$issue_type,"-", inspect$variable)
inspect2<-data.frame(inspect$uuid[8:length(blubb)], blubb[8:length(blubb)])
names(inspect2)<-c('uuid','variables')

##################SPELLING MISTAKES###########################################################################################################

#search in all character columns for writing mistakes
char <- unlist(lapply(som, is.character))  
somChar<-som[ , char]
#justify right
#format(somChar, justify = "right")

#check out header
colnames(somChar)

table(somChar[,3])
#respondent_region: capita (Awdal, Bakool, Bay, Galguduud, Sanaag, Saraar, Sool, Togdheer) & small letters, 21 surveys have no region, "hiiraan" & "hiraan"?, "lower_shabelle" & "Lower Shabelle", "Toddheer" & "Togdheer" & "togdheer", "w/gabeed" &"w/galbeed" &"waqooyi galbeed" & "waqoyi_galbeed"&"woqooyi_galbeed"

table(somChar[,34])
# market_transport: 3 other & 188 empty rows, "Car" &"car"

table(somChar[,35])
which(somChar[,35]==" ")
# market_transport_other: 9 have space, row numbers: 327  3488  4124  6504  7345  7498  9713 10996 12247
# 13560 rows have no entry

table(somChar[,53])
which(somChar[,53]==" ")
# attend_covid19_other: 10 have space, row numbers: 3243  4615  4887  4985  8551  8701  9956 10390 10702 12294
# 13545 rows have no entry

table(somChar[,55])
#remote_edu_via_other: Somali answers: "Aaan dhiira gelina un","ma gaarin heer fiican", "School ayey aadayeen"," ma gaarin heer fiican", "Waala fasaxay wax cashar ah a may qaadan "

table(somChar[,59])
which(somChar[,59]==" ")
#school_barrier_girls_other: 
#25 have space, row numbers: 1968  3469  3901  4114  4190  4658  4725  5686  6065  6284  6589  6973  7458  8453  9635  9672 10804 10846 11098 11105 11489 12270 12320 12619 12906
#Somali: "reer guuraa ayaan kanimi abaarey agteeda", "Waxba iima bartaan ", "Waxbarasho kama jirto halkaan"
#one "no school", 10773 "not school", one "#REF!"?

table(somChar[,61])
which(somChar[,61]==" ")
#cash_education_other: 24 have space, row numbers: 3226  3935  3974  4002  4114  4515  6065  6360  6715  6975  7256  7553  7974  8597  8738  8855  8937  8993  9314  9862 10392 10657 10707 10846

table(somChar[,89])
#water_source: 2013x"public_tap" & 1x"Public_tap"

table(somChar[,90])
which(somChar[,90]==" ")
#water_source_other: 1 space in row: 6462, Somali or regions?: 1x"Waan gataa", 1 x"Berkad", 2x"Barkad", 1x "Baarak from neibourhood", 1x"Branch riyaq "

table(somChar[,95])
#of_water_other: 1x"We have enough water "(space in the end), 1x"We have enough water" (no space), 4x"doesn_t_have_issue", 3x "does_not_have _issues "
#Somali: "Biyo la'aan ma jirto", "Lama qabsan karno "

table(somChar[,96])
#sanitation_facility : 2939 x "pit_latrine_without" 1x "pit_latrine_withot"

table(somChar[,101])
#sanitation_barriers_other: 1x "no_problem", 8x "No_problem", 9x "No_toilet", 1x"No_toilets", 1x"No-toilets"

table(somChar[,103])
#sanitation_coping_other: Somali:"Daarufa jiira awgeed ayaan ulaaqaabsaday "

table(somChar[,106])
#handwashing_facility_other: 13504x "Caag aan isticmaalaa", 1x "ma lihi", 9x "Caag", 37x "Caagado", 1x"Caagag", 1x"Caagag "(with space in the end)

table(somChar[,111])
#hands_other: 5x "Before praying", 35x "Before_praying", 2x "prayer time", 13x "Praying time " (space in the end)

table(somChar[,112])
#shelter_type: 4895x "buul", 1x "Buul", 2x "Buul "(with space), 1988x "cgi", 5x "cgi mud", 1461x"mud" 1559x "stone", 2x "Stone", 22x "Stone "(with space in the end)

table(somChar[,116])
#shelter_damage_other: 9x with space, 1x "waa Buul"  

table(somChar[,118])
#unable_repair_other: 1x "Am not owner ", 1x "I am not owner ", 1x "The house is not mine so can not repair because the owner do not  allow me to repair "

table(somChar[,120])
#hh_cook: 5710 "kitchen", 7x "Kitchen"

table(somChar[,122])
#occupancy_arrangement: 2490x "hosted", 2x "Hosted", 2x"hosted "(with space in the end), 1x "Hosted " (with space in the end), 2166"rented", 1x"Rented", 8x"rented "(with space in the end)

table(somChar[,126])
#hlp_problems_other 7x with space

table(somChar[,132])
#hh_restrictions: I am not sure if this is a mistake, many yes_1, yes_2, yes_3 and combinations of those

table(somChar[,158])
#information_channel_other: 11 with spaces

table(somChar[,173])
#un_stop_other: 1x "Don't know", 5x "Don't know " (with space in the end), 1x "I don't know", 1x"Insecurity purpose", 1x"Where there insecurity area.", 1x"Where there is insecurity "

table(somChar[,178])
#action_to_prevent_other: 3x spaces

colnames(somChar[1])

###########INCONSISTENCIES###########################################################################################################
#formulate inconsistencies, save index, uuids and reason

incon0<-((som$end-som$start)*24*60)<15
i0<-which(incon0)
u0<-som$X_uuid[which(incon0)]
v0<-rep("survey took under 15 min", length(i0))
uuid<-u0
index<-i0
variables<-v0

###DROPPED
#incon0_0<-((som$end-som$start)*24*60)>45
#i0_0<-which(incon0_0)
#u0_0<-som$X_uuid[which(incon0_0)]
#v0_0<-rep("survey took over 45 min", length(i0_0))
#uuid<-c(uuid, u0_0)
#index<-c(index, i0_0)
#variables<-(variables, v0_0)

incon0_1<-((som$end-som$start)*24*60)>90
i0_1<-which(incon0_1)
u0_1<-som$X_uuid[which(incon0_1)]
v0_1<-rep("survey took over 90 min", length(i0_1))
uuid<-c(uuid, u0_1)
index<-c(index, i0_1)
variables<-c(variables, v0_1)

###DROPPED
#incon1<-som$hh_size == som$total_hh
#i1<-which(!incon1)
#u1<-som$X_uuid[which(!incon1)]
#v1<-rep("hh_size doesn't equal total_hh", length(i1))
#uuid<-u1
#index<-i1
#variables<-v1

###DROPPED
hh<-(som$males_0m_5y+som$males_6_12+som$males_13_15+som$males_16_17+som$males_18_40+som$males_41_59+som$males_60_over+som$females_0m_5y+som$females_6_12+som$females_13_15+som$females_16_17+som$females_18_40+som$females_41_59+som$females_60_over)
#incon2<-hh==som$hh_size
#i2<-which(!incon2)
#u2<-som$X_uuid[which(!incon2)]
#v2<-rep("sum of males and females does not match total_hh", length(i2))
#uuid<-c(uuid,u2)
#index<-c(index,i2)
#variables<-c(variables,v2)

hhChildren<-(som$males_0m_5y+som$males_6_12+som$males_13_15+som$males_16_17+som$females_0m_5y+som$females_6_12+som$females_13_15+som$females_16_17)
incon3<-hhChildren==som$hh_children
i3<-which(!incon3)
u3<-som$X_uuid[which(!incon3)]
v3<-rep("sum of males and females under 18 does not match hh_children", length(i3))
uuid<-c(uuid,u3)
index<-c(index,i3)
variables<-c(variables,v3)

incon4<-som$females_6_12>=som_nona$females_6_12me
i4<-which(!incon4)
u4<-som$X_uuid[which(!incon4)]
v4<-rep("more females_6_12me (health issue) than in females_6_12", length(i4))
uuid<-c(uuid,u4)
index<-c(index,i4)
variables<-c(variables,v4)

incon5<-som$males_6_12>=som_nona$males_6_12me
i5<-which(!incon5)
u5<-som$X_uuid[which(!incon5)]
v5<-rep("more males_6_12me (health issue) than in males_6_12", length(i5))
uuid<-c(uuid,u5)
index<-c(index,i5)
variables<-c(variables,v5)

incon6<-som$females_13_15>=som_nona$females_13_15e
i6<-which(!incon6)
u6<-som$X_uuid[which(!incon6)]
v6<-rep("more females_13_15e (health issue) than in females_13_15", length(i6))
uuid<-c(uuid,u6)
index<-c(index,i6)
variables<-c(variables,v6)

incon7<-som$males_13_15>=som_nona$males_13_15e
i7<-which(!incon7)
u7<-som$X_uuid[which(!incon7)]
v7<-rep("more males_13_15e (health issue) than in males__13_15", length(i7))
uuid<-c(uuid,u7)
index<-c(index,i7)
variables<-c(variables,v7)

incon8<-som$females_16_17>=som_nona$females_16_17e
i8<-which(!incon8)
u8<-som$X_uuid[which(!incon8)]
v8<-rep("more females_16_17e (health issue) than in females_16_17", length(i8))
uuid<-c(uuid,u8)
index<-c(index,i8)
variables<-c(variables,v8)

incon9<-som$males_16_17>=som_nona$males_16_17e
i9<-which(!incon9)
u9<-som$X_uuid[which(!incon9)]
v9<-rep("more males_16_17e (health issue) than in males_16_17", length(i9))
uuid<-c(uuid,u9)
index<-c(index,i9)
variables<-c(variables,v9)

incon10<-som$females_18_40>=som_nona$females_18_40e
i10<-which(!incon10)
u10<-som$X_uuid[which(!incon10)]
v10<-rep("more females_18_40e (health issue) than in females_18_40", length(i10))
uuid<-c(uuid,u10)
index<-c(index,i10)
variables<-c(variables,v10)

incon11<-som$males_18_40>=som_nona$males_18_40e
i11<-which(!incon11)
u11<-som$X_uuid[which(!incon11)]
v11<-rep("more males_18_40e (health issue) than in males_18_40", length(i11))
uuid<-c(uuid,u11)
index<-c(index,i11)
variables<-c(variables,v11)

incon12<-som$females_41_59>=som_nona$females_41_59e
i12<-which(!incon12)
u12<-som$X_uuid[which(!incon12)]
v12<-rep("more females_41_59e (health issue) than in females_41_59", length(i12))
uuid<-c(uuid,u12)
index<-c(index,i12)
variables<-c(variables,v12)

incon13<-som$males_41_59>=som_nona$males_41_59e
i13<-which(!incon13)
u13<-som$X_uuid[which(!incon13)]
v13<-rep("more males_41_59e (health issue) than in males_41_59", length(i13))
uuid<-c(uuid,u13)
index<-c(index,i13)
variables<-c(variables,v13)

incon14<-som$females_60_over>=som_nona$females_60_overe
i14<-which(!incon14)
u14<-som$X_uuid[which(!incon14)]
v14<-rep("more females_60_overe (health issue) than in females_60_over", length(i14))
uuid<-c(uuid,u14)
index<-c(index,i14)
variables<-c(variables,v14)

incon15<-som$males_60_over>=som_nona$males_60_overe
i15<-which(!incon15)
u15<-som$X_uuid[which(!incon15)]
v15<-rep("more males_60_overe (health issue) than in males_60_over", length(i15))
uuid<-c(uuid,u15)
index<-c(index,i15)
variables<-c(variables,v15)

##########SEE IF ISSUE IS DUE TO hhh_count_check###########################################################################

hhCheck1 <-sort(index)
hhCheck2 <- which(som$hhh_count_check=="no")

for (i in 1:length(hhCheck2))
{
  print(which(hhCheck1[i]==hhCheck2))
}

#only two of the  hhh_count_check entries have "no" as answer for (145,169 in hhCheck2) 5007, 7002 in data set, uuid: 69c54c62-3367-47b8-b08c-8cbc16a85658, acaa7e57-1300-419b-bca6-bc336c6043f2
# 10 in version 30_v3

#########CONTINUE INCONSISTENCIES#####################################################################################################
###DROPPED
#incon16<-(som$avg_income+som$avg_debt)>=som$food_expenditure
#i16<-which(!incon16)
#u16<-som$X_uuid[which(!incon16)]
#v16<-rep("avg_income + avg_dept is smaller than food_expenditures", length(i16))
#uuid<-c(uuid,u16)
#index<-c(index,i16)
#variables<-c(variables,v16)

incon16_1<-som$income_src.none==1 & som$income_src.cash_crop_farming==1
i16_1<-which(incon16_1)
u16_1<-som$X_uuid[which(incon16_1)]
v16_1<-rep("income_src.none = 1 and income_src.cash_crop_farming = 1", length(i16_1))
uuid<-c(uuid,u16_1)
index<-c(index,i16_1)
variables<-c(variables,v16_1)

incon16_2<-som$income_src.none==1 & som$income_src.cash_fishing==1
i16_2<-which(incon16_2)
u16_2<-som$X_uuid[which(incon16_2)]
v16_2<-rep("income_src.none = 1 and income_src.cash_fishing = 1", length(i16_2))
uuid<-c(uuid,u16_2)
index<-c(index,i16_2)
variables<-c(variables,v16_2)

incon16_3<-som$income_src.none==1 & som$income_src.daily_labour==1
i16_3<-which(incon16_3)
u16_3<-som$X_uuid[which(incon16_3)]
v16_3<-rep("income_src.none = 1 and income_src.daily_labour = 1", length(i16_3))
uuid<-c(uuid,u16_3)
index<-c(index,i16_3)
variables<-c(variables,v16_3)

incon16_4<-som$income_src.none==1 & som$income_src.livestock_production==1
i16_4<-which(incon16_4)
u16_4<-som$X_uuid[which(incon16_4)]
v16_4<-rep("income_src.none = 1 and income_src.livestock_production = 1", length(i16_4))
uuid<-c(uuid,u16_4)
index<-c(index,i16_4)
variables<-c(variables,v16_4)

incon16_5<-som$income_src.none==1 & som$income_src.subsistence_farming_or_fishing==1
i16_5<-which(incon16_5)
u16_5<-som$X_uuid[which(incon16_5)]
v16_5<-rep("income_src.none = 1 and income_src.subsistence_farming_or_fishing = 1", length(i16_5))
uuid<-c(uuid,u16_5)
index<-c(index,i16_5)
variables<-c(variables,v16_5)

incon16_6<-som$income_src.none==1 & som$income_src.contracted_job==1
i16_6<-which(incon16_6)
u16_6<-som$X_uuid[which(incon16_6)]
v16_6<-rep("income_src.none = 1 and income_src.contracted_job = 1", length(i16_6))
uuid<-c(uuid,u16_6)
index<-c(index,i16_6)
variables<-c(variables,v16_6)

incon16_7<-som$income_src.none==1 & som$income_src.remittances==1
i16_7<-which(incon16_7)
u16_7<-som$X_uuid[which(incon16_7)]
v16_7<-rep("income_src.none = 1 and income_src.remittances = 1", length(i16_7))
uuid<-c(uuid,u16_7)
index<-c(index,i16_7)
variables<-c(variables,v16_7)

incon16_8<-som$income_src.none==1 & som$income_src.humanitarian_assistance==1
i16_8<-which(incon16_8)
u16_8<-som$X_uuid[which(incon16_8)]
v16_8<-rep("income_src.none = 1 and income_src.humanitarian_assistance = 1", length(i16_8))
uuid<-c(uuid,u16_8)
index<-c(index,i16_8)
variables<-c(variables,v16_8)

incon16_9<-som$income_src.none==1 & som$income_src.sale_of_humanitarian_assistance==1
i16_9<-which(incon16_9)
u16_9<-som$X_uuid[which(incon16_9)]
v16_9<-rep("income_src.none = 1 and income_src.sale_of_humanitarian_assistance = 1", length(i16_9))
uuid<-c(uuid,u16_9)
index<-c(index,i16_9)
variables<-c(variables,v16_9)

incon16_10<-som$income_src.none==1 & som$income_src.rent_of_land==1
i16_10<-which(incon16_10)
u16_10<-som$X_uuid[which(incon16_10)]
v16_10<-rep("income_src.none = 1 and income_src.rent_of_land = 1", length(i16_10))
uuid<-c(uuid,u16_10)
index<-c(index,i16_10)
variables<-c(variables,v16_10)

incon16_11<-som$income_src.none==1 & som$income_src.business==1
i16_11<-which(incon16_11)
u16_11<-som$X_uuid[which(incon16_11)]
v16_11<-rep("income_src.none = 1 and income_src.business = 1", length(i16_11))
uuid<-c(uuid,u16_11)
index<-c(index,i16_11)
variables<-c(variables,v16_11)

###UPDATED, see below
#incon16_12<-som$income_src.none==1 & som$avg_income>0
#i16_12<-which(incon16_12)
#u16_12<-som$X_uuid[which(incon16_12)]
#v16_12<-rep("income_src.none = 1, avg_income bigger than 0", length(i16_12))
#uuid<-c(uuid,u16_12)
#index<-c(index,i16_12)
#variables<-c(variables,v16_12)

###UPDATED
#people might see debts as income, a friend lending money could be income
incon16_12<-som$income_src.none==1 & som$avg_debt-som$avg_income<0
i16_12<-which(incon16_12)
u16_12<-som$X_uuid[which(incon16_12)]
v16_12<-rep("income_src.none = 1, avg_debt minus avg_income smaller than 0", length(i16_12))
uuid<-c(uuid,u16_12)
index<-c(index,i16_12)
variables<-c(variables,v16_12)

###DROPPED
#incon16_13<-(som$hh_members_income>0 & som$income_src.none==1)
#i16_13<-which(incon16_13)
#u16_13<-som$X_uuid[which(incon16_13)]
#v16_13<-rep("income_src.none=1, but hh_members_income more than 0", length(i16_13))
#uuid<-c(uuid,u16_13)
#index<-c(index,i16_13)
#variables<-c(variables,v16_13)

#maybe debt is counted as income
#incon16_13<-((som$avg_debt-som$hh_members_income<0) & som$income_src.none==1)
#i16_13<-which(incon16_13)
#u16_13<-som$X_uuid[which(incon16_13)]
#v16_13<-rep("income_src.none=1, but hh_members_income more than 0", length(i16_13))
#uuid<-c(uuid,u16_13)
#index<-c(index,i16_13)
#variables<-c(variables,v16_13)

incon16_14<-som$hh_members_new_unemployed<=(som$males_18_40+som$males_41_59+som$males_60_over+som$females_18_40+som$females_41_59+som$females_60_over)
i16_14<-which(!incon16_14)
u16_14<-som$X_uuid[which(!incon16_14)]
v16_14<-rep("more hh_members_new_unemployed than adult hh members", length(i16_14))
uuid<-c(uuid,u16_14)
index<-c(index,i16_14)
variables<-c(variables,v16_14)

#incon16_15<-som$avg_income[som$income_src.humanitarian_assistance==1] >= 300
#i16_15<-which(!incon16_15)
#u16_15<-som$X_uuid[which(!incon16_15)]
#v16_15<-rep("hh with humanitarian assistance have less than 300$ income", length(i16_15))
#uuid<-c(uuid,u16_15)
#index<-c(index,i16_15)
#variables<-c(variables,v16_15)
######dunno the threshold, 300$ flags 2105 surveys

#incon16_16<-som$avg_income[som$hh_members_income==0&&som$hh_members_new_unemployed==0&&som$income_src.humanitarian_assistance==0]==0
#i16_16<-which(!incon16_16)
#u16_16<-som$X_uuid[which(!incon16_16)]
#v16_16<-rep("avg_income bigger than 0, even though neither income_src.humanitarian_assistance nor hh_members_income nor hh_members_new_unemployed", length(i16_16))
#uuid<-c(uuid,u16_16)
#index<-c(index,i16_16)
#variables<-c(variables,v16_16)
#####flags almost all surveys, doesn't make sense, income sources could be remittance or other without being employed

#incon17<-(som$education_high+som$education_tertiary+som$education_primary+som$education_middle+som$education_vocational+som$education_none)<=(som$males_18_40+som$males_41_59+som$males_60_over+som$females_18_40+som$females_41_59+som$females_60_over)
#different levels might be counted for one person, instead only taken highest level and non-educated

###DROPPED
#incon17<-(som$education_tertiary+som$education_none)<=hh
#i17<-which(!incon17)
#u17<-som$X_uuid[which(!incon17)]
#v17<-rep("education_tertiary+education_none is bigger than all hh members over 18", length(i17))
#uuid<-c(uuid,u17)
#index<-c(index,i17)
#variables<-c(variables,v17)

###DROPPED
#incon17<-(som$education_tertiary+som$education_none)<=(som$males_18_40+som$males_41_59+som$males_60_over+som$females_18_40+som$females_41_59+som$females_60_over)
#i17<-which(!incon17)
#u17<-som$X_uuid[which(!incon17)]
#v17<-rep("education_tertiary+education_none is bigger than all hh members over 18", length(i17))
#uuid<-c(uuid,u17)
#index<-c(index,i17)
#variables<-c(variables,v17)

###DROPPED (sum variable up again)
#incon18<-som_nona$enrolled_total==(som_nona$enrolled_boys_6_12+som_nona$enrolled_girls_6_12+som_nona$enrolled_boys_13_17+som_nona$enrolled_girls_13_17)
#i18<-which(!incon18)
#u18<-som$X_uuid[which(!incon18)]
#v18<-rep("enrolled_total does not equal sum of enrolled children", length(i18))
#uuid<-c(uuid,u18)
#index<-c(index,i18)
#variables<-c(variables,v18)

###DROPPED
#incon19<-(som$covid_boys_6_12+som$covid_girls_6_12+som$covid_boys_13_17+som$covid_girls_13_17)==(som$enrolled_boys_6_12+som$enrolled_girls_6_12+som$enrolled_boys_13_17+som$enrolled_girls_13_17+som$enrolled_boys_6_12e+som$enrolled_girls_6_12e+som$enrolled_boys_13_17e+som$enrolled_girls_13_17e)
#i19<-which(!incon19)
#u19<-som$X_uuid[which(!incon19)]
#v19<-rep("sum of covid enrolled children is not equal to sum of enrolled children", length(i19))
#uuid<-c(uuid,u19)
#index<-c(index,i19)
#variables<-c(variables,v19)

###DROPPED (sum variable up again)
#incon19_1<-(som$covid_boys_6_12+som$covid_girls_6_12+som$covid_boys_13_17+som$covid_girls_13_17)==som$covid_enrollement
#i19_1<-which(!incon19_1)
#u19_1<-som$X_uuid[which(!incon19_1)]
#v19_1<-rep("covid_enrollement is not equal to sum of covid children", length(i19_1))
#uuid<-c(uuid,u19_1)
#index<-c(index,i19_1)
#variables<-c(variables,v19_1)

###DROPPED
#incon20<-(som$males_6_12+som$males_13_15+som$males_16_17+som$females_6_12+som$females_13_15+som$females_16_17)>=(som_nona$enrolled_boys_6_12e+som_nona$enrolled_girls_6_12e+som_nona$enrolled_boys_13_17e+som_nona$enrolled_girls_13_17e+som_nona$enrolled_boys_6_12e+som_nona$enrolled_girls_6_12e+som_nona$enrolled_boys_13_17e+som_nona$enrolled_girls_13_17e)
#i20<-which(!incon20)
#u20<-som$X_uuid[which(!incon20)]
#v20<-rep("sum of children in hh between 6-17y is smaller than sum of enrolled and dropped out children", length(i20))
#uuid<-c(uuid,u20)
#index<-c(index,i20)
#variables<-c(variables,v20)

###DROPPED (sum variable up again)
#incon21<-som_nona$enrollement_note<=som_nona$enrolled_total
#i21<-which(!incon21)
#u21<-som$X_uuid[which(!incon21)]
#v21<-rep("enrollement_note is bigger than enrolled_total", length(i21))
#uuid<-c(uuid,u21)
#index<-c(index,i21)
#variables<-c(variables,v21)

incon22<-(som$males_0m_5y+som$males_6_12+som$males_13_15+som$males_16_17+som$females_0m_5y+som$females_6_12+som$females_13_15+som$females_16_17)>=(som_nona$home_boys_6_12+som_nona$home_girls_6_12+som_nona$home_boys_13_17+som_nona$home_girls_13_17)
i22<-which(!incon22)
u22<-som$X_uuid[which(!incon22)]
v22<-rep("sum of children in hh is smaller than sum of home children", length(i22))
uuid<-c(uuid,u22)
index<-c(index,i22)
variables<-c(variables,v22)

incon23<-som_nona$rooms_total==som_nona$sum_rooms
i23<-which(!incon23)
u23<-som$X_uuid[which(!incon23)]
v23<-rep("sum_rooms does not equal rooms_total", length(i23))
uuid<-c(uuid,u23)
index<-c(index,i23)
variables<-c(variables,v23)

incon24<-som_nona$sum_rooms == (som_nona$bedrooms+som_nona$living_rooms+som_nona$kitchens+som_nona$toilets)
i24<-which(!incon24)
u24<-som$X_uuid[which(!incon24)]
v24<-rep("sum_rooms does not equal all rooms added", length(i24))
uuid<-c(uuid,u24)
index<-c(index,i24)
variables<-c(variables,v24)

incon25<-som_nona$rooms_total == (som_nona$bedrooms+som_nona$living_rooms+som_nona$kitchens+som_nona$toilets)
i25<-which(!incon25)
u25<-som$X_uuid[which(!incon25)]
v25<-rep("rooms_total does not equal rooms added", length(i25))
uuid<-c(uuid,u25)
index<-c(index,i25)
variables<-c(variables,v25)

###DROPPED
#incon26<-som$child_labor_notes == (som$boys_labor+som$girls_labor)
#i26<-which(!incon26)
#u26<-som$X_uuid[which(!incon26)]
#v26<-rep("child_labor_notes does not equal boys_labor+girls_labor", length(i26))
#uuid<-c(uuid,u26)
#index<-c(index,i26)
#variables<-c(variables,v26)

incon26_1<-(som$males_0m_5y+som$females_6_12 +som$males_13_15+ som$males_16_17) >= som_nona$boys_labor
i26_1<-which(!incon26_1)
u26_1<-som$X_uuid[which(!incon26_1)]
v26_1<-rep("all boys in hh are less than boys_labor", length(i26_1))
uuid<-c(uuid,u26_1)
index<-c(index,i26_1)
variables<-c(variables,v26_1)

incon26_2<-(som$females_0m_5y+som$females_6_12 +som$females_13_15+ som$females_16_17) >= som_nona$girls_labor
i26_2<-which(!incon26_2)
u26_2<-som$X_uuid[which(!incon26_2)]
v26_2<-rep("all girls in hh are less than girls_labor", length(i26_2))
uuid<-c(uuid,u26_2)
index<-c(index,i26_2)
variables<-c(variables,v26_2)

incon27<-som$breadwinner[som$hh_size==1] == som$household_expenditure[som$hh_size==1]
u27<- rep(0, times=length(which(!incon27)))
i27<- rep(0, times=length(which(!incon27)))
for (i in 1:length(which(!incon27)))
{u27[i]<-som$X_uuid[som$hh_size==1][which(!incon27)[i]]
i27[i]<-which(som$X_uuid==u27[i])
}
v27<-rep("hh=1, breadwinner different age as household_expenditure", length(which(!incon27)))
uuid<-c(uuid,u27)
index<-c(index,i27)
variables<-c(variables,v27)

incon28<-(som_nona$chronic_illness_hh_members.female_13+som_nona$chronic_illness_hh_members.male_13+som_nona$chronic_illness_hh_members.female_14_17+som_nona$chronic_illness_hh_members.male_14_17)<=hhChildren
i28<-which(!incon28)
u28<-som$X_uuid[which(!incon28)]
v28<-rep("more chronic ill children than hh children", length(i28))
uuid<-c(uuid,u28)
index<-c(index,i28)
variables<-c(variables,v28)

incon29<-(som_nona$chronic_illness_hh_members.adult_female+som_nona$chronic_illness_hh_members.adult_male+som_nona$chronic_illness_hh_members.eldery_female+som_nona$chronic_illness_hh_members.eldery_male)<=(som$males_18_40+som$males_41_59+som$males_60_over+som$females_18_40+som$females_41_59+som$females_60_over)
i29<-which(!incon29)
u29<-som$X_uuid[which(!incon29)]
v29<-rep("more chronic ill adults than hh adults", length(i29))
uuid<-c(uuid,u29)
index<-c(index,i29)
variables<-c(variables,v29)

###DROPPED
#incon30<-(som$pregnancy=="yes" & (som$females_16_17+som$females_13_15+som$females_18_40+som$females_41_59)==0)
#i30<-which(incon30)
#u30<-som$X_uuid[which(incon30)]
#v30<-rep("pregnancy=yes, but no females between 13&59 years in hh", length(i30))
#uuid<-c(uuid,u30)
#index<-c(index,i30)
#variables<-c(variables,v30)

#counting girls from 6-12 as well
#incon30<-(som$pregnancy=="yes" & (som$females_6_12+som$females_16_17+som$females_13_15+som$females_18_40+som$females_41_59)==0)
#i30<-which(incon30)
#u30<-som$X_uuid[which(incon30)]
#v30<-rep("pregnancy=yes, but no females between 13&59 years in hh", length(i30))
#uuid<-c(uuid,u30)
#index<-c(index,i30)
#variables<-c(variables,v30)

###DROPPED
#incon31<-som$read_write=="no" & (som$remote_edu_via.reading==1 | som$remote_edu_via.online_classes==1 | som$remote_edu_via.online==1 | som$remote_edu_via.school==1)
#i31<-which(incon31)
#u31<-som$X_uuid[which(incon31)]
#v31<-rep("read_write=no, doesn't fit to remote_edu_via answer", length(i31))
#uuid<-c(uuid,u31)
#index<-c(index,i31)
#variables<-c(variables,v31)


#incon32<-som$water_barrier.waterpoints_far==1 & (som$water_source_time== "water_premises" | som$water_source_time=="less_5_min" | som$water_source_time=="between_and_15_min") & som$of_water.doesn_t_have_issue==1
#i32<-which(incon32)
#u32<-som$X_uuid[which(incon32)]
#v32<-rep("water_barrier.waterpoints_far=1, but water_source_time under 15min", length(i32))
#uuid<-c(uuid,u32)
#index<-c(index,i32)
#variables<-c(variables,v32)

###DROPPED
#incon32<-som$water_barrier.waterpoints_far==1 & (som$water_source_time== "water_premises" | som$water_source_time=="less_5_min" | som$water_source_time=="between_and_15_min")
#i32<-which(incon32)
#u32<-som$X_uuid[which(incon32)]
#v32<-rep("water_barrier.waterpoints_far=1, but water_source_time under 15min", length(i32))
#uuid<-c(uuid,u32)
#index<-c(index,i32)
#variables<-c(variables,v32)

###DROPPED
#incon33<-som$difference_arrived_today_days>90 & som$employ_loss_why.displacement==1
#i33<-which(incon33)
#u33<-som$X_uuid[which(incon33)]
#v33<-rep("employ_loss_why.displacement=1, but difference_arrived_today_days over 90", length(i33))
#uuid<-c(uuid,u33)
#index<-c(index,i33)
#variables<-c(variables,v33)

incon34<-som$hosting_idp=="yes" & som$hosting_idp_number==0
i34<-which(incon34)
u34<-som$X_uuid[which(incon34)]
v34<-rep("hosting_idp=yes and hosting_idp_number=0", length(i34))
uuid<-c(uuid,u34)
index<-c(index,i34)
variables<-c(variables,v34)

incon35<-(som$difference_arrived_left_days<=0) & (som$displaced_locs>1)
i35<-which(incon35)
u35<-som$X_uuid[which(incon35)]
v35<-rep("difference_arrived_left_days=0 or less and more than one displaced_locs", length(i35))
uuid<-c(uuid,u35)
index<-c(index,i35)
variables<-c(variables,v35)

incon36<-som$disp_why1=="none" & (som$disp_why2!="none" | som$disp_why2!="")
i36<-which(incon36)
u36<-som$X_uuid[which(incon36)]
v36<-rep("disp_why1=none, but reason for disp_why2", length(i36))
uuid<-c(uuid,u36)
index<-c(index,i36)
variables<-c(variables,v36)

###DROPPED after converting som$sanitation_facility to ""
#incon37<-(som$sanitation_facility=="none_of" | som$latrine_features.walls_==1) & (som$toilets>0)
#i37<-which(incon37)
#u37<-som$X_uuid[which(incon37)]
#v37<-rep("sanitation_facility=none_of or latrine_features.walls_=1, but toilets in hh", length(i37))
#uuid<-c(uuid,u37)
#index<-c(index,i37)
#variables<-c(variables,v37)
#flags 2k, in survey toilets/bathrooms, might just be a shower or sink

incon38<- som$latrine_features.lock==1 & (som$latrine_features.door==0 | som$latrine_features.walls_==0)
i38<-which(incon38)
u38<-som$X_uuid[which(incon38)]
v38<-rep("latrine_features.lock=1, but latrine_features.door or latrine_features.walls_ isn't", length(i38))
uuid<-c(uuid,u38)
index<-c(index,i38)
variables<-c(variables,v38)

incon39<- (som$latrine_features.door==1 & som$latrine_features.walls_==0)
i39<-which(incon39)
u39<-som$X_uuid[which(incon39)]
v39<-rep("latrine_features.door=1, but latrine_features.walls_=0", length(i39))
uuid<-c(uuid,u39)
index<-c(index,i39)
variables<-c(variables,v39)

##DROPPED
#incon40<- (som$latrine_features.lock==1 & som$sanitation_barriers.sanitation_etc==1)
#i40<-which(incon40)
#u40<-som$X_uuid[which(incon40)]
#v40<-rep("latrine_features.lock=1, but stated no privacy: sanitation_barriers.sanitation_etc=1", length(i40))
#uuid<-c(uuid,u40)
#index<-c(index,i40)
#variables<-c(variables,v40)

###DROPPED
#incon41<- (som$shelter_type=="buul" & som$sum_rooms>5)
#i41<-which(incon41)
#u41<-som$X_uuid[which(incon41)]
#v41<-rep("shelter_type==buul, but sum_rooms over 5", length(i41))
#uuid<-c(uuid,u41)
#index<-c(index,i41)
#variables<-c(variables,v41)

###DROPPED
#incon42<-(som$written_documentation=="yes" & som$hlp_problems.lack_documents==1)
#i42<-which(incon42)
#u42<-som$X_uuid[which(incon42)]
#v42<-rep("written_documentation=yes & hlp_problems.lack_documents=1", length(i42))
#uuid<-c(uuid,u42)
#index<-c(index,i42)
#variables<-c(variables,v42)

###DROPPED
#incon43<-(som$health_access=="no" & som$barriers_health.no_issues==1)
#i43<-which(incon43)
#u43<-som$X_uuid[which(incon43)]
#v43<-rep("health_access=no, but barriers_health.no_issues=1", length(i43))
#uuid<-c(uuid,u43)
#index<-c(index,i43)
#variables<-c(variables,v43)
#could happen if no money or insurance....?

###DROPPED
#incon44<-(som$medical_services=="no" & som$barriers_health.no_issues==1)
#i44<-which(incon44)
#u44<-som$X_uuid[which(incon44)]
#v44<-rep("medical_services=no and barriers_health.no_issues=1", length(i44))
#uuid<-c(uuid,u44)
#index<-c(index,i44)
#variables<-c(variables,v44)

#without the walking time and mobile health I get about 350 more
#incon44<-(som$medical_services=="no" & (som$health_time=="less15" | som$health_time=="16_30"| som$health_time=="31_60"| som$health_mobile=="yes") & som$barriers_health.no_issues==1)
#i44<-which(incon44)
#u44<-som$X_uuid[which(incon44)]
#v44<-rep("medical_services=no AND barriers_health.no_issues==1, but 0-60min to health_time to facility or health_mobile=yes", length(i44))
#uuid<-c(uuid,u44)
#index<-c(index,i44)
#variables<-c(variables,v44)

###DROPPED
#incon45<-som$information_channel=="no_need" & som$aid_barriers.lack_information==1
#i45<-which(incon45)
#u45<-som$X_uuid[which(incon45)]
#v45<-rep("information_channel=no_need & aid_barriers.lack_information=1", length(i45))
#uuid<-c(uuid,u45)
#index<-c(index,i45)
#variables<-c(variables,v45)

incon46<- (som$un_continue!="none") & (som$un_continue!="") & (som$un_continue==som$un_stop)
i46<-which(incon46)
u46<-som$X_uuid[which(incon46)]
v46<-rep("un_continue=un_stop", length(i46))
uuid<-c(uuid,u46)
index<-c(index,i46)
variables<-c(variables,v46)

incon47<-som$factors_aid.Disability._Person_living_with_a_disability==1 & (som$dis_seeing=="no"&som$dis_communi=="no"&som$dis_hearing=="no"&som$dis_walk=="no"&som$dis_concentr=="no"&som$dis_selfcare=="no")
i47<-which(incon47)
u47<-som$X_uuid[which(incon47)]
v47<-rep("factors_aid.Disability._Person_living_with_a_disability=1, but no in all disabalitily types (hearing, seeing etc", length(i47))
uuid<-c(uuid,u47)
index<-c(index,i47)
variables<-c(variables,v47)

###DROPPED
#incon48<-som$enough_food=="yes"&(((som$avg_income-som$on_water-som$note_acccomodation)>=som$food_expenditure*1.25)&((som$avg_income-som$on_water-som$note_acccomodation)>=600))
#i48<-which(incon48)
#u48<-som$X_uuid[which(incon48)]
#v48<-rep("enough_food=yes, but income too high and on_water&note_accomodation&food_expenditure not high enough", length(i48))
#uuid<-c(uuid,u48)
#index<-c(index,i48)
#variables<-c(variables,v48)


incon49<-(hhChildren+som$females_18_40+som$males_18_40)==0 & som$factors_aid.30.==1
i49<-which(incon49)
u49<-som$X_uuid[which(incon49)]
v49<-rep("factors_aid.30.=1, but no hh member under 40 listed", length(i49))
uuid<-c(uuid,u49)
index<-c(index,i49)
variables<-c(variables,v49)

###DROPPED
#incon50<-(som$females_60_over+som$males_60_over)==0 & som$factors_aid.60.==1
#i50<-which(incon50)
#u50<-som$X_uuid[which(incon50)]
#v50<-rep("factors_aid.60.=1, but no hh memeber over 60 listed", length(i50))
#uuid<-c(uuid,u50)
#index<-c(index,i50)
#variables<-c(variables,v50)

###DROPPED
#incon51<-(som$females_60_over+som$males_60_over)==0 & som$less_food>0
#i51<-which(incon51)
#u51<-som$X_uuid[which(incon51)]
#v51<-rep("less_food to elderly, but no elderly in hh", length(i51))
#uuid<-c(uuid,u51)
#index<-c(index,i51)
#variables<-c(variables,v51)

###DROPPED
#incon52<-(som$females_18_40+som$females_41_59+som$males_18_40+som$males_41_59)==0 & som$less_food_adult>0
#i52<-which(incon52)
#u52<-som$X_uuid[which(incon52)]
#v52<-rep("less_food_adult to adults, but no adults in hh", length(i52))
#uuid<-c(uuid,u52)
#index<-c(index,i52)
#variables<-c(variables,v52)

incon53<-hhChildren==0 & ((som$child_fever=="yes_7less"| som$child_fever=="yes7more") |(som$child_fever=="yes_7less"& som$child_fever=="yes7more"))
i53<-which(incon53)
u53<-som$X_uuid[which(incon53)]
v53<-rep("child_fever=yes_7less and or yes7more, but no child in hh", length(i53))
uuid<-c(uuid,u53)
index<-c(index,i53)
variables<-c(variables,v53)

incon54<-hhChildren==0 & som$child_thin=="yes"
i54<-which(incon54)
u54<-som$X_uuid[which(incon54)]
v54<-rep("child_this=yes, but no child in hh", length(i54))
uuid<-c(uuid,u54)
index<-c(index,i54)
variables<-c(variables,v54)

#incon55<-som$school_barrier_boys_note=="" & (som$males_6_12+som$males_13_15+som$males_16_17)>0
#i55<-which(incon55)
#u55<-som$X_uuid[which(incon55)]
#v55<-rep("no school_barries_boys_note, but boys between 6&17 in hh", length(i55))
#uuid<-c(uuid,u55)
#index<-c(index,i55)
#variables<-c(variables,v55)
#no cases

#interesting since no cases with boys, but 646 with girls, aren't they meant to go to school?
#incon56<-som$school_barrier_girls_note=="" & (som$females_6_12+som$females_13_15+som$females_16_17)>0
#i56<-which(incon56)
#u56<-som$X_uuid[which(incon56)]
#v56<-rep("no school_barries_girls_note, but girls between 6&17 in hh", length(i56))
#uuid<-c(uuid,u56)
#index<-c(index,i56)
#variables<-c(variables,v56)

incon57<-som_nona$enrolled_girls_6_12+som_nona$enrolled_girls_13_17>0 & som$school_barrier_girls_note==""
i57<-which(incon57)
u57<-som$X_uuid[which(incon57)]
v57<-rep("school_barries_girls_note=empty, but girls between 6&17 enrolled", length(i57))
uuid<-c(uuid,u57)
index<-c(index,i57)
variables<-c(variables,v57)

incon57_1<-som_nona$enrolled_boys_6_12+som_nona$enrolled_boys_13_17>0 & som_nona$school_barrier_boys_note==""
i57_1<-which(incon57_1)
u57_1<-som$X_uuid[which(incon57_1)]
v57_1<-rep("school_barries_boys_note=empty, but boys between 6&17 enrolled", length(i57_1))
uuid<-c(uuid,u57_1)
index<-c(index,i57_1)
variables<-c(variables,v57_1)

#incon58<-(som$enrolled_girls_6_12+som$enrolled_girls_13_17)< (som$females_6_12+som$females_13_15+som$females_16_17)
#i58<-which(incon58)
#u58<-som$X_uuid[which(incon58)]
#v58<-rep("girls in hh, but not enrolled", length(i58))
#uuid<-c(uuid,u58)
#index<-c(index,i58)
#variables<-c(variables,v58)
#1353 girls are not enrolled in school

###DROPPED (sum up to new variable)
#incon59<-som$hh_female == (som$females_13_15+som$females_16_17+som$females_18_40+som$females_41_59+som$females_60_over)
#i59<-which(!incon59)
#u59<-som$X_uuid[which(!incon59)]
#v59<-rep("hh_female does not fit to females added", length(i59))
#uuid<-c(uuid,u59)
#index<-c(index,i59)
#variables<-c(variables,v59)

incon60<-som_nona$chronic_illness_hh_members.female_13 <= som$females_13_15
i60<-which(!incon60)
u60<-som$X_uuid[which(!incon60)]
v60<-rep("chronic_illness_hh_members.female_13 are more than females_13_15", length(i60))
uuid<-c(uuid,u60)
index<-c(index,i60)
variables<-c(variables,v60)

incon61<-som_nona$chronic_illness_hh_members.female_14_17 <= (som$females_13_15+som$females_16_17)
i61<-which(!incon61)
u61<-som$X_uuid[which(!incon61)]
v61<-rep("chronic_illness_hh_members.female_14_17 are more than females_16_17+females_13_15", length(i61))
uuid<-c(uuid,u61)
index<-c(index,i61)
variables<-c(variables,v61)

incon62<-som_nona$chronic_illness_hh_members.male_13 <= som$males_13_15
i62<-which(!incon62)
u62<-som$X_uuid[which(!incon62)]
v62<-rep("chronic_illness_hh_members.male_13 are more than males_13_15", length(i62))
uuid<-c(uuid,u62)
index<-c(index,i62)
variables<-c(variables,v62)

incon63<-som_nona$chronic_illness_hh_members.male_14_17 <= (som$males_13_15+som$males_16_17)
i63<-which(!incon63)
u63<-som$X_uuid[which(!incon63)]
v63<-rep("chronic_illness_hh_members.male_14_17 are more than males_16_17+males_13_15", length(i63))
uuid<-c(uuid,u63)
index<-c(index,i63)
variables<-c(variables,v63)

###DROPPED
#incon64<- ((som$avg_income+som$avg_debt) >= (som$food_expenditure+som$health_cost+som$on_water))
#i64<-which(!incon64)
#u64<-som$X_uuid[which(!incon64)]
#v64<-rep("more expenditures (food, health, water) than income and dept", length(i64))
#uuid<-c(uuid,u64)
#index<-c(index,i64)
#variables<-c(variables,v64)

###DROPPED (sum variable up again)
#incon65<- som_nona$HH_schoolaged_children==(som$males_6_12+som$males_13_15+som$males_16_17+som$females_6_12+som$females_13_15+som$females_16_17)
#i65<-which(!incon65)
#u65<-som$X_uuid[which(!incon65)]
#v65<-rep("HH_schoolaged_children not equal added children 6-17years", length(i65))
#uuid<-c(uuid,u65)
#index<-c(index,i65)
#variables<-c(variables,v65)

incon66<- som_nona$enrolled_girls_6_12<=som$females_6_12
i66<-which(!incon66)
u66<-som$X_uuid[which(!incon66)]
v66<-rep("enrolled_girls_6_12 is bigger than females_6_12", length(i66))
uuid<-c(uuid,u66)
index<-c(index,i66)
variables<-c(variables,v66)

incon67<- som_nona$enrolled_girls_13_17<=som$females_13_15+som$females_16_17
i67<-which(!incon67)
u67<-som$X_uuid[which(!incon67)]
v67<-rep("enrolled_girls_13_17 is bigger than females_13_15+females_16_17", length(i67))
uuid<-c(uuid,u67)
index<-c(index,i67)
variables<-c(variables,v67)

incon68<- som_nona$enrolled_boys_6_12<=som$males_6_12
i68<-which(!incon68)
u68<-som$X_uuid[which(!incon68)]
v68<-rep("enrolled_boys_6_12 is bigger than males_6_12", length(i68))
uuid<-c(uuid,u68)
index<-c(index,i68)
variables<-c(variables,v68)

incon69<- som_nona$enrolled_boys_13_17<=som$males_13_15+som$males_16_17
i69<-which(!incon69)
u69<-som$X_uuid[which(!incon69)]
v69<-rep("enrolled_boys_13_17 is bigger than males_13_15+males_16_17", length(i69))
uuid<-c(uuid,u69)
index<-c(index,i69)
variables<-c(variables,v69)

incon70<- som_nona$enrolled_girls_6_12e<=som$females_6_12
i70<-which(!incon70)
u70<-som$X_uuid[which(!incon70)]
v70<-rep("enrolled_girls_6_12e is bigger than females_6_12", length(i70))
uuid<-c(uuid,u70)
index<-c(index,i70)
variables<-c(variables,v70)

incon71<- som_nona$enrolled_girls_13_17e<=(som$females_13_15+som$females_16_17)
i71<-which(!incon71)
u71<-som$X_uuid[which(!incon71)]
v71<-rep("enrolled_girls_13_17e is bigger than females_13_15+females_16_17", length(i71))
uuid<-c(uuid,u71)
index<-c(index,i71)
variables<-c(variables,v71)

incon72<- som_nona$enrolled_boys_6_12e<=som$males_6_12
i72<-which(!incon72)
u72<-som$X_uuid[which(!incon72)]
v72<-rep("enrolled_boys_6_12e is bigger than males_6_12", length(i72))
uuid<-c(uuid,u72)
index<-c(index,i72)
variables<-c(variables,v72)

incon73<- som_nona$enrolled_boys_13_17e<=(som$males_13_15+som$males_16_17)
i73<-which(!incon73)
u73<-som$X_uuid[which(!incon73)]
v73<-rep("enrolled_boys_13_17e is bigger than males_13_15+males_16_17", length(i73))
uuid<-c(uuid,u73)
index<-c(index,i73)
variables<-c(variables,v73)

###DROPPED
#incon74<- (som_nona$enrolled_girls_6_12e +som_nona$enrolled_girls_6_12)<=som$females_6_12
#i74<-which(!incon74)
#u74<-som$X_uuid[which(!incon74)]
#v74<-rep("enrolled_girls_6_12e + enrolled_girls_6_12 is bigger than females_6_12", length(i74))
#uuid<-c(uuid,u74)
#index<-c(index,i74)
#variables<-c(variables,v74)

###DROPPED
#incon75<- (som_nona$enrolled_girls_13_17e +som_nona$enrolled_girls_13_17)<=som$females_13_15+som$females_16_17
#i75<-which(!incon75)
#u75<-som$X_uuid[which(!incon75)]
#v75<-rep("enrolled_girls_13_17e + enrolled_girls_13_17 is bigger than females_13_15+females_16_17", length(i75))
#uuid<-c(uuid,u75)
#index<-c(index,i75)
#variables<-c(variables,v75)

###DROPPED
#incon76<- (som_nona$enrolled_boys_6_12e + som_nona$enrolled_boys_6_12)<=som$males_6_12
#i76<-which(!incon76)
#u76<-som$X_uuid[which(!incon76)]
#v76<-rep("enrolled_boys_6_12e +enrolled_boys_6_12 is bigger than males_6_12", length(i76))
#uuid<-c(uuid,u76)
#index<-c(index,i76)
#variables<-c(variables,v76)

#DROPPED
#incon77<- (som_nona$enrolled_boys_13_17e +som_nona$enrolled_boys_13_17)<=som$males_13_15+som$males_16_17
#i77<-which(!incon77)
#u77<-som$X_uuid[which(!incon77)]
#v77<-rep("enrolled_boys_13_17e + enrolled_boys_13_17 is bigger than males_13_15+males_16_17", length(i77))
#uuid<-c(uuid,u77)
#index<-c(index,i77)
#variables<-c(variables,v77)


incon78<- som_nona$covid_girls_6_12<=som$females_6_12
i78<-which(!incon78)
u78<-som$X_uuid[which(!incon78)]
v78<-rep("covid_girls_6_12 is bigger than females_6_12", length(i78))
uuid<-c(uuid,u78)
index<-c(index,i78)
variables<-c(variables,v78)

incon79<- som_nona$covid_girls_13_17<=som$females_13_15+som$females_16_17
i79<-which(!incon79)
u79<-som$X_uuid[which(!incon79)]
v79<-rep("covid_girls_13_17 is bigger than females_13_15+females_16_17", length(i79))
uuid<-c(uuid,u79)
index<-c(index,i79)
variables<-c(variables,v79)

incon80<- som_nona$covid_boys_6_12<=som$males_6_12
i80<-which(!incon80)
u80<-som$X_uuid[which(!incon80)]
v80<-rep("covid_boys_6_12 is bigger than males_6_12", length(i80))
uuid<-c(uuid,u80)
index<-c(index,i80)
variables<-c(variables,v80)

incon81<- som_nona$covid_boys_13_17<=som$males_13_15+som$males_16_17
i81<-which(!incon81)
u81<-som$X_uuid[which(!incon81)]
v81<-rep("covid_boys_13_17 is bigger than males_13_15+males_16_17", length(i81))
uuid<-c(uuid,u81)
index<-c(index,i81)
variables<-c(variables,v81)

incon82<- som_nona$home_girls_6_12<=som$females_6_12
i82<-which(!incon82)
u82<-som$X_uuid[which(!incon82)]
v82<-rep("home_girls_6_12 is bigger than females_6_12", length(i82))
uuid<-c(uuid,u82)
index<-c(index,i82)
variables<-c(variables,v82)

incon83<- som_nona$home_girls_13_17<=som$females_13_15+som$females_16_17
i83<-which(!incon83)
u83<-som$X_uuid[which(!incon83)]
v83<-rep("home_girls_13_17 is bigger than females_13_15+females_16_17", length(i83))
uuid<-c(uuid,u83)
index<-c(index,i83)
variables<-c(variables,v83)

incon84<- som_nona$home_boys_6_12<=som$males_6_12
i84<-which(!incon84)
u84<-som$X_uuid[which(!incon84)]
v84<-rep("home_boys_6_12 is bigger than males_6_12", length(i84))
uuid<-c(uuid,u84)
index<-c(index,i84)
variables<-c(variables,v84)

incon85<- som_nona$home_boys_13_17<=som$males_13_15+som$males_16_17
i85<-which(!incon85)
u85<-som$X_uuid[which(!incon85)]
v85<-rep("home_boys_13_17 is bigger than males_13_15+males_16_17", length(i85))
uuid<-c(uuid,u85)
index<-c(index,i85)
variables<-c(variables,v85)

###DROPPED (sum variable up again)
#incon86<- som_nona$children_school_age==(som$males_6_12+som$males_13_15+som$males_16_17+som$females_6_12+som$females_13_15+som$females_16_17)
#i86<-which(!incon86)
#u86<-som$X_uuid[which(!incon86)]
#v86<-rep("children_school_age not equal added children 6-17years", length(i86))
#uuid<-c(uuid,u86)
#index<-c(index,i86)
#variables<-c(variables,v86)

###DROPPED (sum variable up again)
#incon87<-(som_nona$covid_boys_6_12+som_nona$covid_girls_6_12+som_nona$covid_boys_13_17+som_nona$covid_girls_13_17)==som_nona$covid_enrollement
#i87<-which(!incon87)
#u87<-som_nona$X_uuid[which(!incon87)]
#v87<-rep("covid_enrollement is not equal to sum of covid children", length(i87))
#uuid<-c(uuid,u87)
#index<-c(index,i87)
#variables<-c(variables,v87)

incon88<-(som_nona$enrolled_girls_6_12e+som_nona$enrolled_girls_13_17e+som_nona$enrolled_boys_6_12e+som_nona$enrolled_boys_13_17e)==som_nona$enrollement_note2
i88<-which(!incon88)
u88<-som_nona$X_uuid[which(!incon88)]
v88<-rep("enrolement_note2 is not equal to sum of dopped out children", length(i88))
uuid<-c(uuid,u88)
index<-c(index,i88)
variables<-c(variables,v88)


########EXPORTING###################################################################################################
#remove na's which happened through loop without cases 
uuid <- uuid[!is.na(uuid)]


inconsistencies<-data.frame(uuid, index, variables)
ones<-as.integer(rep(1, length(uuid)))
inconsis<-data.frame(uuid, variables, ones)


#for debugging
length(variables)
length(index)
length(uuid)
length(unique(uuid))

#####Export inconsistencies
inconsis=dcast(data = inconsis, formula = uuid ~ variables, fun.aggregate = mean, value.var = "ones")

#add total column
l=length(inconsis)
for (i in 1:nrow(inconsis))
{
  inconsis$total[i]<-sum(inconsis[i,2:l], na.rm = T)
}

write_xlsx(inconsis, paste0("~/Desktop/REACH/Data/inconsistencies",today,".xlsx"))

######Export Count of inconsistencies
l_2<-length(inconsis)
colSum <- data.frame(names = names(inconsis[,2:l_2]), sums_inconsis=colSums(inconsis[,2:l_2], na.rm = TRUE, dims = 1))
write_xlsx(colSum, paste0("inconCount",today,".xlsx"))

###Export Inspect and Inconsistencies combined

#merge outliers and inconsistencies, make a wide table, add total and totalInd and export
mergedOutInc<-Reduce(function(x, y) merge(x, y, all=TRUE), list(inconsistencies, inspect2))
mergedOutInc$ones<-as.integer(rep(1, nrow(mergedOutInc)))
Out_Inc<-dcast(data = mergedOutInc, formula = uuid ~ variables, fun.aggregate = mean, value.var = "ones")

#add total column 
l_3=length(Out_Inc)
for (i in 1:nrow(Out_Inc))
{
  Out_Inc$total[i]<-sum(Out_Inc[i,2:l_3], na.rm = T)
}


#sum all inconsistencies including questions for the indicators for each survey

###UPDATED, see below
#for (i in 1:nrow(Out_Inc))
#{
#  Out_Inc$total_Ind[i]<-sum(Out_Inc[i,c("less_food to elderly, but no elderly in hh","sanitation_facility=none_of or latrine_features.walls_=1, but toilets in hh",
#                                       "medical_services=no and barriers_health.no_issues=1","latrine_features.door=1, but latrine_features.walls_=0",
#                                       "latrine_features.lock=1, but latrine_features.door or latrine_features.walls_ isn't","un_continue=un_stop",
#                                       "water_barrier.waterpoints_far=1, but water_source_time under 15min", "factors_aid.60.=1, but no hh memeber over 60 listed",
#                                       "latrine_features.lock=1, but stated no privacy: sanitation_barriers.sanitation_etc=1", "factors_aid.Disability._Person_living_with_a_disability=1, but no in all disabalitily types (hearing, seeing etc",
#                                       "read_write=no, doesn't fit to remote_edu_via answer", "enrollement_note is bigger than som$enrolled_total",
#                                       "health_access=no, but barriers_health.no_issues=1", "sum of covid enrolled children is not equal to sum of enrolled children",
#                                       "less_food_adult to adults, but no adults in hh", "child_labor_notes does not equal boys_labor+girls_labor",
#                                       "enough_food=yes, but income too high and on_water&note_accomodation&food_expenditure not high enough",
#                                       "sum of males and females under 18 does not match hh_children", "all girls in hh are less than girls_labor",
#                                       "information_channel=no_need & aid_barriers.lack_information=1", "more hh_members_new_unemployed than adult hh members",
#                                       "more chronic ill children than hh children")], na.rm = T)
#}

###UPDATED
for (i in 1:nrow(Out_Inc))
{
  Out_Inc$total_Ind[i]<-sum(Out_Inc[i,c(#"sanitation_facility=none_of or latrine_features.walls_=1, but toilets in hh",
                                        #"latrine_features.door=1, but latrine_features.walls_=0",
                                        #"latrine_features.lock=1, but latrine_features.door or latrine_features.walls_ isn't","un_continue=un_stop",
                                        "factors_aid.Disability._Person_living_with_a_disability=1, but no in all disabalitily types (hearing, seeing etc",
                                       #"enrollement_note is bigger than enrolled_total", #no cases any more
                                       #"sum of males and females under 18 does not match hh_children", 
                                        "all girls in hh are less than girls_labor", "more hh_members_new_unemployed than adult hh members",
                                        "more chronic ill children than hh children")], na.rm = T)
}


write_xlsx(Out_Inc, paste0("~/Desktop/REACH/Data/Out_Inc",today,".xlsx"))

####EXPORT only for inconsistencies (no outliers etc)#############################################################################################

Inc<-inconsistencies
Inc$ones<-as.integer(rep(1, nrow(Inc)))
Inc<-dcast(data = Inc, formula = uuid ~ variables, fun.aggregate = mean, value.var = "ones")

l_3=length(Inc)
for (i in 1:nrow(Inc))
{
  Inc$total[i]<-sum(Inc[i,2:l_3], na.rm = T)
}


for (i in 1:nrow(Inc))
{
  Inc$total_Ind[i]<-sum(Inc[i,c(#"sanitation_facility=none_of or latrine_features.walls_=1, but toilets in hh",
    #"latrine_features.door=1, but latrine_features.walls_=0",
    #"latrine_features.lock=1, but latrine_features.door or latrine_features.walls_ isn't","un_continue=un_stop",
    "factors_aid.Disability._Person_living_with_a_disability=1, but no in all disabalitily types (hearing, seeing etc",
    #"enrollement_note is bigger than enrolled_total", #no cases any more
    #"sum of males and females under 18 does not match hh_children", 
    "all girls in hh are less than girls_labor", "more hh_members_new_unemployed than adult hh members",
    "more chronic ill children than hh children")], na.rm = T)
}


write_xlsx(Inc, paste0("~/Desktop/REACH/Data/Inc",today,".xlsx"))


#######check which surveys with the last inconsistency screw our sampling frame####################################################################################
i64_non_idp<-which(!incon64 & som$idp_settlement=="no")
i64_idp<-which(!incon64 & som$idp_settlement=="yes")
table_dist_idp<-as.data.frame(table(som$district[i64_idp]))
table_dist_non_idp<-as.data.frame(table(som$district[i64_non_idp]))
table_dist<-as.data.frame(table(som$district[i64]))

write_xlsx(table_dist_idp, paste0("~/Desktop/REACH/Data/table_dist_idp",today,"_clean_data.xlsx"))
write_xlsx(table_dist_non_idp, paste0("~/Desktop/REACH/Data/table_dist_non_idp",today,"_clean_data.xlsx"))

#for callbacks (focus on surveys crucial for sampling frame)
i64_1<-which(!incon64 & som$idp_settlement=="no" & som$district=="buur_hakaba")
i64_2<-which(!incon64 & som$idp_settlement=="no" & som$district=="cadaado")
i64_3<-which(!incon64 & som$idp_settlement=="no" & som$district=="badhaadhe")
i64_4<-which(!incon64 & som$idp_settlement=="yes"& som$district=="hargeysa")
i64_5<-which(!incon64 & som$district=="balcad")

index_call<-c(i30,i64_1,i64_2,i64_3,i64_4)
reason1<-rep("pregnancy under 13 years?", length(i30))
reason2<-rep("avg_income+avg_debt is less than expenditures for food, water & health?", (length(index_call)-length(i30)))
reason<- c(reason1,reason2)

uuids<-som$X_uuid[index_call]

for_callbacks<-data.frame(uuids, reason)
write_xlsx(for_callbacks, paste0("~/Desktop/REACH/Data/for_callbacks",today,".xlsx"))


######CHANGES & DELETION#############################################################################################################################################################
incon37<-incon37<-som$sanitation_facility=="none_of" & som$latrine_features.walls_==1
i37<-which(incon37)
u37<-som$X_uuid[which(incon37)]

som[i37,"sanitation_facility"]<-""
som[i39, "latrine_features.walls_"]<- 1
som[i38, "latrine_features.walls_"]<- 1
som[i38, "latrine_features.door"]<- 1
#som[i46, "un_stop"] <- ""
#som[i46, "un_continue"] <- ""

####cleaning log run afterwards


write_xlsx(som, paste0("~/Desktop/REACH/Data/SOM_MSNA2020_Merged_V5",today,"_clean_data.xlsx"))
write.csv(som, file= paste0("~/Desktop/REACH/Data/SOM_MSNA2020_Merged_V5",today,"_clean_data.csv"), row.names=FALSE)
################################################################################################################################################################
#delete surveys with inconsistencies left (1983)
som_final<-som
for (i in 1:length(unique(uuid))){
  som_final<-som_final[!som_final$X_uuid==unique(uuid)[i],]}

write_xlsx(som_final, paste0("~/Desktop/REACH/Data/SOM_MSNA2020_Merged_V5",today,"_clean_data.xlsx"))
write.csv(som_final, file= paste0("~/Desktop/REACH/Data/SOM_MSNA2020_Merged_V5",today,"_clean_data.csv"), row.names=FALSE)

#cleaning log
X_uuid<-unique(uuid)
issue<-rep("deleted survey due to inconsistency with R (see github)", length(X_uuid))
today2 <- Sys.Date()
today2<-format(today2, format="%Y %b %d")
changed<-rep(today2, length(X_uuid))

log<-data.frame(X_uuid,issue,changed )

write_xlsx(log, paste0("~/Desktop/REACH/Data/Cleaning_log_deletion",today,"_clean_data.xlsx"))
write.csv(log, file= paste0("~/Desktop/REACH/Data/Cleaning_log_deletion",today,"_clean_data.csv"), row.names=FALSE)

#get deleted surveys

som_delete<-anti_join(som,som_final)

write_xlsx(som_delete, paste0("~/Desktop/REACH/Data/SOM_MSNA2020_Merged_V5",today,"_deleted_surveys.xlsx"))
write.csv(som_delete, file= paste0("~/Desktop/REACH/Data/SOM_MSNA2020_Merged_V5",today,"_deleted_surveys.csv"), row.names=FALSE)


#####FEEDING IN NEW CALLBACKS (AFTER FEEDBACK FROM HQ)#################################################################################################################################

library(help="clog")

som[] <- lapply(som, as.character)

clean_log <- read_excel("~/Desktop/REACH/Data/callback_data_september07.xlsx")
clean_log$change<-rep(TRUE, nrow(clean_log))
cl=cleaninglog(ids=clean_log$uuids, variables=clean_log$Question, new_values=clean_log$New_Value,change=clean_log$change, data_id_column_name='X_uuid')

cleaned_data <- som %>% clog_clean(cl)
now_clog<-clog_get_cleaninglog(cleaned_data)

write_xlsx(cleaned_data, paste0("~/Desktop/REACH/Data/SOM_MSNA2020",today,".xlsx"))
write_xlsx(now_clog, paste0("~/Desktop/REACH/Data/Cleaning_log",today,".xlsx"))


###########FIX FURTHER ISSUES 01.10.20#########################################################################################

###fix second, third party problem
second<-som$second_priority
third<-som$third_priority
som$second_priority[som$second_priority==""]<-som$third_priority[som$second_priority==""]
som$third_priority[second==""]<-""

###change names of districts
som$district[som$district=="badhan"]<-"laasqoray"
som$district[som$district=="wardhiigleey" | som$district=="hawl_wadaag" | som$district=="hamar_jaab_jab" | som$district=="abdulaziz" | som$district=="karaan" | som$district=="shibis" | som$district=="waaberi" | som$district=="wadajir" | som$district=="hamar_weyne" | som$district=="heliwa" | som$district=="yaaqshiid" | som$district=="boondheere" | som$district=="shangaani"]<-"banadir_other"
som$district[som$district=="daynile"]<-"banadir_daynile"
som$district[som$district=="dharkenley"]<-"banadir_dharkenley"
som$district[som$district=="hodan"]<-"banadir_hodan"
som$district[som$district=="kahda"]<-"banadir_kahda"
som$district[som$district=="gaalkacyo_north" | som$district=="gaalkacyo_south"] <- "gaalkacyo"
som$district[som$district=="mataban"]<-"belet_weyne"

###fix incon 46
som$un_continue[i46]<-""
som$un_stop[i46]<-""


##############run cleaning log script#####################################
write_xlsx(som, paste0("C:/Users/Vanessa Causemann/Desktop/REACH/Data/myOutputs/cleaned_data",today,".xlsx"))

###delete surveys with not solvable inconsistencies

#combine all indexes meant to delete and pick unique values
d_index<- unique(c(i57, i73, i26_1))
#delete
som_final<-som[-d_index,]

#get deleted surveys

som_delete<-anti_join(som,som_final)

write_xlsx(som_delete, paste0("C:/Users/Vanessa Causemann/Desktop/REACH/Data/myOutputs/",today,"_deleted_surveys.xlsx"))
write_xlsx(som_final, paste0("C:/Users/Vanessa Causemann/Desktop/REACH/Data/myOutputs/cleaned_data3",today,".xlsx"))


#adding more district renaming##################################################################
ban<- which(som_final$idp_settlement=="no" & (som_final$district=="banadir_daynile" | som_final$district=="banadir_dharkenley" | som_final$district=="banadir_hodan" | som_final$district=="banadir_kahda"| som_final$district=="banadir_other"))

#create and export cleaning log
uuid_ban<-som_final$X_uuid[ban]
quest_ban<-rep("district", length(uuid_ban))
old_val<-som_final$district[ban]
new_val<- rep("banadir_hc", length(uuid_ban))

clean_log<-data.frame(uuid=uuid_ban, question=quest_ban, old_value=old_val, new_value=new_val)
write_xlsx(clean_log, paste0("C:/Users/Vanessa Causemann/Desktop/REACH/Data/myOutputs/",today,"clean_log.xlsx"))

#do the actual change
som_final$district[ban]<-"banadir_hc"



#delete surveys which didn't meet the target of 30 surveys by district

som_cover_hc = som_final[som_final$idp_settlement=="no",] %>% count(district)
som_cover_idp = som_final[som_final$idp_settlement=="yes",] %>% count(district)

d_distr_hc<- som_cover_hc$district[which(som_cover_hc$n<30)]
d_distr_idp<- som_cover_idp$district[which(som_cover_idp$n<30)]

d_hc<- which(som_final$district==d_distr_hc[1] & som_final$idp_settlement=="no")
for (i in 2:length(d_distr_hc))
{
  d_hc<-c(d_hc, which(som_final$district==d_distr_hc[i] & som_final$idp_settlement=="no"))
}


d_idp<- which(som_final$district==d_distr_idp[1] & som_final$idp_settlement=="yes")
for (i in 2:length(d_distr_idp))
{
  d_idp<-c(d_idp, which(som_final$district==d_distr_idp[i] & som_final$idp_settlement=="yes"))
}

d_bal<- which(som_final$district=="balcad" & som_final$idp_settlement=="yes")

d_index2<-c(d_hc,d_idp, d_bal)
som_final2<-som_final[-d_index2,]

#get deleted surveys again

som_delete2<-anti_join(som_final,som_final2)
som_delete2$index<-d_index2

write_xlsx(som_delete2, paste0("C:/Users/Vanessa Causemann/Desktop/REACH/Data/myOutputs/",today,"_deleted_surveys2.xlsx"))
write.csv(som_delete2, file= paste0("C:/Users/Vanessa Causemann/Desktop/REACH/Data/myOutputs/",today,"_deleted_surveys2.csv"), row.names=FALSE)
write_xlsx(som_final2, paste0("C:/Users/Vanessa Causemann/Desktop/REACH/Data/myOutputs/cleaned_data3",today,".xlsx"))
write.csv(som_final2, file= paste0("C:/Users/Vanessa Causemann/Desktop/REACH/Data/myOutputs/",today,"cleaned_data3.csv"), row.names=FALSE)


#############got done by Mohamed#####################################################################################################################################################################################
######SUM UP TOTAL VARIBALES NEW################################
num <- unlist(lapply(som, is.numeric)) 
somNum<-som[ , num]
colnames(somNum)


som_nona<-som
som_nona[is.na(som_nona)]=0


som$hh_size <- (som$males_0m_5y+som$males_6_12+som$males_13_15+som$males_16_17+som$males_18_40+som$males_41_59+som$males_60_over+som$females_0m_5y+som$females_6_12+som$females_13_15+som$females_16_17+som$females_18_40+som$females_41_59+som$females_60_over)
som$total_hh<- (som$males_0m_5y+som$males_6_12+som$males_13_15+som$males_16_17+som$males_18_40+som$males_41_59+som$males_60_over+som$females_0m_5y+som$females_6_12+som$females_13_15+som$females_16_17+som$females_18_40+som$females_41_59+som$females_60_over)
som$hh_children<-(som$males_0m_5y+som$males_6_12+som$males_13_15+som$males_16_17+som$females_0m_5y+som$females_6_12+som$females_13_15+som$females_16_17)
som$hh_female<-(som$females_13_15+som$females_16_17+som$females_18_40+som$females_41_59+som$females_60_over)
som$children_school_age<- (som$males_6_12+som$males_13_15+som$males_16_17+som$females_6_12+som$females_13_15+som$females_16_17)
som$HH_schoolaged_children<- (som$males_6_12+som$males_13_15+som$males_16_17+som$females_6_12+som$females_13_15+som$females_16_17)
# ? children_vaccine_age which age group?
#? hh_difference

som$enrolled_total<-(som_nona$enrolled_boys_6_12+som_nona$enrolled_girls_6_12+som_nona$enrolled_boys_13_17+som_nona$enrolled_girls_13_17)
som$enrolled_total[som$enrolled_total==0]<-"" # in original variabl 3x0 ?

#? som$enrollement_note (children attending school regular before covid, where to take that information from?, 1281 NA, 5641 = 0)
# ? som$enrollement_note2 #(children dropping out school before covid,, where to take that information from? 0 if none (8337times), also 1281 NAs in the variable?)

som$covid_enrollement<- (som_nona$covid_boys_6_12+som_nona$covid_girls_6_12+som_nona$covid_boys_13_17+som_nona$covid_girls_13_17)
som$covid_enrollement[som$covid_enrollement==0]<-"" # in original variable 246x0 & 128xNA's?

som$sum_rooms <-(som_nona$bedrooms+som_nona$living_rooms+som_nona$kitchens+som_nona$toilets) #986 NA#s & 237  0's
som$rooms_total <- (som_nona$bedrooms+som_nona$living_rooms+som_nona$kitchens+som_nona$toilets) #986 NA#s & 237  0's

som$child_labor_notes<-(som_nona$boys_labor+som_nona$girls_labor) 
som$child_labor_notes[som$child_labor_notes==0]<-"" # in original variable 4x0 & 10073xNA?


###find skip logic:

table(som$covid_enrollement)
#find NA's
sum(is.na(som$covid_enrollement)*1)


#######changes due to labelling issues in Tableau########################################################################################################################################################################################################
som<-read.csv(file="C:/Users/Vanessa Causemann/Desktop/REACH/Data/REACH_SOM2006_JMCNA_IV_Data-Set_August2020_4.csv", head=T, dec=".", sep=",")
#remove funky added on 700k rows since last cleaning
som=som[1:10222,]

ind<- which(som$school_transport=="business")
som$school_transport[ind]<- "bus"

uuids<-som$X_uuid[ind]

names(som)[names(som) == "hand_washing_times.coughing"] <- "hand_washing_times.cough_sneeze"
names(som)[names(som) == "nutrition_barriers.not"] <- "nutrition_barriers.not_enough_prov"

#add date for export 
today <- Sys.Date()
today<-format(today, format="_%Y_%b_%d")
write.csv(som, file= paste0("C:/Users/Vanessa Causemann/Desktop/REACH/Data/myOutputs/",today,"cleaned_data.csv"), row.names=FALSE)
write.csv(uuids, file= paste0("C:/Users/Vanessa Causemann/Desktop/REACH/Data/myOutputs/",today,"uuids.csv"), row.names=FALSE)

som<-read.csv(file="C:/Users/Vanessa Causemann/Desktop/REACH/Data/myOutputs/_2020_Oct_13cleaned_data.csv", head=T, dec=".", sep=",")

som$written_documentation[som$written_documentation=="NO"]<- "no"
som$written_documentation[som$written_documentation=="No"]<- "no"
som$written_documentation[som$written_documentation=="YES"] <- "yes"

names(som)[names(som) == "wintting_blankets"] <- "nfi_market.wintting_blankets"
names(som)[names(som) == "mattting_mats"] <- "nfi_market.mattting_mats"
names(som)[names(som) == "cookting_utensils"] <- "nfi_market.cookting_utensils"
names(som)[names(som) == "cookting_fuel"] <- "nfi_market.cookting_fuel"
names(som)[names(som) == "wateting_containers"] <- "nfi_market.wateting_containers"

names(som)[names(som) == "jerrting_can"] <- "nfi_market.jerrting_can"
names(som)[names(som) == "Torches"] <- "nfi_market.Torches"
names(som)[names(som) == "solating_lamps"] <- "nfi_market.solating_lamps"
names(som)[names(som) == "solating_panels"] <- "nfi_market.solating_panels"
names(som)[names(som) == "Generators"] <- "nfi_market.Generators"

names(som)[names(som) == "Batteries"] <- "nfi_market.Batteries"
names(som)[names(som) == "Clothing"] <- "nfi_market.Clothing"
names(som)[names(som) == "wintting_clothing"] <- "nfi_market.wintting_clothing"
names(som)[names(som) == "Shoes"] <- "nfi_market.Shoes"
names(som)[names(som) == "wintting_shoes"] <- "nfi_market.wintting_shoes"

names(som)[names(som) == "wintting_heaters"] <- "nfi_market.wintting_heaters"
names(som)[names(som) == "heatting_fuel"] <- "nfi_market.heatting_fuel"
names(som)[names(som) == "dispting_diapers"] <- "nfi_market.dispting_diapers"
names(som)[names(som) == "saniting_pads"] <- "nfi_market.saniting_pads"
names(som)[names(som) == "Soap"] <- "nfi_market.Soap"

names(som)[names(som) == "washting_powder"] <- "nfi_market.washting_powder"
names(som)[names(som) == "cleating_house"] <- "nfi_market.cleating_house"
names(som)[names(som) == "deteting_dishes"] <- "nfi_market.deteting_dishes"
names(som)[names(som) == "hygiting_kits"] <- "nfi_market.hygiting_kits"
names(som)[names(som) == "handting_sanitiser"] <- "nfi_market.handting_sanitiser"

names(som)[names(som) == "faceting_masks"] <- "nfi_market.faceting_masks"
names(som)[names(som) == "dispting_gloves"] <- "nfi_market.dispting_gloves"
names(som)[names(som) == "mosqting_Nets"] <- "nfi_market.mosqting_Nets"

#add date for export 
today <- Sys.Date()
today<-format(today, format="_%Y_%b_%d")

write.csv(som, file= paste0("C:/Users/Vanessa Causemann/Desktop/REACH/Data/myOutputs/",today,"cleaned_data.csv"), row.names=FALSE)