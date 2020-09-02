install.packages("readxl")
install.packages("tidyverse")

library(readxl)
library(tidyverse)

#import the callback information
callbacks <- read.csv(file="~/Desktop/REACH/Data/callback_merged_empty cells.csv", head=T, dec=".", sep=",")

#import the actual version of the cleaned data twice
som<-read.csv(file="~/Desktop/REACH/Data/SOM_MSNA2020_Merged_2020-08-30_v4_clean_data.csv", head=T, dec=".", sep=",")
som_old<-som

###########REFORMAT################################################################################################################################

#take over columns which are not multiple
call<-callbacks[,1:4]
call[,5:8]<-callbacks[,16:19]

#merge multiple columns to one, values separated by " " and add to new data frame
call$lack_enclosure <- paste0(callbacks[,6]," ", callbacks[,7])
call$shelter_damage <- paste0(callbacks[,8]," ", callbacks[,9]," ", callbacks[,10])
call$unable_repair <- paste0(callbacks[,11]," ", callbacks[,12])
call$shelter_issues <- paste0(callbacks[,13]," ", callbacks[,14]," ", callbacks[,15])
call$shetler_support <- paste0(callbacks[,28]," ", callbacks[,29]," ", callbacks[,30])
call$hlp_problems <- paste0(callbacks[,20]," ", callbacks[,21])
call$nfi_access <- paste0(callbacks[,22]," ", callbacks[,23]," ", callbacks[,24])
call$nfi_market <- paste0(callbacks[,25]," ", callbacks[,26]," ", callbacks[,27])


###########CREATE DUMMIES##############################################################################################################################################

#create binary columns from multiple
make_dummies<-function(x, df) {
    colnum=grep(x, colnames(df))[1]
    uni<-unique(scan(text = df[,colnum], what = ""))
    l=length(uni)
    l_2=length(df)
  
    for (i in 1:l){
        blubb<-(grepl(uni[i],df[,colnum], fixed=T)*1)
        df[,l_2+i]<-blubb
        names(df)[l_2+i] <- paste0(x, ".", uni[i])
    }
    
    sub_df <- df[, c(x, paste0(x, ".", uni))]
    empty_value <- sub_df[,x] %in% c(" ", "", "  ")
    sub_df[empty_value, ] <- ""
    sub_df<-sub_df[,-1]
    return(sub_df)
}

#create data frames with binaries
dum1<-make_dummies("lack_enclosure", call)
dum2<-make_dummies("shelter_damage", call)
dum3<-make_dummies("unable_repair", call)
dum4<-make_dummies("shelter_issues", call)
dum5<-make_dummies("shetler_support", call)
dum6<-make_dummies("hlp_problems", call)
dum7<-make_dummies("nfi_access", call)
dum8<-make_dummies("nfi_market", call)

#merge all binaries with the other variables
call_all <- cbind(call, dum1,dum2, dum3,dum4, dum5, dum6, dum7)

#######SPELLING MISTAKES#########################################################################################################
#find spelling mistakes
col<-colnames(call_all)

som[c(col[1:11])]
som[c(col[116])]
colnames(call_all[116])

som[c(col[117:118])]

som[c(col[1:70])]
som[c(col[71])]
colnames(call_all[71])
#shetler_support.solating_panel
som[c(col[72])]
colnames(call_all[73])
#shetler_support.mosqutig-_Net
som[c(col[74:80])]
colnames(call_all[81])
#shetler_support.none
som[c(col[82:96])]
colnames(call_all[97])
#hlp_problems.cookting_utensils
colnames(call_all[98])
#hlp_problems.beddting_items
colnames(call_all[99])
#hlp_problems.wateting_containers
som[c(col[100:109])]
colnames(call_all[110])
#nfi_access.beddting_items
som[c(col[111])]
colnames(call_all[112])
#nfi_access.shoes
som[c(col[113:122])]
colnames(call_all[123])
#nfi_access.bedding
colnames(call_all[124])
#nfi_access.items
som[c(col[125])]
colnames(call_all[126])
#nfi_access.none_of
som[c(col[127:128])]

which(colnames(call_all)=="shetler_support.none")
which(colnames(call_all)=="nfi_access.beddting_items")
which(colnames(call_all)=="nfi_access.none_of")

#if needed: delete spelling mistakes to make the replacement run
#call_all<-call_all[,-c(81,110,126)]

#######REPLACE values in data set with callbacks values#####################################################################################################################

col<-colnames(call_all)
l=length(call_all)
nro<-nrow(call_all)
w<- 0

for (i in 1:nro){
  w[i]<-which(som$X_uuid==call_all$X_uuid[i])
  for (j in 1:l){
    som[w[i],col[j]]<-call_all[i,j]
  }
}


######debugging######################################################################################################################

#check = columns to compare
check<-c(110:116)

som[w[1],col[check]]
som_old[w[1],col[check]]
call_all[1,check]

#######EXPORT CLEANED DATA###################################################################################################################
today <- Sys.Date()
today<-format(today, format="_%Y_%b_%d")

write_xlsx(som, paste0("~/Desktop/REACH/Data/SOM_MSNA2020_Merged_2020-08-30_v4_clean_data_incl_callbacks",today,".xlsx"))
write.csv(som, file= paste0("~/Desktop/REACH/Data/SOM_MSNA2020_Merged_2020-08-30_v4_clean_data_incl_callbacks",today,".csv"), row.names=FALSE)
