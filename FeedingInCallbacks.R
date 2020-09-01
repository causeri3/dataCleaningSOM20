install.packages("readxl")
install.packages("tidyverse")

library(readxl)
library(tidyverse)

#import
callbacks <- read.csv(file="~/Desktop/REACH/Data/Call_back_Excel_Sheet_August 28_Somaliland.csv", head=T, dec=".", sep=",")

#delete senseless row in the end
row=nrow(callbacks)
callbacks<-callbacks[1:row-1,]

###########REFORMAT################################################################################################################################

#take over columns which are not multiple
call<-callbacks[,1:4]
call[,5:8]<-callbacks[,15:18]

#merge multiple columns to one, values separated by " " and add to new data frame
call$lack_enclosure <- paste0(callbacks[,5]," ", callbacks[,6])
call$shelter_damage <- paste0(callbacks[,7]," ", callbacks[,8]," ", callbacks[,9])
call$unable_repair <- paste0(callbacks[,10]," ", callbacks[,11])
call$shelter_issues <- paste0(callbacks[,12]," ", callbacks[,13]," ", callbacks[,14])
call$shetler_support <- paste0(callbacks[,27]," ", callbacks[,28]," ", callbacks[,29])
call$hlp_problems <- paste0(callbacks[,19]," ", callbacks[,20])
call$nfi_access <- paste0(callbacks[,21]," ", callbacks[,22]," ", callbacks[,23])
call$nfi_market <- paste0(callbacks[,24]," ", callbacks[,25]," ", callbacks[,26])


###########CREATE DUMMIES##############################################################################################################################################

#create binary columns from multiple
make_dummies_over<-function(x, df) {
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
dum1<-make_dummies_over("lack_enclosure", call)
dum2<-make_dummies_over("shelter_damage", call)
dum3<-make_dummies_over("unable_repair", call)
dum4<-make_dummies_over("shelter_issues", call)
dum5<-make_dummies_over("shetler_support", call)
dum6<-make_dummies_over("hlp_problems", call)
dum7<-make_dummies_over("nfi_access", call)
dum8<-make_dummies_over("nfi_market", call)

#merge all binaries with the other variables
call_all <- cbind(call, dum1,dum2, dum3,dum4, dum5, dum6, dum7)

#######SPELLING MISTAKES#########################################################################################################
#find spelling mistakes
col<-colnames(call_all)
som[c(col[1:60])]
colnames(call_all[61])
#shetler_support.none
som[c(col[62:74])]
colnames(call_all[75])
#nfi_access.beddting_items
som[c(col[76:86])]
colnames(call_all[93])
#nfi_access.none_of
som[c(col[87:89])]

which(colnames(call_all)=="shetler_support.none")
which(colnames(call_all)=="nfi_access.beddting_items")
which(colnames(call_all)=="nfi_access.none_of")

#delete spelling mistakes
call_all<-call_all[,-c(61,75,86)]

#######REPLACE values in data set with call backs values#####################################################################################################################

col<-colnames(call_all)
l=length(call_all)
nr<-nrow(som)
nro<-nrow(call_all)


for (i in 1:nro){
  w[i]<-which(som$X_uuid==call_all$X_uuid[i])
  for (j in 1:l){
  som[w[i],col[j]]<-call_all[i,j]
  }
}


######debugging######################################################################################################################
som_old<-read.csv(file="~/Desktop/REACH/Data/SOM_MSNA2020_Merged_2020-08-30_v3-clean_data.csv", head=T, dec=".", sep=",")

#check = columns to compare
check<-c(4:10)

som[w[1],col[check]]
som_old[w[1],col[check]]
call_all[1,check]

