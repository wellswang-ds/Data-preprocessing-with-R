library(data.table)
library(tidyr)
library(tibble)
library(dplyr)

JJ <- fread('hitest.csv')


hp<-fread('hp.csv')
#hp<-hp[,-1]
M<-fread('M.csv')
#M<-M[,-1]

GG<-cbind(hp,M)
GG <- GG%>% 
  mutate(ID = 1:40589)



# M -----------------------------------------------------------------------


New_M<-GG[,-c(2,4)]
#New_M[!is.na(New_M$hp_M)&&!is.na(New_M$M_M)]
Q = which(!is.na(New_M$hp_M))
QQ = which(!is.na(New_M$M_M))
QQQ = matrix(0,421)
k=1
for(i in Q){
  for(j in QQ){
    if(i == j){
      QQQ[k]<-i
      k = k+1
      }
  }
}
QQQ<-as.numeric(QQQ)
Same<-New_M[QQQ,]
#Dif<-Same[Same$hp_M!=Same$M_M,]
#Error<-JJ[Dif$ID,]
New_M$hp_M[QQQ]<-NA


# merge failed -------------------------------------------------------------------

# na_hp<-which(is.na(New_M$hp_M))
# na_M<-which(is.na(New_M$M_M))
# na_both<-matrix(0,11104)
# k=1
# for(i in na_hp){
#   for(j in na_M){
#     if(i == j){
#       na_both[k]<-i
#       k = k+1
#     }
#   }
# }
# na_both<- as.numeric(na_both)
# na_both[na_both==0]
# Mobile<-matrix(0,40589)

# for(i in 1:40589){
#   for(j in na_both){
#     if(i == j){
#       Mobile[i]<- NA
#     }
#   }
# }
# 
# for(i in 1:40589){
#   for(j in na_hp){
#     if(i == j){
#       Mobile[i]<-New_M$hp_M[i]
#     }else if(i!=j && is.na(Mobile[i])==T){
#       Mobile[i]<-NA
#     }else{
#       Mobile[i]<-New_M$M_M[i]
#     }
#   }
# }
# 



# merge M-------------------------------------------------------------------
New_M$Mobile = New_M$hp_M  
New_M$Mobile[!is.na(New_M$M_M)] = New_M$M_M[!is.na(New_M$M_M)]  # merge with M_M

# hp ----------------------------------------------------------------------


New_hp<-GG[,-c(1,3)]
Q = which(!is.na(New_hp$hp_hp))
QQ = which(!is.na(New_hp$M_hp))
QQQ = matrix(0,421)
k=1
for(i in Q){
  for(j in QQ){
    if(i == j){
      QQQ[k]<-i
      k = k+1
    }
  }
}
QQQ<-as.numeric(QQQ)
Same<-New_hp[QQQ,]
Dif<-Same[Same$hp_hp!=Same$M_hp,]
#Error<-JJ[Dif$ID,]
New_hp$M_hp[QQQ]<-NA


# Merge hp ----------------------------------------------------------------

New_hp$Home_phone = New_hp$hp_hp  
New_hp$Home_phone[!is.na(New_hp$M_hp)] = New_hp$M_hp[!is.na(New_hp$M_hp)]  # merge with M_M


# Final_Merge!!!!!!!!!!!!!! -----------------------------------------------

New_hitest<-cbind(New_hp,New_M)
New_hitest<-New_hitest[,c(4,8)]



# More fucking error!!!! --------------------------------------------------

New_hitest$Home_phone[154]<-NA
New_hitest$Home_phone[2005]<-NA
New_hitest$Home_phone[20172]<-NA
New_hitest$Home_phone[10959]<-"0209088051"
New_hitest$Home_phone[10960]<-"0209088051"
New_hitest$Home_phone[35767]<-"0425391661"
New_hitest$Mobile[12478]<-"0952906615"
New_hitest$Mobile[39402]<-"0979677313"
New_hitest$Mobile[20682]<-"0989366281"
New_hitest$Mobile[31450]<-"0980415827"
New_hitest$Mobile[35642]<-"0912203238"
New_hitest$Mobile[36698]<-NA
New_hitest$Mobile[5591]<-NA

# Final Final Final!!!!!!!!!!!!!!! ----------------------------------------

New_hitest$Mobile <- gsub("(^\\d{4})(\\d{3})(\\d{3}$)", 
                          "\\1-\\2-\\3",
                          New_hitest$Mobile)
New_hitest$Home_phone <- gsub("(^\\d{2})(\\d{4})(\\d{4}$)", 
                              "\\1-\\2-\\3",
                              New_hitest$Home_phone)

# Save file ---------------------------------------------------------------

write.csv(New_hitest,file = 'New_hitest.csv',row.names = F)
