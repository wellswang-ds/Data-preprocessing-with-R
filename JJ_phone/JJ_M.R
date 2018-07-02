library(data.table)
library(tidyr)
library(tibble)
library(dplyr)
JJ <- fread('hitest.csv')
JJ$HOME_PHONE_NO <- as.character(JJ$HOME_PHONE_NO)
JJ$Mobile <- as.character(JJ$Mobile)
summary(JJ)
head(JJ)



for(i in JJ$HOME_PHONE_NO[1:1000]){
  if(nchar(i)==10){
    print(i)
  }
}

# Error solved ------------------------------------------------------------

JJ$Mobile[1129]<-"0928904048"
JJ$Mobile[1776]<-"0922792600"
JJ$Mobile[1777]<-"0922792600"
JJ$Mobile[2335]<-""
JJ$Mobile[3565]<-"O92O7O3889"
JJ$Mobile[5389]<-"0958225761"
JJ$Mobile[9723]<-"0906833374"
JJ$Mobile[11036]<-"0937283702"
JJ$Mobile[13350:13359]<-""
JJ$Mobile[13784]<-"0933406853"
JJ$Mobile[14175]<-"O968865488"
JJ$Mobile[14176]<-"O968865489"
JJ$Mobile[16567]<-""
JJ$Mobile[20249]<-"0919206858"
JJ$Mobile[26795]<-"02-23959825"
JJ$Mobile[26796]<-"02-23959825"
JJ$Mobile[26797]<-"02-23959825"
JJ$Mobile[c(26822,27044,27893,27894,27895,27920,28142)]<-"02-23959825"
JJ$Mobile[28336]<-"0931651756"
JJ$Mobile[28502]<-"0931651756"
JJ$Mobile[28542]<-"0931686386"
JJ$Mobile[33853]<-""
JJ$Mobile[34341]<-"0932587632"
JJ$Mobile[34344]<-"0962005570"
JJ$Mobile[34835]<-"0912020982"
JJ$Mobile[34907]<-""
JJ$Mobile[35033]<-"0921618469"
JJ$Mobile[35051]<-"O9352O3157"
JJ$Mobile[35226]<-"0928660177"
JJ$Mobile[35420]<-"0933926594"
JJ$Mobile[35538]<-""
JJ$Mobile[35542]<-"0919869058"
JJ$Mobile[35576]<- "O973853915"
JJ$Mobile[40155]<-"0953658566"
JJ$Mobile[40235]<-""
JJ$Mobile[40372]<-""
JJ$Mobile[40554]<-"0911414211"
# Function hp -------------------------------------------------------------


MM1<-JJ
GGphone <- function(phone){
  
  phone <- gsub("[[:space:]]", "", phone)
  phone <- gsub("[[:punct:]]", "", phone)
  phone <- gsub('O','0',phone)
  phone <- gsub('o','0',phone)
  phone[nchar(phone)==7] <- unlist(paste0('022',phone[nchar(phone) == 7]))
  phone[nchar(phone)==8] <- unlist(paste0('02',phone[nchar(phone) == 8]))
  phone[nchar(phone) == 9] <- unlist(paste0(0,phone[nchar(phone) == 9]))
  #phone[nchar(phone) == 10] <- gsub("(^\\d{4})(\\d{3})(\\d{3}$)", 
  #                                   "\\1-\\2-\\3",
  #                                  phone[nchar(phone) == 10])
  phone[nchar(phone)==12]<- gsub('886','0',phone[nchar(phone)==12])
  
  phone[nchar(phone) < 7] <- ""
  phone[nchar(phone) > 10] <- ""
  phone[nchar(phone) == 0] <- NA
  return(phone)
}
MM1$Mobile<-GGphone(MM1$Mobile)




# seperate ----------------------------------------------------------------

MM1 <- as_tibble(MM1)
MM1 <- MM1%>% 
  mutate(ID = 1:40589)
MM1 <- MM1[,-1]


MM2 <- MM1 %>%
  filter(!is.na(Mobile))%>%
  mutate(M = Mobile)%>%
  separate(M,into = c('ftwo','num'),sep=2,convert = T)


MM2$ftwo<- as.factor(MM2$ftwo)
summary(MM2)


# Find error --------------------------------------------------------------


WrongID<-MM2[which(MM2$ftwo==0),]
WID<- WrongID$ID

MM1$Mobile[WID]<-NA 
# j=1
# for(i in WID){
#   MM1$Mobile[i] <- unlist(paste0('09',WrongID[j,"num"]))
#   j=j+1
# }

##run again JJ change to MM

#   JJ2 <- JJ1 %>%
#   filter(!is.na(HOME_PHONE_NO))%>%
#   mutate(hp = HOME_PHONE_NO)%>%
#   separate(hp,into = c('ftwo','num'),sep=2,convert = T)
# 
# 
# JJ2$ftwo<- as.factor(JJ2$ftwo)
# summary(JJ2)


GG<-MM2[which(MM2$ftwo==8),]
GG2<-MM2[which(!MM2$ftwo%in%c(0,2,4,8,9)),]

#clean the 10 digits wrong numbers that don't start with 0
GG2$ftwo <- as.character(GG2$ftwo)
MM1$Mobile[GG2$ID[nchar(GG2$ftwo)==2]]<-NA 

##run again JJ change to MM

#   JJ2 <- JJ1 %>%
#   filter(!is.na(HOME_PHONE_NO))%>%
#   mutate(hp = HOME_PHONE_NO)%>%
#   separate(hp,into = c('ftwo','num'),sep=2,convert = T)
# 
# 
# JJ2$ftwo<- as.factor(JJ2$ftwo)
# summary(JJ2)




# hp to M, hp to hp-----------------------------------------------------------------

MM3<-MM1
MM3 <- MM3%>%
  mutate(M_M = Mobile)%>%
  mutate(M_hp = Mobile)

MM3$M_M[MM2$ID[which(MM2$ftwo!=9)]] <- NA
MM3$M_hp[MM2$ID[which(MM2$ftwo==9)]] <- NA


# save data  --------------------------------------------------------------

MM3 <- MM3[,-c(1:2)]
write.csv(MM3,file = 'M.csv',row.names = F)
