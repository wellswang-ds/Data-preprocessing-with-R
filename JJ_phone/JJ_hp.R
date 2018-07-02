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

JJ$HOME_PHONE_NO[151]<-""
JJ$HOME_PHONE_NO[1129]<-"04-29928840"
JJ$HOME_PHONE_NO[1776]<-"02-29864976"
JJ$HOME_PHONE_NO[1777]<-"02-29864977"
JJ$HOME_PHONE_NO[2334]<-"02-82856678"
JJ$HOME_PHONE_NO[2657]<-"03-25278999"
JJ$HOME_PHONE_NO[3593]<-""
JJ$HOME_PHONE_NO[5013]<-"02-25917114"
JJ$HOME_PHONE_NO[8991]<-"0987780608"
JJ$HOME_PHONE_NO[12042]<-"04-22290312"
JJ$HOME_PHONE_NO[12043]<-"04-22290313"
JJ$HOME_PHONE_NO[12992]<-"04-22618085"
JJ$HOME_PHONE_NO[14175]<-"04-24624512"
JJ$HOME_PHONE_NO[14176]<-"04-24624513"
JJ$HOME_PHONE_NO[16084]<-"06-85970116"
JJ$HOME_PHONE_NO[16172]<-"02-23971400"
JJ$HOME_PHONE_NO[16346]<-"05-42275256"
JJ$HOME_PHONE_NO[16561]<-"03-84361719"
JJ$HOME_PHONE_NO[16567]<-""
JJ$HOME_PHONE_NO[20233]<-"07-07230060"
JJ$HOME_PHONE_NO[20536]<-""
JJ$HOME_PHONE_NO[20573]<-"03-08246458"
JJ$HOME_PHONE_NO[20615]<-"02-07882720"
JJ$HOME_PHONE_NO[25300]<-"04-22227877"
JJ$HOME_PHONE_NO[25614]<-"02-82513408"
JJ$HOME_PHONE_NO[25834]<-"02-23959825"
JJ$HOME_PHONE_NO[26341]<-"03-05527677"
JJ$HOME_PHONE_NO[26406]<-"03-05527678"
JJ$HOME_PHONE_NO[28308]<-"03-23298600"
JJ$HOME_PHONE_NO[28336]<-"04-22296887"
JJ$HOME_PHONE_NO[28502]<-"04-22296887"
JJ$HOME_PHONE_NO[28520]<-"0986138698"
JJ$HOME_PHONE_NO[28542]<-"03-27743050"
JJ$HOME_PHONE_NO[28600]<-"07-22915853"
JJ$HOME_PHONE_NO[31464]<-"07-27930613"
JJ$HOME_PHONE_NO[31932]<-"03-23631234"
JJ$HOME_PHONE_NO[34341]<-"04-22986461"
JJ$HOME_PHONE_NO[34344]<-"03-84361719"
JJ$HOME_PHONE_NO[34814]<-"02-26715560"
JJ$HOME_PHONE_NO[35780]<-"0921355201"
JJ$HOME_PHONE_NO[35842]<-"06-22328018"
JJ$HOME_PHONE_NO[35971]<-"04-23604823"
JJ$HOME_PHONE_NO[36029]<-"03-24818688"
JJ$HOME_PHONE_NO[36211]<-"02-22026032"
JJ$HOME_PHONE_NO[36219]<-'02-25238993'
JJ$HOME_PHONE_NO[36272]<-"0928770888"
JJ$HOME_PHONE_NO[36445]<-""
JJ$HOME_PHONE_NO[36532]<-""
JJ$HOME_PHONE_NO[36550]<-"06-22337906"
JJ$HOME_PHONE_NO[36579]<-"04-27269766"
JJ$HOME_PHONE_NO[36603]<-"0919280548"
JJ$HOME_PHONE_NO[36608]<-"02-25968872"
JJ$HOME_PHONE_NO[36609]<-"0933942299"
JJ$HOME_PHONE_NO[36783]<-"0932497679"
JJ$HOME_PHONE_NO[36908]<-"02-23032372"
JJ$HOME_PHONE_NO[36925]<-"0922792600"
JJ$HOME_PHONE_NO[37013]<-"04-22568779"
JJ$HOME_PHONE_NO[37039]<-"07-26224527"
JJ$HOME_PHONE_NO[37109]<-"0933818000"
JJ$HOME_PHONE_NO[37178]<-"0919763970"
JJ$HOME_PHONE_NO[37258]<-""
JJ$HOME_PHONE_NO[37261]<-""
JJ$HOME_PHONE_NO[37262]<-""
JJ$HOME_PHONE_NO[37375]<-"06-27861999"
JJ$HOME_PHONE_NO[37448]<-"03-24571112"
JJ$HOME_PHONE_NO[37464]<-"02-26980770"
JJ$HOME_PHONE_NO[37472]<-"04-26233808"
JJ$HOME_PHONE_NO[37473]<-"04-26233808"
JJ$HOME_PHONE_NO[37525]<-"0926395497"
JJ$HOME_PHONE_NO[37667]<-""
JJ$HOME_PHONE_NO[37721]<-"04-22953861"
JJ$HOME_PHONE_NO[37742]<-"03-2363852O"
JJ$HOME_PHONE_NO[37885]<-"06-22397591"
JJ$HOME_PHONE_NO[38044]<-"0912987736"
JJ$HOME_PHONE_NO[38404]<-"03-23363565"
JJ$HOME_PHONE_NO[39245]<-"0933424815"
JJ$HOME_PHONE_NO[39776]<-""
JJ$HOME_PHONE_NO[39984]<-""
JJ$HOME_PHONE_NO[40252]<-"02-24923778"

JJ$HOME_PHONE_NO[8572]<-"0926103802"
JJ$HOME_PHONE_NO[13187]<-"0966724318"
JJ$HOME_PHONE_NO[13188]<-"0966724318"
JJ$HOME_PHONE_NO[37648]<-"02-65839245"
JJ$HOME_PHONE_NO[37651]<-"02-52256593"

# Function hp -------------------------------------------------------------


JJ1<-JJ
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
  
  phone[nchar(phone) < 9] <- ""
  phone[nchar(phone) > 10] <- ""
  phone[nchar(phone) == 0] <- NA
  return(phone)
}
JJ1$HOME_PHONE_NO<-GGphone(JJ1$HOME_PHONE_NO)



# GG ----------------------------------------------------------------------

KKK<-JJ1[which(!nchar(JJ1$HOME_PHONE_NO) %in% c(0,10)),]
rm(KKK)

# seperate ----------------------------------------------------------------

JJ1 <- as_tibble(JJ1)
JJ1 <- JJ1%>% 
  mutate(ID = 1:40589)

JJ2 <- JJ1 %>%
  filter(!is.na(HOME_PHONE_NO))%>%
  mutate(hp = HOME_PHONE_NO)%>%
  separate(hp,into = c('ftwo','num'),sep=2,convert = T)


JJ2$ftwo<- as.factor(JJ2$ftwo)
summary(JJ2)


# Find error --------------------------------------------------------------


WrongID<-JJ2[which(JJ2$ftwo==0),]
WID<- WrongID$ID

JJ1$HOME_PHONE_NO[WID]<-NA 
# j=1
# for(i in WID){
#   JJ1$HOME_PHONE_NO[i] <- unlist(paste0('09',WrongID[j,"num"]))
#   j=j+1
# }

##run again

#   JJ2 <- JJ1 %>%
#   filter(!is.na(HOME_PHONE_NO))%>%
#   mutate(hp = HOME_PHONE_NO)%>%
#   separate(hp,into = c('ftwo','num'),sep=2,convert = T)
# 
# 
# JJ2$ftwo<- as.factor(JJ2$ftwo)
# summary(JJ2)


GG<-JJ2[which(JJ2$ftwo==8),]
GG2<-JJ2[which(!JJ2$ftwo%in%c(9,2,4,8,3,6)),]

#clean the 10 digits wrong numbers that don't start with 0
GG2$ftwo <- as.character(GG2$ftwo)
JJ1$HOME_PHONE_NO[GG2$ID[nchar(GG2$ftwo)==2]]<-NA 

##run again

#   JJ2 <- JJ1 %>%
#   filter(!is.na(HOME_PHONE_NO))%>%
#   mutate(hp = HOME_PHONE_NO)%>%
#   separate(hp,into = c('ftwo','num'),sep=2,convert = T)
# 
# 
# JJ2$ftwo<- as.factor(JJ2$ftwo)
# summary(JJ2)




# hp to M, hp to hp-----------------------------------------------------------------

JJ3<-JJ1
JJ3 <- JJ3%>%
  mutate(hp_M = HOME_PHONE_NO)%>%
  mutate(hp_hp = HOME_PHONE_NO)

JJ3$hp_M[JJ2$ID[which(JJ2$ftwo!=9)]] <- NA
JJ3$hp_hp[JJ2$ID[which(JJ2$ftwo==9)]] <- NA



# save data  --------------------------------------------------------------

JJ3 <- JJ3[,-c(1:3)]
write.csv(JJ3,file = 'hp.csv',row.names = F)

