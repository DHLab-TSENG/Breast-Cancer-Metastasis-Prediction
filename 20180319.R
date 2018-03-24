library(readr)
library(data.table)
library(ggplot2)
library(lubridate)
library(dplyr)
library(dtplyr)
library(tidyr)
library(XLConnect)
library(outliers)
library(ROCR)
library(caret)
library(tableone)

########################
#      data load       #
########################

excel.file_er <- file.path('HER2_total_raw_data.xls')
HER2_total <- readWorksheetFromFile(excel.file_er, sheet=1)
HER2_total <- data.table(HER2_total)
excel.file_er <- file.path('0123_HER2_1.xlsx')
HER2_1_1 <- readWorksheetFromFile(excel.file_er, sheet=1)
HER2_1_1 <- data.table(HER2_1_1)
excel.file_er <- file.path('0123_HER2_1.xlsx')
HER2_1_2 <- readWorksheetFromFile(excel.file_er, sheet=2)
HER2_1_2 <- data.table(HER2_1_2)
excel.file_er <- file.path('0123_HER2_2.xlsx')
HER2_2_1 <- readWorksheetFromFile(excel.file_er, sheet=1)
HER2_2_1 <- data.table(HER2_2_1)
excel.file_er <- file.path('0123_HER2_2.xlsx')
HER2_2_2 <- readWorksheetFromFile(excel.file_er, sheet=2)
HER2_2_2 <- data.table(HER2_2_2)
excel.file_er <- file.path('0123_HER2_3.xlsx')
HER2_3_1 <- readWorksheetFromFile(excel.file_er, sheet=1)
HER2_3_1 <- data.table(HER2_3_1)
excel.file_er <- file.path('0123_HER2_3.xlsx')
HER2_3_2 <- readWorksheetFromFile(excel.file_er, sheet=2)
HER2_3_2 <- data.table(HER2_3_2)
HER2_3_2<-subset(HER2_3_2,select= - Col42)

HER2_1 <- rbind(HER2_1_1,HER2_2_1,HER2_3_1,fill=TRUE)
rm(HER2_1_1,HER2_2_1,HER2_3_1)
nameHER2_1<-names(HER2_1_2)
nameHER2_2<-names(HER2_2_2)
nameHER2_3<-names(HER2_3_2)
NAME<-cbind(nameHER2_1,nameHER2_2, nameHER2_3)
NAME<- data.table(NAME)
rm(nameHER2_1,nameHER2_2,nameHER2_3)

setnames(HER2_2_2,c(1:41),NAME$nameHER2_1)
setnames(HER2_3_2,c(1:41),NAME$nameHER2_1)

HER2_2 <- rbind(HER2_1_2,HER2_2_2,HER2_3_2)
rm(HER2_1_2,HER2_2_2,HER2_3_2,NAME)

HER2_2$OP.Date <- as.character(HER2_2$OP.Date)
HER2_2[ID=="490637"]$OP.Date <- "20021201"
HER2_2[ID=="20183927"]$OP.Date <- "20051201"
HER2_2[ID=="377386"]$OP.Date <- "20010630"
HER2_2[ID=="1497884"]$OP.Date <- "20060615"
HER2_2[ID=="9868771"]$OP.Date <- "20070630"
HER2_2[ID=="21149670"]$OP.Date <- "20071115"

setnames(HER2_1,"病歷號","ID")
setnames(HER2_total,"CHTNO","ID")
HER2_2$ID<-as.character(HER2_2$ID)
setnames(HER2_1,c(2:6,10),c("CNM","LABNO","LABRESUVAL","Age","BRNDAT","RCVDAT"))
HER2_1<-HER2_1[,list(ID,LABNO,CNM,Sex="F",BRNDAT,RCVDAT,Age,LABNMABV="c-erbB-2",LABRESUVAL)]
names(HER2_total)
HER2_total<-subset(HER2_total,select = c(-OPDate,-ReDate, -CNM))
HER2_1<-subset(HER2_1,select = c( -CNM))
HER2_1$ID<-as.character(HER2_1$ID)
HER2_total<-merge(HER2_total,HER2_1,by=c("ID","LABNO","Sex","BRNDAT","RCVDAT","Age","LABNMABV","LABRESUVAL"),all = TRUE)
HER2_total<-HER2_total[!duplicated(HER2_total)]

data_merge_new <- merge(HER2_total,HER2_2,by="ID",all=TRUE)
rm(HER2_total,HER2_2,HER2_1)
data_merge_new$BRNDAT<-as.character(data_merge_new$BRNDAT)
data_merge_new[ID=="8884471"]$BRNDAT<-"1974/4/4"
data_merge_new[ID=="13591"]$BRNDAT<-"1943/01/27"
data_merge_new[ID=="21400"]$BRNDAT<-"1948/04/24"
data_merge_new[ID=="46896"]$BRNDAT<-"1953/12/14"
data_merge_new[grepl("HER|erbB",LABNMABV)]$LABNMABV<-'HER-2'

data_merge_new$OP.Date<-ymd(data_merge_new$OP.Date)
data_merge_new$RCVDAT<-ymd(data_merge_new$RCVDAT)
data_merge_new$OP_duration<-data_merge_new$RCVDAT-data_merge_new$OP.Date
data_merge_new$OP_duration<-as.numeric(data_merge_new$OP_duration)
data_merge_new[ID=="10668595"]$Re.Date<-"20110615" #201106
data_merge_new$Re.Date<-ymd(data_merge_new$Re.Date)
data_merge_new$LabPeriod<-''
data_merge_new[OP_duration>0]$LabPeriod<-"AfterOP"
data_merge_new[OP_duration<=0]$LabPeriod<-"BeforeOP"
data_merge_new[OP_duration>0&RCVDAT>=Re.Date]$LabPeriod<-"AfterRecur"

data_merge_new$IsRe<-F
data_merge_new[!is.na(Re.Date)]$IsRe<-T
data_merge_new$OP_duration<-as.numeric(data_merge_new$OP_duration)
data_merge_new$IsRe<-ifelse(data_merge_new$IsRe==T,"Recurrence","Non.Recurrence")
data_merge_new$RIA<-F
data_merge_new[grepl("(RIA)",LABRESUVAL)]$RIA<-T
data_merge_new$LABRESUVAL<-gsub("[(RIA)]", "", data_merge_new$LABRESUVAL)
data_merge_new$LABRESUVAL<-gsub("[<]", "", data_merge_new$LABRESUVAL)
data_merge_new$LABRESUVAL <- as.numeric(data_merge_new$LABRESUVAL)
data_merge_new$BRNDAT <- ymd(data_merge_new$BRNDAT)
data_merge_new$LabValue <- data_merge_new$LABRESUVAL
data_merge_new$LabName <- data_merge_new$LABNMABV
data_merge_new[LABNMABV=="HER-2"]$LabName <- "HER2"
data_merge_new[LABNMABV=="CA15-3"]$LabName <- "CA153"
data_merge_new[LABNMABV=="CEA"]$LabName <- "CEA"

data_merge_new<- data_merge_new[order(ID,LABNMABV,RCVDAT)]
data_merge_new$NO <- 1:8533
NewLastRecordBeforeRecur<-data_merge_new[LabPeriod=="AfterOP"][order(ID,RCVDAT)][, .SD[c(.N)], by=.(ID,LABNMABV)]
NewFirstRecordAfterOP<-data_merge_new[LabPeriod=="AfterOP"][order(ID,RCVDAT)][, .SD[c(1)], by=.(ID,LABNMABV)]
NewLastRecordBeforeRecur$IsLast <- T
NewFirstRecordAfterOP$IsFirst <- T
data_merge_new <- merge(data_merge_new, NewFirstRecordAfterOP ,by="NO", all.x= T, suffixes="")
data_merge_new <- data_merge_new[,c(1:55,110), with = FALSE]
data_merge_new <- merge(data_merge_new, NewLastRecordBeforeRecur ,by="NO", all.x= T, suffixes="")
data_merge_new <- data_merge_new[,c(1:56,111), with = FALSE]
Only <- data_merge_new[c(IsFirst=="TRUE" & IsLast=="TRUE")]
data_merge_new$LabTime<- ""
data_merge_new[NO %in% NewLastRecordBeforeRecur$NO ]$LabTime <- "Last" 
data_merge_new[NO %in% NewFirstRecordAfterOP$NO ]$LabTime <- "First"
data_merge_new[NO %in% Only$NO]$LabTime <- "Only"
rm(NewLastRecordBeforeRecur,NewFirstRecordAfterOP,Only)
data_merge_new[is.na(IsFirst)]$IsFirst<-F
data_merge_new[is.na(IsLast)]$IsLast<-F
#本來就有NA,不是轉numeric造成
data_merge_new$Tissue.ER<-as.numeric(data_merge_new$Tissue.ER)
data_merge_new$Tissue.PR<-as.numeric(data_merge_new$Tissue.PR)
data_merge_new$Tissue.HER2<-as.numeric(data_merge_new$Tissue.HER2)

###TNM processing
data_merge_new[pTi=='is']$pTi<-'1'
data_merge_new[pTi=='1c']$pTi<-'5'
data_merge_new[pTi=='2']$pTi<-'6'
data_merge_new[pTi=='3']$pTi<-'7'
data_merge_new[pTi=='4a']$pTi<-'8'
data_merge_new[pTi=='4b']$pTi<-'8'
data_merge_new[pTi=='4c']$pTi<-'8'
data_merge_new[pTi=='4d']$pTi<-'8'
data_merge_new[pTi=='x']$pTi<-'NA'
data_merge_new[pTi=='1micro']$pTi<-'2'
data_merge_new[pTi=='1a']$pTi<-'3'
data_merge_new[pTi=='1b']$pTi<-'4'
###End TNM processing

data_merge_new$pTi<-as.numeric(data_merge_new$pTi)
data_merge_new$pNi<-as.numeric(data_merge_new$pNi)
data_merge_new$pMi<-as.numeric(data_merge_new$pMi)
data_merge_new$IsRe<-factor(data_merge_new$IsRe,levels = c("Non.Recurrence","Recurrence"))


#days before the index date
data_merge_new <- data_merge_new[,c(1:58)]
Newest<-data_merge_new
Newest$Re.Date<-as.character(Newest$Re.Date)
Newest[ID=="2439637"]$Re.Date<-"20050404"
Newest[ID=="1976663"]$Re.Date<-"20140123"
Newest[ID=="1194468"]$Re.Date<-"20150507"
Newest$Re.Date<-ymd(Newest$Re.Date)
Newest$IsRe<-""
Newest$IsRe<-"Non.Recurrence"
Newest[!is.na(Re.Date)]$IsRe<-"Recurrence"
Newest$IsRe<-factor(Newest$IsRe,levels = c("Non.Recurrence","Recurrence"))
Newest<-Newest[,c(1,2,3,5,6,7,10,26:28,30:32,42,46,51,52,54,55,56,57,58)]
Newest<-Newest[!duplicated(Newest,by=c( "ID","LABNO","LabName"))]
Newest[OP_duration>0&RCVDAT>=Re.Date]$LabPeriod<-"AfterRecur"
Newest<-Newest[,c(1:19)]
NewLastRecordBeforeRecur<-Newest[LabPeriod=="AfterOP"][order(ID,RCVDAT)][, .SD[c(.N)], by=.(ID,LabName)]
NewFirstRecordAfterOP<-Newest[LabPeriod=="AfterOP"][order(ID,RCVDAT)][, .SD[c(1)], by=.(ID,LabName)]
NewLastRecordBeforeRecur$IsLast <- T
NewFirstRecordAfterOP$IsFirst <- T
Newest <- merge(Newest, NewFirstRecordAfterOP ,by="NO", all.x= T, suffixes="")
Newest <- Newest[,c(1:19,38), with = FALSE]
Newest <- merge(Newest, NewLastRecordBeforeRecur ,by="NO", all.x= T, suffixes="")
Newest <- Newest[,c(1:20,39), with = FALSE]
Only <- Newest[c(IsFirst=="TRUE" & IsLast=="TRUE")]
Newest$LabTime<- ""
Newest[NO %in% NewLastRecordBeforeRecur$NO ]$LabTime <- "Last" 
Newest[NO %in% NewFirstRecordAfterOP$NO ]$LabTime <- "First"
Newest[NO %in% Only$NO]$LabTime <- "Only"
rm(NewLastRecordBeforeRecur,NewFirstRecordAfterOP,Only)
Newest[is.na(IsFirst)]$IsFirst<-F
Newest[is.na(IsLast)]$IsLast<-F

IsLast<-Newest[LabPeriod=="AfterOP"][IsLast=="TRUE"][order(ID,RCVDAT)][, .SD[c(.N)], by=ID]
IsLast$IndexDAT<-IsLast$RCVDAT
IsLast$IndexDAT<-ymd(IsLast$IndexDAT)
IsLast[IsRe=="Recurrence"]$IndexDAT<-IsLast[IsRe=="Recurrence"]$Re.Date
IsLast<-IsLast[,list(ID,IndexDAT)]


Newest<-merge(Newest,IsLast,by="ID",all=T)
Newest$Index_duration<-interval(start = Newest$RCVDAT, 
                                        end = Newest$IndexDAT) / duration(num = 1, units = "days")

Newest<-merge(Newest,data_merge_new,by="NO",all.x = T, suffixes="")
Newest<-Newest[,list(ID,NO,LABNO,Sex,BRNDAT,RCVDAT,Age,LABNMABV, LABRESUVAL,
                     OP_duration,PreTx.test,X1st.test,X2nd.test,X3rd.test,
                     X4th.test,X5th.test,X.,Correlation,Slope,Interception,
                     Ti,Ni,Mi,TNM,neoadjuvant.therapy,pTi,pNi,pMi,pTNM,
                     Tissue.ER,Tissue.PR,Tissue.HER2,HER2.FISH,ER..,
                     faint.intermediate.,PR.,Col28,HER2.,Col30,Diagnosis,
                     Site,OP.Date,OP,PostOP.C.T,Col36,Re.Date,
                     Re.type,Time.to.Progression,Last.date.of.OPD.follow.up.,
                     Censored..5yr.,LabPeriod,IsRe,RIA,LabValue,LabName,IsFirst,
                     IsLast,LabTime,IndexDAT,Index_duration)]
rm(data_merge_new,IsLast)
#days before the index date end

########################
#      table one       #
########################
options(scipen = 999)

#Patients had at least one follow-up visit record 
#between first operation date and the index date (n=206)
Newest$OPAge<-interval(start = Newest$BRNDAT, 
                               end = Newest$OP.Date) / duration(num = 1, units = "years")
Newest <- 
  Newest[,c(1,2,5,6,7,21:32,42,46,51,52,54,55,56,57,59,60,61), with = FALSE][!duplicated(Newest)]
Newest <- Newest[,c(1:5,11:13,15:28), with = FALSE]
Newest <- Newest[LabPeriod=="AfterOP"]

over0days <- Newest[Index_duration>0]
over0days <- over0days[order(ID,LabName,-RCVDAT)][,.SD[c(1)],by=.(ID,LabName)]
CA153_Last <- over0days[LabName=="CA153"]
CA153_Last$CA153 <- CA153_Last$LabValue
CA153_Last <- CA153_Last[,c(1,5,23),with=FALSE]
CEA_Last <- over0days[LabName=="CEA"]
CEA_Last$CEA <- CEA_Last$LabValue
CEA_Last <- CEA_Last[,c(1,5,23),with=FALSE]
HER2_Last <- over0days[LabName=="HER2"]
HER2_Last$HER2 <- HER2_Last$LabValue
HER2_Last <- HER2_Last[,c(1,5,23),with=FALSE]

LastRecordAfterOP <- over0days[order(ID,-RCVDAT)][,.SD[c(1)],by=ID]
LastRecordAfterOP <- 
  LastRecordAfterOP[,list(ID, BRNDAT, OPAge, pTi, pNi, pMi, Tissue.ER, Tissue.PR, Tissue.HER2, IsRe, Index_duration,IndexDAT)]
LastRecordAfterOP <- merge(LastRecordAfterOP,CA153_Last,all.x = T,by="ID")
LastRecordAfterOP <- merge(LastRecordAfterOP,CEA_Last,all.x = T,by="ID")
LastRecordAfterOP <- merge(LastRecordAfterOP,HER2_Last,all.x = T,by="ID")

LastRecordAfterOP <- LastRecordAfterOP[ID!="1798196"] #特例 移除


rm(over0days,CA153_Last,CEA_Last,HER2_Last)

vars <- c("OPAge","pTi","pNi","pMi","Tissue.ER","Tissue.PR","Tissue.HER2","CA153","CEA","HER2","Index_duration") 
cols <- c("pTi", "pNi", "pMi", "Tissue.ER", "Tissue.PR", "Tissue.HER2")
ALL_tableOne <- CreateTableOne(vars = vars, strata = c("IsRe"),factorVars = cols, data = LastRecordAfterOP) 
ALL_tableOne <- data.table(as.matrix(print(ALL_tableOne,nonnormal= "Index_duration")),keep.rownames = T)
#table one end

########################
#  data preprocessing  #
########################

#Patients included in predictive model development (n=144)
#90days before the index date
over90days <- Newest[Index_duration>90]
over90days <- over90days[order(ID,LabName,-RCVDAT)][,.SD[c(1)],by=.(ID,LabName)]
CA153_Last <- over90days[LabName=="CA153"]
CA153_Last$CA153 <- CA153_Last$LabValue
CA153_Last <- CA153_Last[,c(1,5,23),with=FALSE]
CEA_Last <- over90days[LabName=="CEA"]
CEA_Last$CEA <- CEA_Last$LabValue
CEA_Last <- CEA_Last[,c(1,5,23),with=FALSE]
HER2_Last <- over90days[LabName=="HER2"]
HER2_Last$HER2 <- HER2_Last$LabValue
HER2_Last <- HER2_Last[,c(1,5,23),with=FALSE]

LastRecordAfterOP_90 <- over90days[order(ID,-RCVDAT)][,.SD[c(1)],by=ID]
LastRecordAfterOP_90 <- 
  LastRecordAfterOP_90[,list(ID, BRNDAT, OPAge, pTi, pNi, pMi, Tissue.ER, Tissue.PR, Tissue.HER2, IsRe, Index_duration,IndexDAT)]
LastRecordAfterOP_90 <- merge(LastRecordAfterOP_90,CA153_Last,all.x = T,by="ID")
LastRecordAfterOP_90 <- merge(LastRecordAfterOP_90,CEA_Last,all.x = T,by="ID")
LastRecordAfterOP_90 <- merge(LastRecordAfterOP_90,HER2_Last,all.x = T,by="ID")

LastRecordAfterOP_90_rmna <- LastRecordAfterOP_90[!is.na(CA153)&!is.na(CEA)&!is.na(HER2)&!is.na(pTi)&!is.na(pNi)&!is.na(pMi)
                                                  &!is.na(Tissue.ER)&!is.na(Tissue.PR)&!is.na(Tissue.HER2)]
LastRecordAfterOP_90_rmna <- LastRecordAfterOP_90_rmna[ID!="1798196"] #特例 移除
rm(over90days,CA153_Last,CEA_Last,HER2_Last,LastRecordAfterOP_90)

cols <- c("pTi", "pNi", "pMi", "Tissue.ER", "Tissue.PR", "Tissue.HER2")
LastRecordAfterOP_90_rmna<-data.frame(LastRecordAfterOP_90_rmna)
LastRecordAfterOP_90_rmna[cols] <- lapply(LastRecordAfterOP_90_rmna[cols], factor)
LastRecordAfterOP_90_rmna<-data.table(LastRecordAfterOP_90_rmna)
LastRecordAfterOP_90_rmna[IsRe=="Non-Recurrence"]$IsRe<-"Non.Recurrence"
LastRecordAfterOP_90_rmna$IsRe<-factor(LastRecordAfterOP_90_rmna$IsRe,levels = c("Non.Recurrence","Recurrence"))
LastRecordAfterOP_90_rmna_IsRe<-LastRecordAfterOP_90_rmna[IsRe=="Recurrence"]
LastRecordAfterOP_90_rmna_NotRe<-LastRecordAfterOP_90_rmna[IsRe=="Non.Recurrence"]
#3-fold data
smp_size <- floor(0.33 * nrow(LastRecordAfterOP_90_rmna_IsRe))
set.seed(3231)
A_ind <- sample(seq_len(nrow(LastRecordAfterOP_90_rmna_IsRe)), size = smp_size)
rmna_IsRe_A <- LastRecordAfterOP_90_rmna_IsRe[A_ind, ]
rmna_IsRe_BC <- LastRecordAfterOP_90_rmna_IsRe[-A_ind, ]
smp_size <- floor(0.5 * nrow(rmna_IsRe_BC))
set.seed(3231)
B_ind <- sample(seq_len(nrow(rmna_IsRe_BC)), size = smp_size)
rmna_IsRe_B <- rmna_IsRe_BC[B_ind, ]
rmna_IsRe_C <- rmna_IsRe_BC[-B_ind, ]
rm(rmna_IsRe_BC)

smp_size <- floor(0.34 * nrow(LastRecordAfterOP_90_rmna_NotRe))
set.seed(3231)
A_ind <- sample(seq_len(nrow(LastRecordAfterOP_90_rmna_NotRe)), size = smp_size)
rmna_NotRe_A <- LastRecordAfterOP_90_rmna_NotRe[A_ind, ]
rmna_NotRe_BC <- LastRecordAfterOP_90_rmna_NotRe[-A_ind, ]
smp_size <- floor(0.5 * nrow(rmna_NotRe_BC))
set.seed(3231)
B_ind <- sample(seq_len(nrow(rmna_NotRe_BC)), size = smp_size)
rmna_NotRe_B <- rmna_NotRe_BC[B_ind, ]
rmna_NotRe_C <- rmna_NotRe_BC[-B_ind, ]
rm(rmna_NotRe_BC)
rm(LastRecordAfterOP_90_rmna_IsRe,LastRecordAfterOP_90_rmna_NotRe)

testing_1<-rbind(rmna_IsRe_A,rmna_NotRe_A)
testing_2<-rbind(rmna_IsRe_B,rmna_NotRe_B)
testing_3<-rbind(rmna_IsRe_C,rmna_NotRe_C)

rm(rmna_IsRe_A,rmna_NotRe_A,rmna_IsRe_B,rmna_NotRe_B,rmna_IsRe_C,rmna_NotRe_C)

training_1<-rbind(testing_2,testing_3)
training_2<-rbind(testing_1,testing_3)
training_3<-rbind(testing_1,testing_2)

training_1$training<-"1"
training_2$training<-"2"
training_3$training<-"3"
training_90<-rbind(training_1,training_2,training_3)

testing_1$testing<-"1"
testing_2$testing<-"2"
testing_3$testing<-"3"
testing_90<-rbind(testing_1,testing_2,testing_3)

rm(training_1,training_2,training_3,testing_1,testing_2,testing_3)
#3-fold data end

#60days before the index date
over60days <- Newest[Index_duration>60]
over60days <- over60days[order(ID,LabName,-RCVDAT)][,.SD[c(1)],by=.(ID,LabName)]
CA153_Last <- over60days[LabName=="CA153"]
CA153_Last$CA153 <- CA153_Last$LabValue
CA153_Last <- CA153_Last[,c(1,5,23),with=FALSE]
CEA_Last <- over60days[LabName=="CEA"]
CEA_Last$CEA <- CEA_Last$LabValue
CEA_Last <- CEA_Last[,c(1,5,23),with=FALSE]
HER2_Last <- over60days[LabName=="HER2"]
HER2_Last$HER2 <- HER2_Last$LabValue
HER2_Last <- HER2_Last[,c(1,5,23),with=FALSE]

LastRecordAfterOP_60 <- over60days[order(ID,-RCVDAT)][,.SD[c(1)],by=ID]
LastRecordAfterOP_60 <- 
  LastRecordAfterOP_60[,list(ID, BRNDAT, OPAge, pTi, pNi, pMi, Tissue.ER, Tissue.PR, Tissue.HER2, IsRe, Index_duration,IndexDAT)]
LastRecordAfterOP_60 <- merge(LastRecordAfterOP_60,CA153_Last,all.x = T,by="ID")
LastRecordAfterOP_60 <- merge(LastRecordAfterOP_60,CEA_Last,all.x = T,by="ID")
LastRecordAfterOP_60 <- merge(LastRecordAfterOP_60,HER2_Last,all.x = T,by="ID")

LastRecordAfterOP_60_rmna <- LastRecordAfterOP_60[!is.na(CA153)&!is.na(CEA)&!is.na(HER2)&!is.na(pTi)&!is.na(pNi)&!is.na(pMi)
                                                  &!is.na(Tissue.ER)&!is.na(Tissue.PR)&!is.na(Tissue.HER2)]
LastRecordAfterOP_60_rmna <- LastRecordAfterOP_60_rmna[ID!="1798196"] #特例 移除
rm(over60days,CA153_Last,CEA_Last,HER2_Last,LastRecordAfterOP_60)

cols <- c("pTi", "pNi", "pMi", "Tissue.ER", "Tissue.PR", "Tissue.HER2")
LastRecordAfterOP_60_rmna<-data.frame(LastRecordAfterOP_60_rmna)
LastRecordAfterOP_60_rmna[cols] <- lapply(LastRecordAfterOP_60_rmna[cols], factor)
LastRecordAfterOP_60_rmna<-data.table(LastRecordAfterOP_60_rmna)
LastRecordAfterOP_60_rmna[IsRe=="Non-Recurrence"]$IsRe<-"Non.Recurrence"
LastRecordAfterOP_60_rmna$IsRe<-factor(LastRecordAfterOP_60_rmna$IsRe,levels = c("Non.Recurrence","Recurrence"))
LastRecordAfterOP_60_rmna_IsRe<-LastRecordAfterOP_60_rmna[IsRe=="Recurrence"]
LastRecordAfterOP_60_rmna_NotRe<-LastRecordAfterOP_60_rmna[IsRe=="Non.Recurrence"]
#3-fold data
smp_size <- floor(0.33 * nrow(LastRecordAfterOP_60_rmna_IsRe))
set.seed(3231)
A_ind <- sample(seq_len(nrow(LastRecordAfterOP_60_rmna_IsRe)), size = smp_size)
rmna_IsRe_A <- LastRecordAfterOP_60_rmna_IsRe[A_ind, ]
rmna_IsRe_BC <- LastRecordAfterOP_60_rmna_IsRe[-A_ind, ]
smp_size <- floor(0.5 * nrow(rmna_IsRe_BC))
set.seed(3231)
B_ind <- sample(seq_len(nrow(rmna_IsRe_BC)), size = smp_size)
rmna_IsRe_B <- rmna_IsRe_BC[B_ind, ]
rmna_IsRe_C <- rmna_IsRe_BC[-B_ind, ]
rm(rmna_IsRe_BC)

smp_size <- floor(0.34 * nrow(LastRecordAfterOP_60_rmna_NotRe))
set.seed(3231)
A_ind <- sample(seq_len(nrow(LastRecordAfterOP_60_rmna_NotRe)), size = smp_size)
rmna_NotRe_A <- LastRecordAfterOP_60_rmna_NotRe[A_ind, ]
rmna_NotRe_BC <- LastRecordAfterOP_60_rmna_NotRe[-A_ind, ]
smp_size <- floor(0.5 * nrow(rmna_NotRe_BC))
set.seed(3231)
B_ind <- sample(seq_len(nrow(rmna_NotRe_BC)), size = smp_size)
rmna_NotRe_B <- rmna_NotRe_BC[B_ind, ]
rmna_NotRe_C <- rmna_NotRe_BC[-B_ind, ]
rm(rmna_NotRe_BC)
rm(LastRecordAfterOP_60_rmna_IsRe,LastRecordAfterOP_60_rmna_NotRe)

testing_1<-rbind(rmna_IsRe_A,rmna_NotRe_A)
testing_2<-rbind(rmna_IsRe_B,rmna_NotRe_B)
testing_3<-rbind(rmna_IsRe_C,rmna_NotRe_C)

rm(rmna_IsRe_A,rmna_NotRe_A,rmna_IsRe_B,rmna_NotRe_B,rmna_IsRe_C,rmna_NotRe_C)

training_1<-rbind(testing_2,testing_3)
training_2<-rbind(testing_1,testing_3)
training_3<-rbind(testing_1,testing_2)

training_1$training<-"1"
training_2$training<-"2"
training_3$training<-"3"
training_60<-rbind(training_1,training_2,training_3)

testing_1$testing<-"1"
testing_2$testing<-"2"
testing_3$testing<-"3"
testing_60<-rbind(testing_1,testing_2,testing_3)

rm(training_1,training_2,training_3,testing_1,testing_2,testing_3)
#3-fold data end

#30days before the index date
over30days <- Newest[Index_duration>30]
over30days <- over30days[order(ID,LabName,-RCVDAT)][,.SD[c(1)],by=.(ID,LabName)]
CA153_Last <- over30days[LabName=="CA153"]
CA153_Last$CA153 <- CA153_Last$LabValue
CA153_Last <- CA153_Last[,c(1,5,23),with=FALSE]
CEA_Last <- over30days[LabName=="CEA"]
CEA_Last$CEA <- CEA_Last$LabValue
CEA_Last <- CEA_Last[,c(1,5,23),with=FALSE]
HER2_Last <- over30days[LabName=="HER2"]
HER2_Last$HER2 <- HER2_Last$LabValue
HER2_Last <- HER2_Last[,c(1,5,23),with=FALSE]

LastRecordAfterOP_30 <- over30days[order(ID,-RCVDAT)][,.SD[c(1)],by=ID]
LastRecordAfterOP_30 <- 
  LastRecordAfterOP_30[,list(ID, BRNDAT, OPAge, pTi, pNi, pMi, Tissue.ER, Tissue.PR, Tissue.HER2, IsRe, Index_duration,IndexDAT)]
LastRecordAfterOP_30 <- merge(LastRecordAfterOP_30,CA153_Last,all.x = T,by="ID")
LastRecordAfterOP_30 <- merge(LastRecordAfterOP_30,CEA_Last,all.x = T,by="ID")
LastRecordAfterOP_30 <- merge(LastRecordAfterOP_30,HER2_Last,all.x = T,by="ID")

LastRecordAfterOP_30_rmna <- LastRecordAfterOP_30[!is.na(CA153)&!is.na(CEA)&!is.na(HER2)&!is.na(pTi)&!is.na(pNi)&!is.na(pMi)
                                                  &!is.na(Tissue.ER)&!is.na(Tissue.PR)&!is.na(Tissue.HER2)]
LastRecordAfterOP_30_rmna <- LastRecordAfterOP_30_rmna[ID!="1798196"] #特例 移除
rm(over30days,CA153_Last,CEA_Last,HER2_Last,LastRecordAfterOP_30)

cols <- c("pTi", "pNi", "pMi", "Tissue.ER", "Tissue.PR", "Tissue.HER2")
LastRecordAfterOP_30_rmna<-data.frame(LastRecordAfterOP_30_rmna)
LastRecordAfterOP_30_rmna[cols] <- lapply(LastRecordAfterOP_30_rmna[cols], factor)
LastRecordAfterOP_30_rmna<-data.table(LastRecordAfterOP_30_rmna)
LastRecordAfterOP_30_rmna[IsRe=="Non-Recurrence"]$IsRe<-"Non.Recurrence"
LastRecordAfterOP_30_rmna$IsRe<-factor(LastRecordAfterOP_30_rmna$IsRe,levels = c("Non.Recurrence","Recurrence"))
LastRecordAfterOP_30_rmna_IsRe<-LastRecordAfterOP_30_rmna[IsRe=="Recurrence"]
LastRecordAfterOP_30_rmna_NotRe<-LastRecordAfterOP_30_rmna[IsRe=="Non.Recurrence"]
#3-fold data
smp_size <- floor(0.33 * nrow(LastRecordAfterOP_30_rmna_IsRe))
set.seed(3231)
A_ind <- sample(seq_len(nrow(LastRecordAfterOP_30_rmna_IsRe)), size = smp_size)
rmna_IsRe_A <- LastRecordAfterOP_30_rmna_IsRe[A_ind, ]
rmna_IsRe_BC <- LastRecordAfterOP_30_rmna_IsRe[-A_ind, ]
smp_size <- floor(0.5 * nrow(rmna_IsRe_BC))
set.seed(3231)
B_ind <- sample(seq_len(nrow(rmna_IsRe_BC)), size = smp_size)
rmna_IsRe_B <- rmna_IsRe_BC[B_ind, ]
rmna_IsRe_C <- rmna_IsRe_BC[-B_ind, ]
rm(rmna_IsRe_BC)

smp_size <- floor(0.34 * nrow(LastRecordAfterOP_30_rmna_NotRe))
set.seed(3231)
A_ind <- sample(seq_len(nrow(LastRecordAfterOP_30_rmna_NotRe)), size = smp_size)
rmna_NotRe_A <- LastRecordAfterOP_30_rmna_NotRe[A_ind, ]
rmna_NotRe_BC <- LastRecordAfterOP_30_rmna_NotRe[-A_ind, ]
smp_size <- floor(0.5 * nrow(rmna_NotRe_BC))
set.seed(3231)
B_ind <- sample(seq_len(nrow(rmna_NotRe_BC)), size = smp_size)
rmna_NotRe_B <- rmna_NotRe_BC[B_ind, ]
rmna_NotRe_C <- rmna_NotRe_BC[-B_ind, ]
rm(rmna_NotRe_BC)
rm(LastRecordAfterOP_30_rmna_IsRe,LastRecordAfterOP_30_rmna_NotRe)

testing_1<-rbind(rmna_IsRe_A,rmna_NotRe_A)
testing_2<-rbind(rmna_IsRe_B,rmna_NotRe_B)
testing_3<-rbind(rmna_IsRe_C,rmna_NotRe_C)

rm(rmna_IsRe_A,rmna_NotRe_A,rmna_IsRe_B,rmna_NotRe_B,rmna_IsRe_C,rmna_NotRe_C)

training_1<-rbind(testing_2,testing_3)
training_2<-rbind(testing_1,testing_3)
training_3<-rbind(testing_1,testing_2)

training_1$training<-"1"
training_2$training<-"2"
training_3$training<-"3"
training_30<-rbind(training_1,training_2,training_3)

testing_1$testing<-"1"
testing_2$testing<-"2"
testing_3$testing<-"3"
testing_30<-rbind(testing_1,testing_2,testing_3)

rm(training_1,training_2,training_3,testing_1,testing_2,testing_3)
#3-fold data end
#data preprocessing end

########################
#   predictive model   #
########################

#################### 舊資料.rds ####################
#training_90<-readRDS("training_rm1798196_90.rds")
#testing_90<-readRDS("testing_rm1798196_90.rds")
####################################################

#Logistic regression
#90days before the index date
glm_auc_90=list(0,0,0)
Glm_SSPN_90<-NULL
for (k in 1:50){
  for (i in 1:3){
    set.seed(k+3231)
    Glm <- train(IsRe ~  OPAge + pTi + pNi + Tissue.ER + Tissue.PR + Tissue.HER2 + CA153 + CEA + HER2,
                 data = downSample(training_90[training==i],training_90[training==i]$IsRe),
                 method = "glmStepAIC", metric = "ROC",
                 family=binomial("logit"), 
                 trControl = trainControl(method = "cv", number = 3, 
                                          classProbs=TRUE, summaryFunction = twoClassSummary),trace = FALSE)
    Glm_pr = predict(Glm, type="prob", newdata=testing_90[testing==i])[,2]
    Glm_pred = prediction(Glm_pr,testing_90[testing==i]$IsRe)
    Glm_temp<-data.table(tp=Glm_pred@tp[[1]],fp=Glm_pred@fp[[1]],
                         tn=Glm_pred@tn[[1]],fn=Glm_pred@fn[[1]])
    temp<-data.table(folds=i, times=k)
    Glm_temp<-cbind(temp,Glm_temp)
    Glm_temp$sen <- Glm_temp$tp/(Glm_temp$fn+Glm_temp$tp)
    Glm_temp$spe <- Glm_temp$tn/(Glm_temp$fp+Glm_temp$tn)
    Glm_temp$ppv <- Glm_temp$tp/(Glm_temp$tp+Glm_temp$fp)
    Glm_temp$npv <- Glm_temp$tn/(Glm_temp$fn+Glm_temp$tn)
    Glm_SSPN_90<-rbind(Glm_SSPN_90,Glm_temp)
    Glm_AUC = performance(Glm_pred,"auc")
    glm_auc_90[[i]][k]<-Glm_AUC@y.values[[1]]
  }
}   
glm_auc_90<-c(glm_auc_90[[1]],glm_auc_90[[2]],glm_auc_90[[3]])

#60days before the index date
glm_auc_60=list(0,0,0)
Glm_SSPN_60<-NULL

for (k in 1:50){
  for (i in 1:3){
    set.seed(k+3231)
    Glm <- train(IsRe ~  OPAge + pTi + pNi + Tissue.ER + Tissue.PR + Tissue.HER2 + CA153 + CEA + HER2,
                 data = downSample(training_60[training==i],training_60[training==i]$IsRe),
                 method = "glmStepAIC", metric = "ROC",
                 family=binomial("logit"), 
                 trControl = trainControl(method = "cv", number = 3, 
                                          classProbs=TRUE, summaryFunction = twoClassSummary))
    Glm_pr = predict(Glm, type="prob", newdata=testing_60[testing==i])[,2]
    Glm_pred = prediction(Glm_pr,testing_60[testing==i]$IsRe)
    Glm_temp<-data.table(tp=Glm_pred@tp[[1]],fp=Glm_pred@fp[[1]],
                         tn=Glm_pred@tn[[1]],fn=Glm_pred@fn[[1]])
    temp<-data.table(folds=i, times=k)
    Glm_temp<-cbind(temp,Glm_temp)
    Glm_temp$sen <- Glm_temp$tp/(Glm_temp$fn+Glm_temp$tp)
    Glm_temp$spe <- Glm_temp$tn/(Glm_temp$fp+Glm_temp$tn)
    Glm_temp$ppv <- Glm_temp$tp/(Glm_temp$tp+Glm_temp$fp)
    Glm_temp$npv <- Glm_temp$tn/(Glm_temp$fn+Glm_temp$tn)
    Glm_SSPN_60<-rbind(Glm_SSPN_60,Glm_temp)
    Glm_AUC = performance(Glm_pred,"auc")
    glm_auc_60[[i]][k]<-Glm_AUC@y.values[[1]]
  }
} 
glm_auc_60<-c(glm_auc_60[[1]],glm_auc_60[[2]],glm_auc_60[[3]])

#30days before the index date
glm_auc_30=list(0,0,0)
Glm_SSPN_30<-NULL

for (k in 1:50){
  for (i in 1:3){
    set.seed(k+3231)
    Glm <- train(IsRe ~  OPAge + pTi + pNi + Tissue.ER + Tissue.PR + Tissue.HER2 + CA153 + CEA + HER2,
                 data = downSample(training_30[training==i],training_30[training==i]$IsRe),
                 method = "glmStepAIC", metric = "ROC",
                 family=binomial("logit"), 
                 trControl = trainControl(method = "cv", number = 3, 
                                          classProbs=TRUE, summaryFunction = twoClassSummary))
    Glm_pr = predict(Glm, type="prob", newdata=testing_30[testing==i])[,2]
    Glm_pred = prediction(Glm_pr,testing_30[testing==i]$IsRe)
    Glm_temp<-data.table(tp=Glm_pred@tp[[1]],fp=Glm_pred@fp[[1]],
                         tn=Glm_pred@tn[[1]],fn=Glm_pred@fn[[1]])
    temp<-data.table(folds=i, times=k)
    Glm_temp<-cbind(temp,Glm_temp)
    Glm_temp$sen <- Glm_temp$tp/(Glm_temp$fn+Glm_temp$tp)
    Glm_temp$spe <- Glm_temp$tn/(Glm_temp$fp+Glm_temp$tn)
    Glm_temp$ppv <- Glm_temp$tp/(Glm_temp$tp+Glm_temp$fp)
    Glm_temp$npv <- Glm_temp$tn/(Glm_temp$fn+Glm_temp$tn)
    Glm_SSPN_30<-rbind(Glm_SSPN_30,Glm_temp)
    Glm_AUC = performance(Glm_pred,"auc")
    glm_auc_30[[i]][k]<-Glm_AUC@y.values[[1]]
  }
}
glm_auc_30<-c(glm_auc_30[[1]],glm_auc_30[[2]],glm_auc_30[[3]])
rm(temp,Glm_temp)

#Naive bayes
#90days before the index date
nb_auc_90=list(0,0,0)
Nb_SSPN_90<-NULL

for (k in 1:50){
  for (i in 1:3){
    set.seed(k+3231)
    Nb <- train(IsRe ~  OPAge + pTi + pNi + Tissue.ER + Tissue.PR + Tissue.HER2 + CA153 + CEA + HER2 ,
                data = downSample(training_90[training==i],training_90[training==i]$IsRe),
                method = "naive_bayes", metric = "ROC",
                trControl = trainControl(method = "cv", number = 3, 
                                         classProbs=TRUE, summaryFunction = twoClassSummary),
                tuneGrid = data.frame(fL=0,usekernel=F, adjust=F))
    Nb_pr = predict(Nb, type="prob", newdata=testing_90[testing==i])[,2]
    Nb_pred = prediction(Nb_pr,testing_90[testing==i]$IsRe)
    temp<-data.table(folds=i, times=k)
    Nb_temp<-data.table(tp=Nb_pred@tp[[1]],fp=Nb_pred@fp[[1]],
                        tn=Nb_pred@tn[[1]],fn=Nb_pred@fn[[1]])
    Nb_temp<-cbind(temp,Nb_temp)
    Nb_temp$sen <- Nb_temp$tp/(Nb_temp$fn+Nb_temp$tp)
    Nb_temp$spe <- Nb_temp$tn/(Nb_temp$fp+Nb_temp$tn)
    Nb_temp$ppv <- Nb_temp$tp/(Nb_temp$tp+Nb_temp$fp)
    Nb_temp$npv <- Nb_temp$tn/(Nb_temp$fn+Nb_temp$tn)
    Nb_SSPN_90<-rbind(Nb_SSPN_90, Nb_temp)
    Nb_AUC = performance(Nb_pred,"auc")
    Nb_AUC@y.values[[1]]
    nb_auc_90[[i]][k]<-Nb_AUC@y.values[[1]]
    print(i)
    print(k)
  }
}
nb_auc_90<-c(nb_auc_90[[1]],nb_auc_90[[2]],nb_auc_90[[3]])

#60days before the index date
nb_auc_60=list(0,0,0)
Nb_SSPN_60<-NULL

for (k in 1:50){
  for (i in 1:3){
    set.seed(k+3231)
    Nb <- train(IsRe ~  OPAge + pTi + pNi + Tissue.ER + Tissue.PR + Tissue.HER2 + CA153 + CEA + HER2 ,
                data = downSample(training_60[training==i],training_60[training==i]$IsRe),
                method = "naive_bayes", metric = "ROC",
                trControl = trainControl(method = "cv", number = 3, 
                                         classProbs=TRUE, summaryFunction = twoClassSummary),
                tuneGrid = data.frame(fL=0,usekernel=F, adjust=F))
    Nb_pr = predict(Nb, type="prob", newdata=testing_60[testing==i])[,2]
    Nb_pred = prediction(Nb_pr,testing_60[testing==i]$IsRe)
    temp<-data.table(folds=i, times=k)
    Nb_temp<-data.table(tp=Nb_pred@tp[[1]],fp=Nb_pred@fp[[1]],
                        tn=Nb_pred@tn[[1]],fn=Nb_pred@fn[[1]])
    Nb_temp<-cbind(temp,Nb_temp)
    Nb_temp$sen <- Nb_temp$tp/(Nb_temp$fn+Nb_temp$tp)
    Nb_temp$spe <- Nb_temp$tn/(Nb_temp$fp+Nb_temp$tn)
    Nb_temp$ppv <- Nb_temp$tp/(Nb_temp$tp+Nb_temp$fp)
    Nb_temp$npv <- Nb_temp$tn/(Nb_temp$fn+Nb_temp$tn)
    Nb_SSPN_60<-rbind(Nb_SSPN_60, Nb_temp)
    Nb_AUC = performance(Nb_pred,"auc")
    Nb_AUC@y.values[[1]]
    nb_auc_60[[i]][k]<-Nb_AUC@y.values[[1]]
    print(i)
    print(k)
  }
}
nb_auc_60<-c(nb_auc_60[[1]],nb_auc_60[[2]],nb_auc_60[[3]])

#30days before the index date
nb_auc_30=list(0,0,0)
Nb_SSPN_30<-NULL

for (k in 1:50){
  for (i in 1:3){
    set.seed(k+3231)
    Nb <- train(IsRe ~  OPAge + pTi + pNi + Tissue.ER + Tissue.PR + Tissue.HER2 + CA153 + CEA + HER2 ,
                data = downSample(training_30[training==i],training_30[training==i]$IsRe),
                method = "naive_bayes", metric = "ROC",
                trControl = trainControl(method = "cv", number = 3, 
                                         classProbs=TRUE, summaryFunction = twoClassSummary),
                tuneGrid = data.frame(fL=0,usekernel=F, adjust=F))
    Nb_pr = predict(Nb, type="prob", newdata=testing_30[testing==i])[,2]
    Nb_pred = prediction(Nb_pr,testing_30[testing==i]$IsRe)
    temp<-data.table(folds=i, times=k)
    Nb_temp<-data.table(tp=Nb_pred@tp[[1]],fp=Nb_pred@fp[[1]],
                        tn=Nb_pred@tn[[1]],fn=Nb_pred@fn[[1]])
    Nb_temp<-cbind(temp,Nb_temp)
    Nb_temp$sen <- Nb_temp$tp/(Nb_temp$fn+Nb_temp$tp)
    Nb_temp$spe <- Nb_temp$tn/(Nb_temp$fp+Nb_temp$tn)
    Nb_temp$ppv <- Nb_temp$tp/(Nb_temp$tp+Nb_temp$fp)
    Nb_temp$npv <- Nb_temp$tn/(Nb_temp$fn+Nb_temp$tn)
    Nb_SSPN_30<-rbind(Nb_SSPN_30, Nb_temp)
    Nb_AUC = performance(Nb_pred,"auc")
    Nb_AUC@y.values[[1]]
    nb_auc_30[[i]][k]<-Nb_AUC@y.values[[1]]
    print(i)
    print(k)
  }
}
nb_auc_30<-c(nb_auc_30[[1]],nb_auc_30[[2]],nb_auc_30[[3]])
rm(temp,Nb_temp)

#random forest
#90days before the index date
rf_auc_90=list(0,0,0)
Rf_SSPN_90<-NULL
RF_import_90<-NULL
RF_TREES_90<-NULL
trc<-trainControl(method = "cv", number = n_folds, 
                  classProbs=TRUE, summaryFunction = twoClassSummary)

for (k in 1:50){
  for (i in 1:3){
    set.seed(k+seed)
    Rf <- train(IsRe ~  OPAge + pTi + pNi + Tissue.ER + Tissue.PR + Tissue.HER2 + CA153 + CEA + HER2,
                data = downSample(training_90[training==i],training_90[training==i]$IsRe),
                method = "rf", metric = "ROC", importance=TRUE,
                trControl = trc,
                tuneGrid = expand.grid(mtry = c(1:24)))
    Rf_pr = predict(Rf, type="prob", newdata=testing_90[testing==i])[,2]
    Rf_pred = prediction(Rf_pr,testing_90[testing==i]$IsRe)
    temp<-data.table(folds=i, times=k)
    Rf_temp<-data.table(tp=Rf_pred@tp[[1]],fp=Rf_pred@fp[[1]],
                        tn=Rf_pred@tn[[1]],fn=Rf_pred@fn[[1]])
    Rf_temp<-cbind(temp,Rf_temp)
    Rf_temp$sen <- Rf_temp$tp/(Rf_temp$fn+Rf_temp$tp)
    Rf_temp$spe <- Rf_temp$tn/(Rf_temp$fp+Rf_temp$tn)
    Rf_temp$ppv <- Rf_temp$tp/(Rf_temp$tp+Rf_temp$fp)
    Rf_temp$npv <- Rf_temp$tn/(Rf_temp$fn+Rf_temp$tn)
    Rf_SSPN_90<-rbind(Rf_SSPN_90,Rf_temp)
    TREE<-data.table(getTree(Rf$finalModel,labelVar = TRUE))
    TREE<-cbind(temp,TREE)
    RF_TREES_90<-rbind(RF_TREES_90,TREE)
    Rf_AUC = performance(Rf_pred,"auc")
    rf_auc_90[[i]][k]<-Rf_AUC@y.values[[1]]
    TEMP<-data.table(data.frame(Rf$finalModel$importance),keep.rownames = T)
    import<-cbind(temp,TEMP)
    RF_import_90<-rbind(RF_import_90,import)
    print(i)
    print(k)
  }
}   
rf_auc_90<-c(rf_auc_90[[1]],rf_auc_90[[2]],rf_auc_90[[3]])
mean(rf_auc_90)

#60days before the index date
rf_auc_60=list(0,0,0)
Rf_SSPN_60<-NULL
RF_import_60<-NULL
RF_TREES_60<-NULL

for (k in 1:50){
  for (i in 1:3){
    set.seed(k+3231)
    Rf <- train(IsRe ~  OPAge + pTi + pNi + Tissue.ER + Tissue.PR + Tissue.HER2 + CA153 + CEA + HER2,
                data = downSample(training_60[training==i],training_60[training==i]$IsRe),
                method = "rf", metric = "ROC", importance=TRUE,
                trControl = trainControl(method = "cv", number = 3,
                                         classProbs=TRUE, summaryFunction = twoClassSummary),
                tuneGrid = expand.grid(mtry = c(1:24)))
    Rf_pr = predict(Rf, type="prob", newdata=testing_60[testing==i])[,2]
    Rf_pred = prediction(Rf_pr,testing_60[testing==i]$IsRe)
    temp<-data.table(folds=i, times=k)
    Rf_temp<-data.table(tp=Rf_pred@tp[[1]],fp=Rf_pred@fp[[1]],
                        tn=Rf_pred@tn[[1]],fn=Rf_pred@fn[[1]])
    Rf_temp<-cbind(temp,Rf_temp)
    Rf_temp$sen <- Rf_temp$tp/(Rf_temp$fn+Rf_temp$tp)
    Rf_temp$spe <- Rf_temp$tn/(Rf_temp$fp+Rf_temp$tn)
    Rf_temp$ppv <- Rf_temp$tp/(Rf_temp$tp+Rf_temp$fp)
    Rf_temp$npv <- Rf_temp$tn/(Rf_temp$fn+Rf_temp$tn)
    Rf_SSPN_60<-rbind(Rf_SSPN_60,Rf_temp)
    TREE<-data.table(getTree(Rf$finalModel,labelVar = TRUE))
    TREE<-cbind(temp,TREE)
    RF_TREES_60<-rbind(RF_TREES_60,TREE)
    Rf_AUC = performance(Rf_pred,"auc")
    rf_auc_60[[i]][k]<-Rf_AUC@y.values[[1]]
    TEMP<-data.table(data.frame(Rf$finalModel$importance),keep.rownames = T)
    import<-cbind(temp,TEMP)
    RF_import_60<-rbind(RF_import_60,import)
    print(i)
    print(k)
  }
}   
rf_auc_60<-c(rf_auc_60[[1]],rf_auc_60[[2]],rf_auc_60[[3]])

#30days before the index date
rf_auc_30=list(0,0,0)
Rf_SSPN_30<-NULL
RF_import_30<-NULL
RF_TREES_30<-NULL

for (k in 1:50){
  for (i in 1:3){
    set.seed(k+3231)
    Rf <- train(IsRe ~  OPAge + pTi + pNi + Tissue.ER + Tissue.PR + Tissue.HER2 + CA153 + CEA + HER2,
                data = downSample(training_30[training==i],training_30[training==i]$IsRe),
                method = "rf", metric = "ROC", importance=TRUE,
                trControl = trainControl(method = "cv", number = 3,
                                         classProbs=TRUE, summaryFunction = twoClassSummary),
                tuneGrid = expand.grid(mtry = c(1:24)))
    Rf_pr = predict(Rf, type="prob", newdata=testing_30[testing==i])[,2]
    Rf_pred = prediction(Rf_pr,testing_30[testing==i]$IsRe)
    temp<-data.table(folds=i, times=k)
    Rf_temp<-data.table(tp=Rf_pred@tp[[1]],fp=Rf_pred@fp[[1]],
                        tn=Rf_pred@tn[[1]],fn=Rf_pred@fn[[1]])
    Rf_temp<-cbind(temp,Rf_temp)
    Rf_temp$sen <- Rf_temp$tp/(Rf_temp$fn+Rf_temp$tp)
    Rf_temp$spe <- Rf_temp$tn/(Rf_temp$fp+Rf_temp$tn)
    Rf_temp$ppv <- Rf_temp$tp/(Rf_temp$tp+Rf_temp$fp)
    Rf_temp$npv <- Rf_temp$tn/(Rf_temp$fn+Rf_temp$tn)
    Rf_SSPN_30<-rbind(Rf_SSPN_30,Rf_temp)
    TREE<-data.table(getTree(Rf$finalModel,labelVar = TRUE))
    TREE<-cbind(temp,TREE)
    RF_TREES_30<-rbind(RF_TREES_30,TREE)
    Rf_AUC = performance(Rf_pred,"auc")
    rf_auc_30[[i]][k]<-Rf_AUC@y.values[[1]]
    TEMP<-data.table(data.frame(Rf$finalModel$importance),keep.rownames = T)
    import<-cbind(temp,TEMP)
    RF_import_30<-rbind(RF_import_30,import)
    print(i)
    print(k)
  }
}   
rf_auc_30<-c(rf_auc_30[[1]],rf_auc_30[[2]],rf_auc_30[[3]])
rm(temp,TEMP,TREE,import)
#predictive model end

########################
#        results       #
########################

AUC<-rbind(data.table(Model="GLM",Days_before="90",AUC=glm_auc_90),
           data.table(Model="GLM",Days_before="60",AUC=glm_auc_60),
           data.table(Model="GLM",Days_before="30",AUC=glm_auc_30),
           data.table(Model="NB",Days_before="90",AUC=nb_auc_90),
           data.table(Model="NB",Days_before="60",AUC=nb_auc_60),
           data.table(Model="NB",Days_before="30",AUC=nb_auc_30),
           data.table(Model="RF",Days_before="90",AUC=rf_auc_90),
           data.table(Model="RF",Days_before="60",AUC=rf_auc_60),
           data.table(Model="RF",Days_before="30",AUC=rf_auc_30))

AUCDF<-AUC %>% group_by(Model,Days_before) %>% summarise(Mean=round(mean(AUC),digit=3),SE=round(sd(AUC)/150,digit=4))

# ANOVA # RF > NB > GLM

summary(aov(AUC~Model,data=AUC[Days_before=="90"]))
TukeyHSD(aov(AUC~Model,data=AUC[Days_before=="90"]))

summary(aov(AUC~Model,data=AUC[Days_before=="60"]))
TukeyHSD(aov(AUC~Model,data=AUC[Days_before=="60"]))

summary(aov(AUC~Model,data=AUC[Days_before=="30"]))
TukeyHSD(aov(AUC~Model,data=AUC[Days_before=="30"]))
# ANOVA end

# Time varied # 90 days before > 60 > 30
summary(aov(AUC~Days_before,data=AUC[Model=="RF"]))
TukeyHSD(aov(AUC~Days_before,data=AUC[Model=="RF"]))

# Feature importance # 90 days before # RF
# Mean Decrease Gini
RF_import_90[,.(MeanDecreaseGini=mean(MeanDecreaseGini)),by=(rn)][order(-MeanDecreaseGini)]

# Number of times being the split variable     
RF_TREES_90[,.N,by=`split var`][order(-N)]
# Feature importance end

# Sen > 0.70, Spe # Sen=0.80, spe=0.62
Rf_sen_75 <- Rf_SSPN_90[sen>=0.70 & spe>0][order(sen)][, .SD[1], by=.(folds,times)]

Rf_sen_75 %>% summarize(model="Rf",sen = mean(sen), spe=mean(spe), ppv=mean(ppv), npv=mean(npv))


