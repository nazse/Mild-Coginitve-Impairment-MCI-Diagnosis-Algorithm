####################################################################################################
# This script is perform Mild cognitive Impairment (MCI)                                           #
#                                                                                                  #
# Classification according to Petersen's criteria and Winblad's criteria.                          #
#                                                                                                  #
# The data files are from Gothenburg H85-2015 Examination.                                         #
#                                                                                                  #
# All input files are based on Research questionnaires and were loaded from the respective files   #
#                                                                                                  #
# -Named hereinafter referred to as 'formulars' - as follows:                                      #
#                                                                                                  #
# Psykiatri, Kognition, Anhorig, ADL and Dementia diagnosis file to exclude dementias at baseline. # 
#                                                                                                  #
# A new education variable based on available longitudinal info 2000-2015 from participants and    #
#                                                                                                  #
# Their key informants was computed by Simona Sacuiu for the                                       #
#                                                                                                  #
# MCI class: H85-2015_Kognition(2019-04-15)_bearbetatSFS20190703-EDUC.SAV                          #
#                                                                                                  #
# Script Documentation Start Date: 11-07-2019                                                      #
####################################################################################################



########################################## Load Package into directory: For loop and apply functions
install.packages('dplyr', dependencies = T)
library(dplyr)
list.packages <- c('dplyr','devtools','sjmisc','digest','gmodels','haven','Hmisc',
                   'psych','purrr','pastecs','lubridate','installr','Rcpp','tibble')

options(warn=0) ## turn Off Warninga

for(lib in list.packages){ # load packages in library
  library(lib, require, character.only = TRUE)
  devtools::install_github("strengejacke/sjmisc")
}
#######################################################################################################



########################################################################## Set up the working directory
PATH <- "X:/Nazib/Documentations/Simona/MCI_H85_2015"
setwd(PATH) 
################################################ Use getwd() to access the path to the working directory



####################################################### Load H85-2015 data files into Global Environment
H85.Formulars.path <- file.path(PATH, c("H85-2015_Psykiatri(2018-12-17).SAV",
                                        "H85-2015_Kognition(2018-11-30).SAV", 
                                        "H85-2015_ANH(2018-12-07).SAV",
                                        "H85-2015_ADL(2018-11-30).SAV",
                                        "Demens_H85-KVUS_2015.sav",
                                        "H85-2015_Kognition(2019-04-15)_bearbetatSFS20190703-EDUC.SAV"))

DataFrames <- lapply(H85.Formulars.path, read_sav) ####### Load  tuples of data frame for each formular

names(DataFrames) <- gsub(".*/(.*)\\..*", "\\1", H85.Formulars.path) ### Unpack names of data frames

str(DataFrames, give.attr = FALSE) ### view the structure of d again
#########################################################################################################



############ Data extraction for each formular and variables of interest in Global data frame (DataFrames)

############### Psychiatry Formular Variables:
psyk.data<- as.data.frame(select(DataFrames[[1]],LopNr,KOG1,KOG5,KOG6,KOG9,PSF8,F25DAT))

############### Kognition Formular Variables:
kog.data<- as.data.frame(select(DataFrames[[2]],LopNr,MIN1,MIN2,MIN3,MIN6,MIN7,MIN8,MIN9,MIN10,MIN11,
                                MIN12,MIN13,MIN14,MIN15,MIN16,MIN25,MIN32,MIN17,MIN18,MIN35,MIN37,MIN66,
                                MIN44I,MIN45,MIN46,MIN47,MIN48,MIN49,MIN50,DEM1,DEM2,DEM3,DEM4,DEM5,
                                STA22A,STA23A,STA24A))

############### Anhörig Formular Variables:
anh.data<- as.data.frame(select(DataFrames[[3]],LopNr,AHDATUM,PRA5B,INT4,INT6,INT7,INT10,
                                INT13,INT19,PRAK1,PRA5A,SPRA1,SPRA2,SPRA3,SPRA4,MIN1ANH,
                                MIN8ANH,MIN6ANH,MIN10ANH,MIN12ANH,MIN14ANH,MIN13ANH,MIN16ANH,
                                MIN18ANH,MIN19ANH,MIN20ANH,MIN21ANH,MIN5ANH))

############### ADL Formular Variables:
adl.data<- as.data.frame(select(DataFrames[[4]],LopNr,ADLTE,ADLTF,ADLTG,ADLTH,ADLTJ,ADLTA,
                                ADLTB,ADLTC,ADLTD,ADLTK,ADLTL,ADLTM,ADLTN,ADLTO))

############### H85-2015 Dementia Diagnosis variable retrieve from Hanna W. (Sample Size in file = 491)
dem.data<- as.data.frame(select(DataFrames[[5]],LopNr,Dem2015)) 

############### Simona Computed Education Variables Data Frame:
edu.data<- as.data.frame(select(DataFrames[[6]],LopNr,Sex,educ_sfs))
#########################################################################################################



#########################  Merge all data into a DataFrame and Exclude all Dementia Cases ( DEM2015 != 1)
MCI.Data.Update<- data.frame(list(psyk.data, kog.data, anh.data, adl.data,dem.data,edu.data) %>% 
                               reduce(left_join, by = "LopNr") %>%
                               filter(Dem2015 != 1))
#########################################################################################################



################  Replace character Observations with Numeric Inputs and Missing Observations as "Missing"
options(warn = -1)
MCI.Data.Update[]<-lapply(MCI.Data.Update[], as.numeric)
MCI.Data.Update<-MCI.Data.Update%>% replace(is.na(.), "Missing")
############### Subset the data frame to use in the end of the code to extract raw file and write to file
MCI.Data.Update.last<- data.frame(MCI.Data.Update)
#########################################################################################################



############## Complaint of Defective Memory(corroborated by knowledgeable informant- optional): SRMemImp
MCI.Data.Update<-MCI.Data.Update%>% 
  mutate(SRMemImp=ifelse(KOG1=="Missing","Missing",ifelse( KOG1>2 ,"Yes","No")))

SRMemImp.KOG<- select(MCI.Data.Update, LopNr,SRMemImp)

######### Inspect SRMemImp Cross Table Result 
CrossTable(MCI.Data.Update$SRMemImp)
#########################################################################################################



#################################################################################### SRCogIMP Computation
KOG_Cols<- select(MCI.Data.Update,KOG1,KOG5,KOG6,KOG9)%>% names()

MCI.Data.Update <- MCI.Data.Update %>% 
mutate(SRCogIMP=ifelse(pmin(!!!rlang::syms(KOG_Cols))=="Missing","Missing",
ifelse(pmin(!!!rlang::syms(KOG_Cols)) =="Missing" & pmax(!!!rlang::syms(KOG_Cols)) >2,"No",
ifelse(pmax(!!!rlang::syms(KOG_Cols)) =="Missing" & pmin(!!!rlang::syms(KOG_Cols)) <=2,"No",
ifelse(pmax(!!!rlang::syms(KOG_Cols)) <=2 ,"No",
ifelse(pmax(!!!rlang::syms(KOG_Cols)) =="Missing"|pmin(!!!rlang::syms(KOG_Cols)) >2,"Yes",
ifelse(pmax(!!!rlang::syms(KOG_Cols))>2,"Yes","No")))))))


SR_Mem_Data<- select(MCI.Data.Update, LopNr, SRCogIMP)
######## Inspect SRCogImp Cross Table Result 
CrossTable(MCI.Data.Update$SRCogIMP)
#########################################################################################################



##################################################### Data Cleaning in Specific Individuals and Variables
MCI.Data.Update$AHDATUM[MCI.Data.Update$LopNr=="11346"] <- 20170103
MCI.Data.Update$MIN14ANH[MCI.Data.Update$LopNr=="6248"] <- 4
MCI.Data.Update$PRA5B[MCI.Data.Update$LopNr=="6436"] <- 3
MCI.Data.Update$PRAK1[MCI.Data.Update$LopNr=="6693"] <- 3
MCI.Data.Update$MIN21ANH[MCI.Data.Update$LopNr=="6796"] <- 3
MCI.Data.Update$MIN21ANH[MCI.Data.Update$LopNr=="7626"] <- 3
MCI.Data.Update$PRA5B[MCI.Data.Update$LopNr=="7948"] <- 3
MCI.Data.Update$PRA5A[MCI.Data.Update$LopNr=="11094"] <- 3
MCI.Data.Update$MIN12ANH[MCI.Data.Update$MIN12ANH==9]<-"Missing"
MCI.Data.Update$MIN13ANH[MCI.Data.Update$MIN13ANH==9]<-"Missing"
MCI.Data.Update$MIN18ANH[MCI.Data.Update$MIN18ANH==9]<-"Missing"
MCI.Data.Update$MIN5ANH[MCI.Data.Update$MIN5ANH==9]<-"Missing"
MCI.Data.Update$MIN14ANH[MCI.Data.Update$MIN14ANH==9]<-"Missing"
MCI.Data.Update$PRA5B[MCI.Data.Update$PRA5B==9]<-"Missing"
MCI.Data.Update$PRA5A[MCI.Data.Update$PRA5A==9]<-"Missing"
MCI.Data.Update$PRAK1[MCI.Data.Update$PRAK1==9]<-"Missing"
MCI.Data.Update$SPRA4[MCI.Data.Update$SPRA4==0]<-3
MCI.Data.Update$INT4[MCI.Data.Update$LopNr=="6653"] <- "Missing"
MCI.Data.Update$INT4[MCI.Data.Update$LopNr=="6846"] <- 3
MCI.Data.Update$INT7[MCI.Data.Update$LopNr=="7874"] <- 3
MCI.Data.Update$INT10[MCI.Data.Update$LopNr=="6674"] <- 4
MCI.Data.Update$INT10[MCI.Data.Update$LopNr=="11086"] <- 4
MCI.Data.Update$INT13[MCI.Data.Update$LopNr=="10897"] <- 3
MCI.Data.Update$INT13[MCI.Data.Update$LopNr=="14882"] <- 3
MCI.Data.Update$INT13[MCI.Data.Update$LopNr=="7660"] <- 3
MCI.Data.Update$INT19[MCI.Data.Update$LopNr=="6226"] <- 3
MCI.Data.Update$INT19[MCI.Data.Update$LopNr=="6430"] <- 3
MCI.Data.Update$INT19[MCI.Data.Update$LopNr=="6889"] <- 3
MCI.Data.Update$MIN14ANH[MCI.Data.Update$LopNr=="6248"] <- 4
MCI.Data.Update$SPRA3[MCI.Data.Update$LopNr=="6229"] <- 3
MCI.Data.Update$PRA5B[MCI.Data.Update$LopNr=="6436"] <- 3
MCI.Data.Update$PRAK1[MCI.Data.Update$LopNr=="6693"] <- 3
MCI.Data.Update$MIN21ANH[MCI.Data.Update$LopNr=="6796"] <- 3
MCI.Data.Update$MIN21ANH[MCI.Data.Update$LopNr=="7626"] <- 3
MCI.Data.Update$PRA5B[MCI.Data.Update$LopNr=="7948"] <- 3
MCI.Data.Update$PRA5A[MCI.Data.Update$LopNr=="11094"] <- 3
MCI.Data.Update$INT4[MCI.Data.Update$INT4 == 9]<- "Missing"
MCI.Data.Update$INT6[MCI.Data.Update$INT6 == 9]<- "Missing"
MCI.Data.Update$INT7[MCI.Data.Update$INT7 == 9]<- "Missing"
MCI.Data.Update$INT10[MCI.Data.Update$INT10 == 9]<- "Missing"
MCI.Data.Update$INT19[MCI.Data.Update$INT19 == 9]<- "Missing"
MCI.Data.Update$SPRA1[MCI.Data.Update$SPRA1 == 9]<- "Missing"
MCI.Data.Update$SPRA2[MCI.Data.Update$SPRA2 == 9]<- "Missing"
MCI.Data.Update$SPRA3[MCI.Data.Update$SPRA3 == 9]<- "Missing"
MCI.Data.Update$SPRA4[MCI.Data.Update$SPRA4 == 9]<- "Missing"
#######################################################################################################



######################################## Change dates (F25DAT and AHDATUM) into machine readible format
MCI.Data.Update$F25DAT<- ymd(MCI.Data.Update$F25DAT)
MCI.Data.Update$AHDATUM<- ymd(MCI.Data.Update$AHDATUM)

##### Convert Date to DayDiffPS_KI between time points 
MCI.Data.Update$DayDiffPS_KI<-as.character(difftime(as.Date(MCI.Data.Update$AHDATUM),
                                                    as.Date(MCI.Data.Update$F25DAT),unit="days"))

##### Change Character to Numeric and Store in a data frame
MCI.Data.Update[]<-lapply(MCI.Data.Update[], as.numeric)
MCI.Data.Update<- data.frame(MCI.Data.Update)
#########################################################################################################



################################# COMBINE SubKogImp and SR_IQMemImp BASED ON  DayDiffCat USING DEFINITION
# DayDiffPS_KI grouping function
#0 		='Samma dag '
#1		='1-183 dagar '
#2		='184-365 dagar '
#3 		='365-1000 dagar'
#OTHER 	='**************';
####################################################### For loop to compute DayDiffCat (cut-off variable) 
for(i in 1:nrow(MCI.Data.Update)){  # DayDiffPS_KI grouping function
  if(!is.na(MCI.Data.Update$DayDiffPS_KI)[i] <=0){ 
    MCI.Data.Update$DayDiffCat[i]='Missing'}
  else if(MCI.Data.Update$DayDiffPS_KI[i]>= 1 && MCI.Data.Update$DayDiffPS_KI[i]<184){
    MCI.Data.Update$DayDiffCat[i]=1}
  else if(MCI.Data.Update$DayDiffPS_KI[i]>= 184 && MCI.Data.Update$DayDiffPS_KI[i]<366){
    MCI.Data.Update$DayDiffCat[i]=2}
  else if(MCI.Data.Update$DayDiffPS_KI[i]>= 365 && MCI.Data.Update$DayDiffPS_KI[i]<1001){
    MCI.Data.Update$DayDiffCat[i]=3}
  else {MCI.Data.Update$DayDiffCat[i]=0}
}


##### Inspect Computed Day Difference Groups
CrossTable(MCI.Data.Update$DayDiffCat)
########################################################################################################





######################################################################  16-items Memory IQCODE Computation

IQCODE.1.Variables<-c("MIN1ANH","MIN10ANH","MIN12ANH","MIN13ANH","MIN14ANH","MIN16ANH","MIN18ANH",
                      "MIN19ANH","MIN20ANH","MIN21ANH","MIN5ANH","MIN6ANH","MIN8ANH","PRAK1","PRA5A",
                      "PRA5B")


MCI.Data.Update<-MCI.Data.Update%>% 
  mutate(IQmemAvg=rep(((14*3)+(2*4))/length(IQCODE.1.Variables),nrow(MCI.Data.Update)),
                                           IQmemIndiv=rowMeans(select(.,IQCODE.1.Variables),na.rm=TRUE),
                                           IQmemBin=ifelse(IQmemIndiv>IQmemAvg, "Yes","No"))
#########################################################################################################



######################## Weighing the effect of number of missing to compute IQCODEMEM-16 Item:  19-06-19  
MCI.Data.Update$N_Miss_1 <- rowSums( is.na(MCI.Data.Update[,IQCODE.1.Variables]))
MCI.Data.Update <- row_count(MCI.Data.Update, IQCODE.1.Variables, count = 4, append = TRUE)
MCI.Data.Update <- row_count(MCI.Data.Update, IQCODE.1.Variables, count = 5, append = TRUE)
MCI.Data.Update<- mutate(MCI.Data.Update, IQmemBin_2 = ifelse(rowcount > 2 | rowcount1>0,"Yes","No"))
MCI.Data.Update<- mutate(MCI.Data.Update, Miss_1= 16 - N_Miss_1)
#########################################################################################################



#######  Computing IQCODESBIN accounting for number of Missing in each person data and final IQCODEBINMEM
for(i in 1:nrow(MCI.Data.Update)){
  TotIQcode_1 = length(IQCODE.1.Variables)
  if(MCI.Data.Update$N_Miss_1[i] > 0){ ###25th percental
    MCI.Data.Update$IQCodeBin_Miss_1[i]=(((TotIQcode_1 - MCI.Data.Update$N_Miss_1[i] - 2)*3)+(2*4))/
      (TotIQcode_1 - MCI.Data.Update$N_Miss_1[i])}
  
  else {MCI.Data.Update$IQCodeBin_Miss_1[i]=(((TotIQcode_1 -MCI.Data.Update$N_Miss_1[i] - 2)*3)+(2*4))/
    (TotIQcode_1 - MCI.Data.Update$N_Miss_1[i])}
}

##### IQCODEMEM-16 ltems Mem Final 
options(warn = -1)
MCI.Data.Update$IQmemIndiv[MCI.Data.Update$IQmemIndiv == "NaN"]<- 'NA'
MCI.Data.Update$IQCodeBin_Miss_1[MCI.Data.Update$IQCodeBin_Miss_1 == "Inf"]<- 'NA'
MCI.Data.Update<- mutate(MCI.Data.Update, 
                        IQCODEBINMEM_FINAL = ifelse(IQmemIndiv=="NA" & IQCodeBin_Miss_1 =='NA','Missing',
                                                    ifelse(IQmemIndiv > IQCodeBin_Miss_1, "Yes","No")))

##### Inspect IQCODEBINMEM FINAL
CrossTable(MCI.Data.Update$IQCODEBINMEM_FINAL)
#########################################################################################################




############################################################################  26-items IQCODE Computation
# NOTE:variables MIN7 from ANHÖRIG FORMULÄR 2000 ??? MIN18ANH and MIN15 from ANHÖRIG FORMULÄR 2000 MIN16ANH

IQCODE.2.Variables<-c("MIN1ANH","MIN5ANH","MIN6ANH","MIN8ANH","MIN10ANH","MIN12ANH","MIN13ANH","MIN14ANH",
                      "MIN16ANH","MIN18ANH","MIN19ANH","MIN20ANH","MIN21ANH","PRAK1","PRA5A","PRA5B",
                      "INT4","INT6","INT7","INT10","INT13","INT19","SPRA1","SPRA2" ,"SPRA3","SPRA4") 

#MCI.Data.Update[]<-lapply(MCI.Data.Update[], as.numeric)


#MCI.Data.Update<- data.frame(MCI.Data.Update)
                  

MCI.Data.Update<-MCI.Data.Update%>% 
  mutate(IQcodeAvg=rep(((24*3)+(2*4))/length(IQCODE.2.Variables),nrow(MCI.Data.Update)),
                                           IQcodeIndiv=rowMeans(select(.,IQCODE.2.Variables),na.rm=TRUE),
                                           IQcodeBin=ifelse(IQcodeIndiv>IQcodeAvg, "Yes","No"))
#########################################################################################################


###################################### Weighing the effect of number of missing to compute IQCODE-26 Item:

##### Compute Number of Missing for Individual and counts
MCI.Data.Update<- select(MCI.Data.Update, -N_Miss_1,-rowcount,-rowcount1)
MCI.Data.Update$N_Miss_2 <- rowSums( is.na( MCI.Data.Update[,IQCODE.2.Variables]))
MCI.Data.Update <- row_count(MCI.Data.Update, IQCODE.2.Variables, count = 4, append = TRUE)
MCI.Data.Update <- row_count(MCI.Data.Update, IQCODE.2.Variables, count = 5, append = TRUE)
MCI.Data.Update<- mutate(MCI.Data.Update, IQcodeBin_2 = ifelse(rowcount > 2 | rowcount1>0,"Yes","No"))
MCI.Data.Update<- mutate(MCI.Data.Update, Miss= 26 - N_Miss_2)
#########################################################################################################




######## Function for final computation of IQCODE Binary taken into account the number of missing inputs
for(i in 1:nrow(MCI.Data.Update)){
  TotIQcode <- length(IQCODE.2.Variables)
  if(MCI.Data.Update$N_Miss_2[i] > 0){ ###25th percental
    MCI.Data.Update$IQCodeBin_Miss_2[i]=(((TotIQcode - MCI.Data.Update$N_Miss_2[i] - 2)*3)+(2*4))/
      (TotIQcode - MCI.Data.Update$N_Miss_2[i])}
  
  else {MCI.Data.Update$IQCodeBin_Miss_2[i]=(((TotIQcode - MCI.Data.Update$N_Miss_2[i] - 2)*3)+(2*4))/
    (TotIQcode - MCI.Data.Update$N_Miss_2[i])}
}

options(warn = -1)
MCI.Data.Update$IQcodeIndiv[MCI.Data.Update$IQcodeIndiv == "NaN"]<- 'NA'
MCI.Data.Update$IQCodeBin_Miss_2[MCI.Data.Update$IQCodeBin_Miss_2== "Inf"]<- 'NA'
MCI.Data.Update<- mutate(MCI.Data.Update, IQCODEBIN_FINAL =ifelse(IQcodeIndiv=="NA" 
                & IQCodeBin_Miss_2 =='NA','Missing',ifelse(IQcodeIndiv > IQCodeBin_Miss_2, "Yes","No")))

##### Inspect IQCODEBIN FINAL
CrossTable(MCI.Data.Update$IQCODEBIN_FINAL)
#########################################################################################################



########################################################################################################
#colnames(MCI.Data.Update)
MCI.Data.Update<- select(MCI.Data.Update, -SRCogIMP,-SRMemImp)

MCI.Data.Update.j<- data.frame(list(MCI.Data.Update,SRMemImp.KOG,SR_Mem_Data) %>% 
                                 reduce(left_join, by = "LopNr")) 

MCI.Data.Update<-MCI.Data.Update.j%>% 
  mutate(SubKogImp =ifelse(DayDiffCat =="Missing",'Missing',
                    ifelse(DayDiffCat <3 & (SRCogIMP =="Yes" | IQCODEBIN_FINAL =="Yes"),"Yes",
                    ifelse(DayDiffCat <3 & (SRCogIMP =="No" & IQCODEBIN_FINAL =="Yes"),"Yes",
                    ifelse(DayDiffCat ==3 & SRCogIMP =="Yes"& IQCODEBIN_FINAL =="No","Yes","No")))),
      SR_IQMemImp =ifelse(DayDiffCat == 'Missing','Missing',
                   ifelse(DayDiffCat <3 & (SRMemImp =="Yes" | IQCODEBINMEM_FINAL =="Yes"),"Yes",
                   ifelse(DayDiffCat <3 & (SRMemImp =="No" & IQCODEBINMEM_FINAL =="Yes"),"Yes",
                   ifelse(DayDiffCat >= 3 & SRMemImp =="Yes","Yes","No")))))


##### Updated information 
MCI.Data.Update$SubKogImp <- ifelse(MCI.Data.Update$SubKogImp =='Missing', 
                                    MCI.Data.Update$SRCogIMP, MCI.Data.Update$SubKogImp)
MCI.Data.Update$SR_IQMemImp <- ifelse(MCI.Data.Update$SR_IQMemImp =='Missing',
                                      MCI.Data.Update$SRMemImp, MCI.Data.Update$SR_IQMemImp)
######################################################################################################


#############################  NormalGenKog(Normal General Cognitive Function MMSE.Value of MIN18 >23)


### Adjust with both Handicap and zero: According to standardized Mini-Mental State Examination (MMSE)
############################################# DR. Molloy Weighted Approach for those who are Handicap.
MCI.Data.Update$MIN7[MCI.Data.Update$LopNr ==10382] <- 3
MCI.Data.Update$MIN87[MCI.Data.Update$LopNr ==10382] <- 19
options(warn = -1)
MCI.Data.Update$MIN_adjust_Handicap_Zero<- 'NA' 
MCI.Data.Update$MIN_adjust_Handicap_Zero[MCI.Data.Update$LopNr ==11366] <- (28*30)/(30-1)
MCI.Data.Update$MIN_adjust_Handicap_Zero[MCI.Data.Update$LopNr ==11083] <- (28 *30)/(30-1)
MCI.Data.Update$MIN_adjust_Handicap_Zero[MCI.Data.Update$LopNr ==10098] <- (22*30)/(30-3)
MCI.Data.Update$MIN_adjust_Handicap_Zero[MCI.Data.Update$LopNr ==	7106] <- (28*30)/(30-1)
MCI.Data.Update$MIN_adjust_Handicap_Zero[MCI.Data.Update$LopNr ==7067] <- (29*30)/(30-1)
MCI.Data.Update$MIN_adjust_Handicap_Zero[MCI.Data.Update$LopNr ==6969] <- (27 *30)/(30-2)
MCI.Data.Update$MIN_adjust_Handicap_Zero[MCI.Data.Update$LopNr ==6935] <-(28 *30)/(30-1)
MCI.Data.Update$MIN_adjust_Handicap_Zero[MCI.Data.Update$LopNr ==6739] <- (22 *30)/(30-1)
MCI.Data.Update$MIN_adjust_Handicap_Zero[MCI.Data.Update$LopNr ==7497] <-26 
MCI.Data.Update$MIN_adjust_Handicap_Zero[MCI.Data.Update$LopNr ==6527] <- (25 *30)/(30-4)  
MCI.Data.Update$MIN_adjust_Handicap_Zero[MCI.Data.Update$LopNr ==10931] <- (24*30)/(30-5)
MCI.Data.Update$MIN_adjust_Handicap_Zero[MCI.Data.Update$LopNr ==7829] <- (25 *30)/(30-5)
MCI.Data.Update$MIN_adjust_Handicap_Zero[MCI.Data.Update$LopNr ==6757] <- (15*30)/(30-8)
MCI.Data.Update$MIN_adjust_Handicap_Zero[MCI.Data.Update$LopNr ==10038] <- (26 *30)/(30-3)
MCI.Data.Update$MIN_adjust_Handicap_Zero[MCI.Data.Update$LopNr ==6899] <- (22*30)/(30-2)
MCI.Data.Update$MIN_adjust_Handicap_Zero[MCI.Data.Update$LopNr ==7433] <- (23*30)/(30-2)
MCI.Data.Update$MIN_adjust_Handicap_Zero[MCI.Data.Update$LopNr ==10382] <- (19 *30)/(30-6)

MCI.Data.Update$MIN_adjust_Handicap_Zero<- 
  round(as.numeric(MCI.Data.Update$MIN_adjust_Handicap_Zero), digits = 0)

MCI.Data.Update$MIN18<- as.numeric(as.character(MCI.Data.Update$MIN18))
MCI.Data.Update$MIN18[MCI.Data.Update$MIN18=="<NA>"]<-'M'

MCI.Data.Update$MIN_adjust_Handicap_Zero[MCI.Data.Update$MIN_adjust_Handicap_Zero=="<NA>"]<-'M'

MCI.Data.Update$MIN_adjust_Handicap_Zero[is.na(MCI.Data.Update$MIN_adjust_Handicap_Zero)]<-
  MCI.Data.Update$MIN18[is.na(MCI.Data.Update$MIN_adjust_Handicap_Zero)]
######################################################################################################



######################################################################## Final NormalGenKog Computation
MCI.Data.Update<- MCI.Data.Update %>%
  mutate(NormalGenKog=ifelse(MIN_adjust_Handicap_Zero > 23,"Yes","No"))
######################################################################################################



###################################################################### Abnormal Memory Function for Age
MCI.Data.Update$N_Miss.abmp <- 
  rowSums(is.na(MCI.Data.Update[,c("MIN1","MIN2","MIN3","MIN25","MIN32","MIN35","MIN37")]))

MCI.Data.Update<- MCI.Data.Update %>% rowwise() %>% 
  mutate(SumMemPSyk=sum(MIN1,MIN2,MIN3,MIN25,MIN32,MIN35,MIN37, na.rm=TRUE),
         AbnormalMemPSyk=ifelse(SumMemPSyk>0,"Yes","No"))

options(warn = -1)
MCI.Data.Update$SumMemPSyk[MCI.Data.Update$N_Miss.abmp==7] <- "NA"
MCI.Data.Update$AbnormalMemPSyk[MCI.Data.Update$N_Miss.abmp==7] <- "NA"
MCI.Data.Update$AbnormalMemPSyk[MCI.Data.Update$AbnormalMemPSyk == 'NA']<- 'M'

##### Inspect AbnormalMemPSyk
CrossTable(MCI.Data.Update$AbnormalMemPSyk)
######################################################################################################




##################################################  Word Fluency variable  (WFC_BIN) classify according 
### To Educational levels low, medium and high according to variable educ_SFS (Formal education (years) 
### kohort 1930) computed by Simona Sacuiu July2019 using cut-off according to Tombaugh Tombaugh et al. 
### Normative data stratified by age and educ for verbal fluency testing.ArchClinNeuropsych1999

options(warn = -1)
MCI.Data.Update$MIN66[MCI.Data.Update$MIN66 == 99] <- 'NA'
MCI.Data.Update$MIN66[MCI.Data.Update$MIN66 == 77] <- 'NA'
MCI.Data.Update<-MCI.Data.Update %>% rowwise()%>% mutate(educ_level = ifelse(educ_sfs < 9, 'low',
                                              ifelse(educ_sfs >=9 & educ_sfs < 12,'medium','high')))

MCI.Data.Update$educ_level[is.na(MCI.Data.Update$educ_level)] <-'Missing'

MCI.Data.Update$MIN66<- as.numeric(MCI.Data.Update$MIN66)

hist(MCI.Data.Update$MIN66, col='blue', main = "Word Fluency Distribution", xlab = 'MIN66')

MCI.Data.Update$educ_level<- as.factor(MCI.Data.Update$educ_level)

describeBy(MCI.Data.Update$MIN66, group=MCI.Data.Update$educ_level)


############################################################################# Tombaugh et. al Archieve
#MCI.Data.Update<-MCI.Data.Update %>% rowwise()%>% 
# mutate(WFC_Bin = ifelse(educ_level == 'low' & (MIN66 < (13.1 - (3.8 * 1.5))), 1,
#ifelse(educ_level == 'medium' & (MIN66 < (13.9 - (3.4* 1.5))),1,
#ifelse(educ_level == 'high'  & (MIN66 < (16.3 - (4.3* 1.5))),1,
#ifelse(educ_level =='Missing' & (MIN66 < (13.1 - (3.8* 1.5))),1,0)))))
######################################################################################################



######################################################################################################
#### Word Fluency variable(WFC_BIN) classify using cut-offs at sample MEAN(SD) by Educational levels low,
#### medium and high according to variable educ_SFS (Formal education (years) kohort 1930) 
#### Computed by Simona Sacuiu July 2019.
########################################################################################################
MCI.Data.Update<-MCI.Data.Update %>% rowwise()%>% 
  mutate(WFC_Bin = ifelse(educ_level == 'low' & (MIN66 < (16.63 - (5.41 * 1.5))), 1,
                   ifelse(educ_level == 'medium' & (MIN66 < (19.11 - (5.97* 1.5))),1,
                   ifelse(educ_level == 'high'  & (MIN66 < (21.56 - (5.66* 1.5))),1,
                   ifelse(educ_level =='Missing' & (MIN66 < (16.40 - (5.70* 1.5))),1,0)))))


MCI.Data.Update$WFC_Bin<- as.factor(MCI.Data.Update$WFC_Bin)
MCI.Data.Update$WFC_Bin[MCI.Data.Update$LopNr == 14847] <- 'NA'
MCI.Data.Update$WFC_Bin[is.na(MCI.Data.Update$WFC_Bin)] <- 'Missing'
######################################################################################################



######################################################################################################
wfc<-select(MCI.Data.Update, LopNr, WFC_Bin) 

cc<- select(MCI.Data.Update.j, LopNr, MIN44I,MIN45,MIN46,MIN47,MIN48,MIN49,MIN50, STA22A,STA23A,STA24A)

all.wfc<- merge(cc, wfc, by.x = 'LopNr', by.y = 'LopNr' ,all = T)

all.wfc<- as.data.frame(all.wfc)

all.wfc$WFC_Bin<- as.numeric(as.character(all.wfc$WFC_Bin))
options(warn = -1)

all.wfc$MIN44I[all.wfc$MIN44I > 6]<- "NA"
all.wfc$MIN46[all.wfc$MIN46 > 6]<- "NA"
all.wfc$MIN45[all.wfc$MIN45 > 6]<- "NA"
all.wfc$MIN47[all.wfc$MIN47 > 4]<- "NA"
all.wfc$MIN48[all.wfc$MIN48 > 4]<- "NA"
all.wfc$MIN49[all.wfc$MIN49 > 4]<- "NA"
all.wfc$MIN50[all.wfc$MIN50 > 4]<- "NA"
all.wfc$MIN44I<- as.numeric(as.character(all.wfc$MIN44I))
all.wfc$MIN46<- as.numeric(as.character(all.wfc$MIN46))
all.wfc$MIN45<- as.numeric(as.character(all.wfc$MIN45))
all.wfc$MIN47<- as.numeric(as.character(all.wfc$MIN47))
all.wfc$MIN48<- as.numeric(as.character(all.wfc$MIN48))
all.wfc$MIN49<- as.numeric(as.character(all.wfc$MIN49))
all.wfc$MIN50<- as.numeric(as.character(all.wfc$MIN50))
all.wfc$STA22A<- as.numeric(as.character(all.wfc$STA22A))
all.wfc$STA23A<- as.numeric(as.character(all.wfc$STA23A))
all.wfc$STA24A<- as.numeric(as.character(all.wfc$STA24A))


all.wfc<- all.wfc %>% 
  rowwise() %>% 
mutate(ANM.sum = sum(MIN44I,MIN45,MIN46,MIN47,MIN48,MIN49,MIN50,STA22A,STA23A,STA24A,WFC_Bin, na.rm=T))

##### Count Missing
all.wfc$N_Miss_abnmp <- rowSums(is.na( all.wfc[,c('MIN44I','MIN45','MIN46','MIN47','MIN48','MIN49',
                                                  'MIN50','STA22A','STA23A','STA24A','WFC_Bin')]))


all.wfc<-all.wfc %>% 
  mutate(AbnormalNonMemPSyk=ifelse(N_Miss_abnmp > 10 & ANM.sum >= 0 ,"NA",
                            ifelse(N_Miss_abnmp < 10 & ANM.sum > 0 ,"Yes",
                            ifelse(N_Miss_abnmp == 10 & ANM.sum >0 ,"Yes","No"))))
######################################################################################################



########################################################################  ADL according ADL-index 0-1
ADL_Variables<- select(MCI.Data.Update,ADLTE,ADLTF,ADLTG,ADLTH,ADLTJ) %>% names()

MCI.Data.Update<- MCI.Data.Update %>% rowwise() %>%
  mutate(ADLpreserved=ifelse(pmax(!!!rlang::syms(ADL_Variables))==0,"Yes",
                      ifelse(pmax(!!!rlang::syms(ADL_Variables))> 0 & MIN18 > 26,"Yes",
                      ifelse(pmax(!!!rlang::syms(ADL_Variables))> 0 & MIN18 =="NA","No",
                      ifelse(pmax(!!!rlang::syms(ADL_Variables))> 0 & MIN18 <= 26,"No")))))

#####  Check for row sum Missing 
MCI.Data.Update$N_Miss_adl <- 
  rowSums(is.na( MCI.Data.Update[,c('ADLTE','ADLTF','ADLTG','ADLTH','ADLTJ')]))
options(warn = -1)

MCI.Data.Update$ADLpreserved[MCI.Data.Update$N_Miss_adl==5] <- "NA"

MCI.Data.Update$ADLpreserved[is.na(MCI.Data.Update$ADLpreserved)]<-"No"
######################################################################################################



##################################################################################### iADL Computation 
iadl_Variables<-select(MCI.Data.Update,ADLTA,ADLTB,ADLTC,ADLTD)%>% names()

MCI.Data.Update<- MCI.Data.Update  %>% rowwise() %>%
mutate(iADLpreserved=ifelse(pmax(!!!rlang::syms(iadl_Variables))< 1  ,"Yes" ,
                     ifelse(pmax(!!!rlang::syms(iadl_Variables))== 0 & MIN18 == "NA" ,"Yes",
                     ifelse(sum(ADLTA,ADLTB,ADLTC,ADLTD, na.rm=TRUE) <= 4 & MIN18 > 26,"Yes",
                     ifelse(sum(ADLTA,ADLTB,ADLTC,ADLTD, na.rm=TRUE) >= 1 & MIN18 > 26,"Yes","No"))))) 

##### Compute number of Missing for iadl variables 
MCI.Data.Update$N_Miss_iadl <- rowSums( is.na( MCI.Data.Update[,c('ADLTA','ADLTB','ADLTC','ADLTD')]))

MCI.Data.Update$iADLpreserved[MCI.Data.Update$N_Miss_iadl>=4] <- "Missing"

MCI.Data.Update$iADLpreserved[is.na(MCI.Data.Update$iADLpreserved)] <- "No"

MCI.Data.Update.iadl<- select(MCI.Data.Update,N_Miss_iadl,ADLTA,ADLTB,ADLTC,ADLTD,MIN18 ,iADLpreserved)
######################################################################################################



################################### Final Data Processing and cleaning for Petersen MCI Diagnosis 
PetersenMCI_data<- data.frame(MCI.Data.Update)
options(warn = -1)

PetersenMCI_data$Dementia<- rep("N", 365)

PetersenMCI_data[PetersenMCI_data=="Yes"]<- "Y"

PetersenMCI_data[PetersenMCI_data=="No"]<- "N"

PetersenMCI_data[is.na(PetersenMCI_data)]<- 'M'

PetersenMCI_data<- data.frame(PetersenMCI_data)
#######################################################################################################



################################################## Computing Petersen Base Diagnosis before sub-classes
for(i in 1:nrow(PetersenMCI_data)){
  if(PetersenMCI_data$Dementia[i] =="N"& PetersenMCI_data$SR_IQMemImp[i]=="Y" 
     & PetersenMCI_data$NormalGenKog[i]=="Y" 
     & PetersenMCI_data$AbnormalMemPSyk[i]=="Y" 
     & PetersenMCI_data$ADLpreserved[i]=="Y"){ ###25th percental
    PetersenMCI_data$PetersensMCI_Base[i]="Y"}
  else if(PetersenMCI_data$Dement[i] =="N"& (PetersenMCI_data$SR_IQMemImp[i]=="M" 
                                             | PetersenMCI_data$NormalGenKog[i]=="M" 
                                             | PetersenMCI_data$AbnormalMemPSyk[i]=="M" 
                                             | PetersenMCI_data$ADLpreserved[i]=="M")){ 
    PetersenMCI_data$PetersensMCI_Base[i]="M"}
  else { PetersenMCI_data$PetersensMCI_Base[i]="N"}
}


##### Inspect Petersen base files
CrossTable(PetersenMCI_data$PetersensMCI_Base)
########################################################################################################



#################################################################  MCI Classification Groups By Petersen 
PetersenMCI_data<- PetersenMCI_data %>% 
  mutate(PetersensMCI = ifelse(PetersensMCI_Base=="Y","PetersenMCI",
                        ifelse(PetersenMCI_data$Dementia =="N" 
                        & PetersenMCI_data$SR_IQMemImp=="N" 
                        & PetersenMCI_data$NormalGenKog=="Y" 
                        & PetersenMCI_data$AbnormalMemPSyk=="N" 
                        & PetersenMCI_data$ADLpreserved=="Y","Intact",
                        ifelse(PetersenMCI_data$Dement =="N" 
                        & PetersenMCI_data$SR_IQMemImp=="Y" 
                        & PetersenMCI_data$NormalGenKog=="Y" 
                        & PetersenMCI_data$AbnormalMemPSyk=="N" 
                        & PetersenMCI_data$ADLpreserved=="Y","Subjective Memory Impairment",
                        ifelse(PetersenMCI_data$Dement =="N" & PetersenMCI_data$SR_IQMemImp=="N" 
                        & PetersenMCI_data$NormalGenKog=="Y" 
                        & PetersenMCI_data$AbnormalMemPSyk=="Y" 
                        & PetersenMCI_data$ADLpreserved=="Y","Objective Memory Impairment","Oklar")))))


PetersenMCI_data$DEM1<- as.numeric(as.character(PetersenMCI_data$DEM1))
PetersenMCI_data$DEM2<- as.numeric(as.character(PetersenMCI_data$DEM2))
PetersenMCI_data$DEM3<- as.numeric(as.character(PetersenMCI_data$DEM3))
PetersenMCI_data$DEM4<- as.numeric(as.character(PetersenMCI_data$DEM4))
PetersenMCI_data$DEM5<- as.numeric(as.character(PetersenMCI_data$DEM5))
PetersenMCI_data$ADLTE<- as.numeric(as.character(PetersenMCI_data$ADLTE))
PetersenMCI_data$ADLTF<- as.numeric(as.character(PetersenMCI_data$ADLTF))
PetersenMCI_data$ADLTG<- as.numeric(as.character(PetersenMCI_data$ADLTG))
PetersenMCI_data$ADLTH<- as.numeric(as.character(PetersenMCI_data$ADLTH))
PetersenMCI_data$ADLTJ<- as.numeric(as.character(PetersenMCI_data$ADLTJ))
PetersenMCI_data$SR_IQMemImp[PetersenMCI_data$SR_IQMemImp=='Missing']<- 'M'
PetersenMCI_data$SR_IQMemImp<- as.factor(as.character(PetersenMCI_data$SR_IQMemImp))
PetersenMCI_data$NormalGenKog<- as.factor(as.character(PetersenMCI_data$NormalGenKog))
PetersenMCI_data$AbnormalMemPSyk<- as.factor(as.character(PetersenMCI_data$AbnormalMemPSyk))
PetersenMCI_data$ADLpreserved<- as.factor(as.character(PetersenMCI_data$ADLpreserved))
PetersenMCI_data$dem.sum <- rowSums(PetersenMCI_data[,c('DEM1','DEM2','DEM3','DEM4','DEM5')])
options(warn = -1)
PetersenMCI_data$dem.sum[is.na(dem.sum)]<- 'M'
PetersenMCI_data$adl.sum <- rowSums(PetersenMCI_data[,c('ADLTF','ADLTF','ADLTG','ADLTH','ADLTJ')])
PetersenMCI_data$adl.sum<- as.numeric(PetersenMCI_data$adl.sum)
options(warn = -1)
PetersenMCI_data$adl.sum[is.na(PetersenMCI_data$adl.sum)]<- 'M'
########################################################################################################



############################################################################# Sub-classification group Oklar (N = 25)
for(i in 1:nrow(PetersenMCI_data)){  
  if(PetersenMCI_data$PetersensMCI[i]=="Oklar" 
     & PetersenMCI_data$SR_IQMemImp[i]=="N"
     & PetersenMCI_data$NormalGenKog[i]=="Y"
     & PetersenMCI_data$AbnormalMemPSyk[i]=="N"
     & PetersenMCI_data$ADLpreserved[i]=="NA" 
     & as.numeric(!is.na(PetersenMCI_data$dem.sum)) <= 2){ 
    PetersenMCI_data$PetersensMCI.1[i]='Intact'}
  else if(PetersenMCI_data$PetersensMCI[i]=="Oklar" 
          & PetersenMCI_data$SR_IQMemImp[i]=="N"
          & PetersenMCI_data$NormalGenKog[i]=="Y"
          & PetersenMCI_data$AbnormalMemPSyk[i]=="N"
          & PetersenMCI_data$ADLpreserved[i]=="N"
          & PetersenMCI_data$ADLTE[i]==1 
          & PetersenMCI_data$adl.sum[i]==0){ 
    PetersenMCI_data$PetersensMCI.1[i]='Intact'}
  else if(PetersenMCI_data$PetersensMCI[i]=="Oklar" 
          & (PetersenMCI_data$SR_IQMemImp[i]=="N"|PetersenMCI_data$SR_IQMemImp[i]=="M")
          & (PetersenMCI_data$NormalGenKog[i]=="N"|PetersenMCI_data$NormalGenKog[i]=="Y")
          & PetersenMCI_data$AbnormalMemPSyk[i]=="Y"
          & PetersenMCI_data$ADLpreserved[i]=="Y"){ 
    PetersenMCI_data$PetersensMCI.1[i]='OMI'}
  else if(PetersenMCI_data$PetersensMCI[i]=="Oklar" 
          & PetersenMCI_data$SR_IQMemImp[i]=="N"
          & (PetersenMCI_data$NormalGenKog[i]=="Y"|PetersenMCI_data$NormalGenKog[i]=="N")
          & PetersenMCI_data$AbnormalMemPSyk[i]=="Y"
          & PetersenMCI_data$ADLpreserved[i]=="N"
          & PetersenMCI_data$ADLTE[i]==1
          & PetersenMCI_data$adl.sum[i]==0){ 
    PetersenMCI_data$PetersensMCI.1[i]='OMI'}
  
  else {PetersenMCI_data$PetersensMCI.1[i]=PetersenMCI_data$PetersensMCI[i]}
  
}
########################################################################################################



######################################################## Final MCi Classification according to Petersens 
for(i in 1:nrow(PetersenMCI_data)){  
  if(PetersenMCI_data$PetersensMCI.1[i]=="OMI"){
    PetersenMCI_data$PetersensMCI_FINAL[i]='Objective Memory Impairment'}
  else{
    PetersenMCI_data$PetersensMCI_FINAL[i]=PetersenMCI_data$PetersensMCI.1[i]}
}

########################################################################################################



########################################################################################################
PetersenMCI_data$iADLpreserved<- as.character(PetersenMCI_data$iADLpreserved)

PetersenMCI_data$ADLpreserved<- as.character(PetersenMCI_data$ADLpreserved)
options(warn = -1)

PetersenMCI_data$ADLpreserved[PetersenMCI_data$ADLpreserved =='NA']<- 'M'

PetersenMCI_data$iADLpreserved[PetersenMCI_data$iADLpreserved =='Missing']<- 'M'

all.wfc.1<- data.frame(select(all.wfc, LopNr, AbnormalNonMemPSyk))

options(warn = -1)

all.wfc.1[all.wfc.1 == 'NA']<- 'M'

all.wfc.1[all.wfc.1== 'Yes']<- 'Y'

all.wfc.1[all.wfc.1== 'No']<- 'N'

PetersenMCI_data<- data.frame(list(PetersenMCI_data,all.wfc.1)%>% reduce(left_join, by = 'LopNr'))
########################################################################################################



############################################################################ Everyday Function Preserved 
winbladMCI_data<-PetersenMCI_data %>%
  mutate(EverydayFuncPreserved = ifelse(ADLpreserved == "N" & iADLpreserved == "N","N", 
                                 ifelse(ADLpreserved == "M"& iADLpreserved == "M" ,"M","Y")),
        ObjectiveCogImp = ifelse(AbnormalMemPSyk == "M" | AbnormalNonMemPSyk =="M","M",
                          ifelse(AbnormalMemPSyk == "Y" ,"Y","N"))) 
########################################################################################################



################  WinbladMCI diagnonsis base data|(AbnormalNonMemPSyk == "Y" &  AbnormalNonMemPSyk !="M")
for(i in 1:nrow(winbladMCI_data)){
  if(winbladMCI_data$Dementia[i] =="N"& winbladMCI_data$SubKogImp[i]=="Y" 
     & winbladMCI_data$ObjectiveCogImp[i]=="Y" 
     & winbladMCI_data$EverydayFuncPreserved[i]=="Y" 
  ){ ###25th percental
    winbladMCI_data$WinbladMCI_Base[i]="Y"}
  else if(winbladMCI_data$Dementia[i] =="N"& (winbladMCI_data$SubKogImp[i]=="M" 
                                              |  winbladMCI_data$ObjectiveCogImp[i]=="M" 
                                              | winbladMCI_data$EverydayFuncPreserved[i]=="M" 
  )){ ###25th percental
    winbladMCI_data$WinbladMCI_Base[i]="M"}
  else { winbladMCI_data$WinbladMCI_Base[i]="N"}
}
############################################################################## WinbladMCI Diagnosis final
winbladMCI_data<- winbladMCI_data %>% 
  mutate(WinbladsMCI = ifelse(WinbladMCI_Base=="Y","WinbladsMCI",
                       ifelse(winbladMCI_data$Dementia =="N" 
                       & winbladMCI_data$SubKogImp=="N" 
                       & winbladMCI_data$ObjectiveCogImp=="N" 
                       & winbladMCI_data$EverydayFuncPreserved=="Y","Intact",
                       ifelse(winbladMCI_data$Dementia =="N"  
                       & winbladMCI_data$SubKogImp=="Y"  
                       & winbladMCI_data$ObjectiveCogImp=="N" 
                       & winbladMCI_data$EverydayFuncPreserved=="Y","Subjective Cognitive Impairment",
                       ifelse(winbladMCI_data$Dementia =="N"  
                       & winbladMCI_data$SubKogImp=="N"  
                       & winbladMCI_data$ObjectiveCogImp=="Y"  
                      & winbladMCI_data$EverydayFuncPreserved=="Y",
                      "Objective Cognitive Impairment","Oklar")))))
#######################################################################################################
winbladMCI_data$DEM1<- as.numeric(as.character(winbladMCI_data$DEM1))
winbladMCI_data$DEM2<- as.numeric(as.character(winbladMCI_data$DEM2))
winbladMCI_data$DEM3<- as.numeric(as.character(winbladMCI_data$DEM3))
winbladMCI_data$DEM4<- as.numeric(as.character(winbladMCI_data$DEM4))
winbladMCI_data$DEM5<- as.numeric(as.character(winbladMCI_data$DEM5))
########################################################################################################


########################################################################################################
winbladMCI_data$WinbladsMCI<- as.factor(as.character(winbladMCI_data$WinbladsMCI))
winbladMCI_data$SRCogIMP<- as.factor(as.character(winbladMCI_data$SRCogIMP))
winbladMCI_data$IQCODEBIN_FINAL<- as.factor(as.character(winbladMCI_data$IQCODEBIN_FINAL))
winbladMCI_data$SubKogImp<- as.factor(as.character(winbladMCI_data$SubKogImp))
winbladMCI_data$ObjectiveCogImp<- as.factor(as.character(winbladMCI_data$ObjectiveCogImp))
winbladMCI_data$EverydayFuncPreserved<- as.factor(as.character(winbladMCI_data$EverydayFuncPreserved))
winbladMCI_data$dem.sum <- rowSums(winbladMCI_data[,c('DEM1','DEM2','DEM3','DEM4','DEM5')])
options(warn = -1)
winbladMCI_data$IQCODEBIN_FINAL[winbladMCI_data$IQCODEBIN_FINAL == "Missing"]<-'M'
winbladMCI_data$SRCogIMP[winbladMCI_data$SRCogIMP=='Missing']<- 'M'
########################################################################################################



################################################################ Sub-classification group Oklar (N = 25)
winbladMCI_data$WinbladsMCI<- as.character(winbladMCI_data$WinbladsMCI)
for(i in 1:nrow(winbladMCI_data)){  
  if(winbladMCI_data$WinbladsMCI[i]=="Oklar" 
     & winbladMCI_data$SRCogIMP[i]=="N"
     & winbladMCI_data$IQCODEBIN_FINAL[i]=="N"
     & winbladMCI_data$SubKogImp[i]=="N"
     & winbladMCI_data$ObjectiveCogImp[i]=="N" 
     & winbladMCI_data$EverydayFuncPreserved[i]=="M"
     & winbladMCI_data$dem.sum[i]== 0){ 
    winbladMCI_data$WinbladsMCI.1[i]='Intact'}
  else {winbladMCI_data$WinbladsMCI.1[i]=winbladMCI_data$WinbladsMCI[i]}
  
}
########################################################################################################







########################### Cross Tables to check Summary Results
CrossTable(PetersenMCI_data$PetersensMCI_FINAL)
CrossTable(winbladMCI_data$DayDiffCat)
CrossTable(winbladMCI_data$SR_IQMemImp)
CrossTable(winbladMCI_data$SRMemImp)
CrossTable(winbladMCI_data$IQCODEBINMEM_FINAL)
CrossTable(PetersenMCI_data$NormalGenKog)
CrossTable(PetersenMCI_data$NormalGenKog)
CrossTable(PetersenMCI_data$AbnormalMemPSyk)
CrossTable(PetersenMCI_data$ADLpreserved)

CrossTable(winbladMCI_data$WinbladsMCI.1)
CrossTable(winbladMCI_data$SubKogImp)
CrossTable(winbladMCI_data$SRCogIMP)
CrossTable(winbladMCI_data$IQCODEBIN_FINAL)
CrossTable(winbladMCI_data$ObjectiveCogImp)
CrossTable(winbladMCI_data$AbnormalMemPSyk)
CrossTable(winbladMCI_data$WFC_Bin)
CrossTable(winbladMCI_data$AbnormalNonMemPSyk)
CrossTable(winbladMCI_data$EverydayFuncPreserved)
CrossTable(winbladMCI_data$iADLpreserved)
########################################################################################################



################################################################################ Write Data to CSV file
colnames(winbladMCI_data)
winbladMCI_data.1<- select(winbladMCI_data,1,88:134)
tot.dat<- data.frame(list(MCI.Data.Update.last,winbladMCI_data.1)
                     %>% reduce(left_join, by='LopNr'))
options(warn = -1)
tot.dat[tot.dat== "Missing"]<- 'M'
#write.csv(tot.dat, 'H85-2015-MCI_090819.csv')
########################################################################################################
print("MCI H85 2015 Diagnosis Algorithm Executed")


















