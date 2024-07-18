library(haven)
library(tidyverse) 
# dat<- read_sav('Kink & Consent _ Kink_June 4, 2019_14.06.sav')
# dat<-dat %>% dplyr::mutate(ID = row_number()) %>% select(ID,MMCS_1:CONSENT)


rm(list = ls()) 
dat<- read_sav('Kink_with variables.sav')

dat2<-dat %>% mutate(
    AGE = case_when(AGE=="just turned 58" ~ "58", 
                        T~AGE),
    AGE_SEX = case_when(AGE_SEX=="10 without consent, 15 or 16 with consent." | AGE_SEX=="4 (when sexual abuse began), 16 when it was done at MY request" | AGE_SEX=="I'm not sure what \"counts\" either 4 or 16" | AGE_SEX=="9 (consensually 16)"  ~ "16",
                        AGE_SEX=="14 (providing oral), 16 (receiving), 17 (intercourse)" ~ "14",
                        AGE_SEX=="Consensual at 15." ~ "15", 
                        AGE_SEX=="1520" | AGE_SEX=="fooled around with friends at 10, Intercourse at 17" ~ "17", 
                        AGE_SEX=="consensual - 19." ~ "19",
                        AGE_SEX=="Hasn't happened" ~ NA_character_, 
                        AGE_SEX=="6 or 7" ~ "6.5",
                        AGE_SEX=="2010" ~ "20",
                        AGE_SEX=="Are we talking first o or intercourse? 1986 and 17, respectively" ~ "17",
                        T~AGE_SEX),
    PARTNERS = case_when(
                         PARTNERS=="6 (I think)" ~ "6",
                         PARTNERS=="Unsure (Intercourse-about 10)" | PARTNERS=="&gt;10"|PARTNERS=="10 +"|PARTNERS=="10+"| PARTNERS=="-10" |PARTNERS=="Unsure (Intercourse-about 10)"  ~ "10", 
                         PARTNERS=="12-15?" ~ "13.5",
                         PARTNERS=="14?" ~ "14",
                         PARTNERS=="15-ish" |PARTNERS=="15+" ~ "15", 
                         PARTNERS=="~20" ~ "20",
                         PARTNERS=="20-30, possibly more, I've never really kept close track." |PARTNERS=="25+"|PARTNERS=="25 or mor3"|PARTNERS=="Unknown, 25+"  ~ "25", 
                         PARTNERS=="20+" | PARTNERS=="20-ish" | PARTNERS=="over 20" ~ "20",
                         PARTNERS=="22 or 23" ~ "22.5",  
                         PARTNERS=="30+" |PARTNERS=="30ish"|PARTNERS=="30 ish" |PARTNERS=="about 30" |PARTNERS=="30? 14 with full intercourse"  ~ "30", 
                         PARTNERS=="35?" |PARTNERS=="35+" ~ "35",
                         PARTNERS=="40+" ~ "40",
                         PARTNERS=="50+" ~ "50",
                         PARTNERS=="50-75 unknown" ~ "62.5",
                         PARTNERS=="35 male 30 females" ~ "65",
                         PARTNERS=="8 (unless you count online, and then thousands)" ~ "8", 
                         PARTNERS=="A bunch" | PARTNERS=="A lot." ~ "10", 
                         PARTNERS=="About 50-ish for Penis in vagina sex, probably 100-ish if you count other genital stimulation, and for partners for SM practice something like 300-ish" ~ "",
                         PARTNERS=="Again...just oral or intercourse? Well over 100, 50 something, respectively" ~ "50", 
                         PARTNERS=="greater than 25 in direct play, or even much greater depending on how you're defining it" ~ "25",
                         PARTNERS=="I don't keep count." ~ "",
                         PARTNERS=="I dont know.....over 100, I'm sure." ~ "100", 
                         PARTNERS=="Stopped counting in the 80's" |PARTNERS=="80+" ~ "80",
                         PARTNERS=="Too many to count" ~ "", 
                         PARTNERS=="Unknown. Hundreds" ~ "", 
                         PARTNERS=="&gt;100" |PARTNERS=="Aprox 100" ~ "100",
                         PARTNERS=="~150" | PARTNERS=="150+" ~ "150", 
                         PARTNERS=="Oh goodness 120?" ~ "120", 
                         PARTNERS=="200+" ~ "200",
                         PARTNERS=="over 300 (lost count many years ago)" ~ "300",
                         T~PARTNERS
    ) ,
    YR_PRACTICE = case_when(
                            YR_PRACTICE=="6 months" |YR_PRACTICE=="about six months" |YR_PRACTICE=="Less than 1" ~ ".5", 
                            YR_PRACTICE=="1 year" |YR_PRACTICE=="1&lt;"~ "1", 
                            YR_PRACTICE=="2 in the community, but I have had a paraphilia for as long as I can remember." | YR_PRACTICE=="2 years"|YR_PRACTICE=="two years"  ~ "2",  
                            YR_PRACTICE=="3 years" ~ "3", 
                            YR_PRACTICE=="4 years" ~ "4", 
                            YR_PRACTICE=="5 ish" ~ "5", 
                            YR_PRACTICE=="About 6 years" ~ "6",  
                            YR_PRACTICE=="7?" ~ "7",   
                            YR_PRACTICE=="7 or 8" ~ "7.5", 
                            YR_PRACTICE=="10+"|YR_PRACTICE=="Ten" |YR_PRACTICE=="ten years"  ~ "10", 
                            YR_PRACTICE=="11+ years" ~ "11", 
                            YR_PRACTICE=="12 years" |YR_PRACTICE=="Appriximately 12 years" ~ "12", 
                            YR_PRACTICE=="lifetime by myself, 16 years with partners" ~ "16",   
                            YR_PRACTICE=="17 years" ~ "17", 
                            YR_PRACTICE=="20 years"|YR_PRACTICE=="20 years on and off" |YR_PRACTICE=="20+" |YR_PRACTICE=="about 20 years" |YR_PRACTICE=="Over 20 years" |YR_PRACTICE=="Twenty"|YR_PRACTICE=="20+ years"   ~ "20",   
                            YR_PRACTICE=="25 years, off and on." |YR_PRACTICE=="25 years"~ "25", 
                            YR_PRACTICE=="30 plus" |YR_PRACTICE=="30 yrs" |YR_PRACTICE=="30+"|YR_PRACTICE=="about 30" |YR_PRACTICE=="Thirty" ~ "30",  
                            YR_PRACTICE=="40+" ~ "40", 
                            YR_PRACTICE=="aware it was a part of me since age 12-13, first serious D/s relationship maybe 10  years ago (early 20s)" ~ "10", 
                            YR_PRACTICE=="In porn preference, 10 years. With partners, 2" ~ "2", 
                            YR_PRACTICE=="Decades" ~ "20", 
                            T~YR_PRACTICE )  , 
    YR_COMMUN = case_when(
                          YR_COMMUN=="2 months" |YR_COMMUN=="just joined" ~ ".16", 
                          YR_COMMUN=="1-2" |YR_COMMUN=="6 months"|YR_COMMUN=="Less than 1" |YR_COMMUN=="Few months" |YR_COMMUN=="Almost a year"  ~ ".5",
                          YR_COMMUN=="1 year involved, but never participated actively."|YR_COMMUN=="1ish" |YR_COMMUN=="One"  ~ "1", 
                          YR_COMMUN=="2 years" |YR_COMMUN=="two years" ~ "2", 
                          YR_COMMUN=="3 years" |YR_COMMUN=="about three years" ~ "3", 
                          YR_COMMUN=="3-4" ~ "3.5",  
                          YR_COMMUN=="Like 2 Years in the scene. 4 years in the greater community" ~ "4", 
                          YR_COMMUN=="Approximately 5 years, off and on" ~ "5", 
                          YR_COMMUN=="5-6yrs" ~ "5.5", 
                          YR_COMMUN=="7Denver Sanct" ~ "7",  
                          YR_COMMUN=="Eight" ~ "8",  
                          YR_COMMUN=="10-intermittent" |YR_COMMUN=="10 years" |YR_COMMUN=="ten years"~ "10",  
                          YR_COMMUN=="11+ years" ~ "11", 
                          YR_COMMUN=="15+" ~ "15", 
                          YR_COMMUN=="16 years" ~ "16", 
                          YR_COMMUN=="20+ years" |YR_COMMUN=="20+" |YR_COMMUN=="Over 20 years" |YR_COMMUN=="over 20" ~ "20", 
                          YR_COMMUN=="25+" ~ "25", 
                          YR_COMMUN=="30vplus" |YR_COMMUN=="30 yrs"|YR_COMMUN=="30+"~ "30",  
                          YR_COMMUN=="I have yet to get officially involved in the community, but I intend to."|YR_COMMUN=="Never" |YR_COMMUN=="No" |YR_COMMUN=="None?" |YR_COMMUN=="Not alot around me" |YR_COMMUN=="not really involved with the local community right now" ~ "0", 
                            T~YR_COMMUN)  
)%>% mutate(
    AGE=as.numeric(AGE),
    AGE_SEX=as.numeric(AGE_SEX),
    PARTNERS=as.numeric(PARTNERS), 
    YR_PRACTICE=as.numeric(YR_PRACTICE), 
    YR_COMMUN=as.numeric(YR_COMMUN)
)%>% dplyr::select(id:GROUPS,YR_PRACTICE, YR_COMMUN,
                                 ASSERT_1:ASSERT_25, ASSERT,
                                 ASSUME_1:ASSUME_7, ASSUME,
                                 POS_1:POS_9, POS,
                                 PBC_1:PBC_9, PBC, 
                                 RLN_1:RLN_5, RLN,
                                 IBA_1:IBA_6, IBA,
                                 AWARE_1:AWARE_4, AWARE,
                                 RCUS_1:RCUS_18, RCUS,
                                 LIKELY_2:LIKELY_31, LIKELY_COERCE,
                                 SM_1:SM_23, YOST:YOST_RL,
                                 COMMUNITY_1:COMMUNITY_6, COMMUN) %>% 
    mutate(Women = case_when(Gender_2==1~1, Gender_1==1~0, Gender_3==1~NA_real_),
           Gender3 = case_when(Gender_1 == 1 ~ "Man",
                              Gender_2 == 1 ~ "Woman",
                              Gender_3 == 1 ~ "NB") %>% as.factor() %>% fct_relevel("Man","Woman","NB"),
           Gender2 = case_when(Gender_1 == 1 ~ "Man",
                               Gender_2 == 1 ~ "Woman",
                               Gender_3 == 1 ~ NA_character_) %>% as.factor() %>% fct_relevel("Man","Woman"),
           BDSMRole =  case_when(BDSM_1 == 1 ~ "Top",
                                 BDSM_2 == 1 ~ "Bottom",
                                 BDSM_3 == 1 ~ "Switch")%>% as.factor() %>% fct_relevel("Top","Bottom","Switch")) %>% 
    rename(Man=Gender_1,           Woman=Gender_2,           Enby=Gender_3,
           Top=BDSM_1,            Bottom=BDSM_2,           Switch=BDSM_3,
           Single = Rel_1,
           Casual = Rel_2, #incl. open and poly
           Committed = Rel_3) %>%  #incl. married, domestic, or long term dating 
    dplyr::mutate(YOST_Wrong_ln = log(YOST_Wrong), 
        YOST_V_ln = log(YOST_V), 
        PBC_ln = log(PBC), 
        AWARE_ln = log(AWARE), 
        RCUS_ln = log(RCUS),
        LIKELY_ln = log(LIKELY_COERCE)) %>% 
    rename(Gender4=Gender,         Gender=Gender2)%>% 
    rowwise()%>% mutate(COMM_Positive = mean(COMMUNITY_1,COMMUNITY_2, na.rm = TRUE),
                    COMM_Communicate = mean(COMMUNITY_3,COMMUNITY_4, na.rm = TRUE),
                    COMM_Growth = mean(COMMUNITY_5,COMMUNITY_6, na.rm = TRUE))
# saveRDS(dat_cl, "Final Cleaned Kink.rds")
# write_sav(dat_cl, "Final Cleaned Kink.sav")     # Apply write_sav function
# 
# dat<- readRDS("Final Cleaned Kink.rds")
datIDs<-dat2%>% dplyr::select(id, Gender, BDSMRole, Hetero, 
                          YR_PRACTICE, YR_COMMUN,COMM_Positive, COMM_Communicate, COMM_Growth,
                          ASSERT, ASSUME, POS, PBC, RLN, IBA, AWARE, RCUS, LIKELY_COERCE)
datIDs <- datIDs[complete.cases(datIDs),] #265 remain!
datIDs <- datIDs %>% dplyr::select(id)
dat2 <- right_join(dat2, datIDs, by="id")
write_sav(dat2, "Final Cleaned Kink_clean.sav")     # Apply write_sav function

saveRDS(dat2, "Final Kink.rds")     
