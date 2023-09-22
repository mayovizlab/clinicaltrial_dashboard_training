#_______________________________________________________________________
#_______________________________________________________________________
# Title: Clinical Trial Data Prep Example Protocol ID X
# PI: Dr. X
# Stats: Name
# Date: 
# Description: Prepping data for Dashboard
#_______________________________________________________________________
#_______________________________________________________________________


# Packages
library(tidyverse)
library(Hmisc)
library(arsenal)
library(openxlsx)
library(haven)
library(lubridate)
library(redcapAPI)
library(redcap)
library(flextable)
library(clinicaltrials)
library(kableExtra)
library(pins)
library(glue)

#  Read in data ----

crtlibn(d="ID", dir = "rave", alt = TRUE, fmts = TRUE)
today <- Sys.Date()


#  Prep data ----
length(unique(adjuvant$subject))==dim(adjuvant)[1]
adjuvant <- adjuvant %>% 
  select(subject,raveid,strtadjuvant_dt:hormonal_endo_st_dt)

length(unique(demography$subject))==dim(demography)[1]
demography <- demography %>% 
  select(subject,raveid,pt_initials_name:demo_comments)

length(unique(eligible$subject))==dim(eligible)[1]
eligible <- eligible %>% 
  select(subject,raveid,enroll_dt,consent_dt,eligible_comm)

length(unique(enroll$subject))==dim(enroll)[1]
enroll <- enroll %>% 
  select(subject,raveid,site,sitenumber,clinic)

length(unique(onstudy$subject))==dim(onstudy)[1]
onstudy <- onstudy %>% 
  select(subject,raveid,height:lymphnodeaxis)

length(unique(onstudy_chemo$subject))==dim(onstudy_chemo)[1]
onstudy_chemo <- onstudy_chemo %>% 
  select(subject,raveid,strtadjuvant_dt:hormonal_endo_other_sp)

length(unique(radiation$subject))==dim(radiation)[1]
radiation <- radiation %>% 
  select(subject,raveid,radbegindt:skincare_sp)

length(unique(randomize$subject))==dim(randomize)[1]
randomize <- randomize %>% 
  select(subject,raveid,radiationtype:regime_selection_method)

length(unique(surgery$subject))==dim(surgery)[1]
surgery <- surgery %>% 
  select(subject,raveid,surgerydt:dcisgrade)

length(unique(endstudy$subject))==dim(endstudy)[1]
endstudy <- endstudy %>% 
  select(subject,raveid,lastvisdt:endstrsn_sp)

length(unique(conswdnav$subject))==dim(conswdnav)[1]
conswdnav <- conswdnav %>% 
  select(subject,raveid,conswdcfuind:conswdafuind)



# It is good to create a single dataset with only one row per patient on all data that is not repeated
all_dat <- enroll %>% 
  left_join(demography) %>% left_join(eligible) %>% 
  left_join(onstudy) %>% left_join(radiation) %>% left_join(randomize) %>% 
  left_join(surgery) %>% left_join(endstudy) %>% left_join(conswdnav) %>% 
  left_join(adjuvant)

# PTRAX ---- 
ptrax <- read_sas("/path/to/ptrax/files/from/cancer/center/sas/data/warehouse/ptraxprots.sas7bdat")
ptrax <- ptrax %>% filter(PROTOCOL_ID=="IRB")
ptrax_cases <-haven::read_sas("/path/to/ptrax/files/from/cancer/center/sas/data/warehouse/ptraxcases.sas7bdat") %>%
  filter(PROTOCOL_ID == "IRB")
names(ptrax_cases) <- tolower(names(ptrax_cases))
ptrax_cases <- ptrax_cases %>% filter(current_status==1)

# Toxicity ---- 
tox_oth <- toxicity_oth %>% 
  mutate(type="Other",time=folderinstancename,
         ae_yr = as.numeric(substr(as.character(mincreated), 1, 4)),
         eval_dt = as.Date(substr(as.character(mincreated),1,10),format="%Y-%m-%d")
  ) %>% 
  select(subject,raveid,time,type,eval_dt,toxxx,toxxx_sp,medra=medrasocxx,grade=valxx,valdescxx,
         attrib=tox_rlxx,ae_yr)

tox_sol <- toxicity_solicit %>% 
  mutate(type="Solicit",
         time=folderinstancename,
         ae_yr = as.numeric(substr(as.character(eval_dt), 1, 4)) 
  ) %>% 
  select(subject,raveid,time,type,eval_dt,toxxx,toxxx_sp,medra=medraxx,grade=valxx,valdescxx,
         attrib=tox_rlxx,ae_yr)

tox_telangie <- toxicity_solicit %>% 
  filter(aesolictelangiectasia=="Yes") %>% 
  mutate(
    type="Solicit",
    time=folderinstancename,
    toxxx = "Telangiectasia",
    toxxx_sp = NA_character_,
    medra = 10043189,
    grade = aesoltelgrade,
    valdescxx = NA_character_,
    attrib = NA_character_,
    ae_yr = as.numeric(substr(as.character(eval_dt),1,4))
  ) %>% group_by(subject) %>% slice(1) %>% ungroup() %>% 
  select(subject,raveid,time,type,eval_dt,toxxx,toxxx_sp,medra,grade,valdescxx,
         attrib,ae_yr)

# Combine the toxicity/AE data into a single dataset
tox_combined <- tox_sol %>% rbind(tox_oth) %>% 
  rbind(tox_telangie) %>% 
  filter(!is.na(grade)) %>% 
  mutate(
    time = case_when(
      time=="Baseline" ~ "Baseline",
      time=="Treatment (1)" ~ "Treatment",
      time=="Three Month (1)" ~ "3 Month",
      time=="One Year (1)" ~ "1 Year",
      time=="Two Year (1)" ~ "2 Year",
      TRUE ~ NA_character_
    ),
    time = ordered(time,levels=c("Baseline","Treatment","3 Month","1 Year","2 Year",NA_character_))
  )


# Follow-up ----- 
event_bsl <- event_bsl %>% 
  mutate(
    type="Baseline",
    time="Baseline"
  ) %>% 
  select(subject,raveid,type,time,qolcomp,compdt,fu_date=bsl_dt,fu_stat,deathrsn,deathrsnsp)
event_cfu <- event_cfu %>% 
  mutate(
    type="CFU"
  ) %>% 
  select(subject,raveid,type,time=folderinstancename,qolcomp,
         compdt,fu_date,fu_stat,deathrsn,deathrsnsp)
event_sfu <- event_sfu %>% 
  mutate(
    type="SFU"
  ) %>% 
  select(subject,raveid,type,time=folderinstancename,qolcomp,
         compdt,fu_date,fu_stat,deathrsn,deathrsnsp)
followup <- rbind(event_bsl,event_sfu,event_cfu)



tox <- tox_combined

accrual <- all_dat %>% 
  mutate(
    tx_missing = if_else(is.na(radenddt)==T & is.na(radbegindt)==T,"Mising","Not Missing"),
    tx_status = if_else(is.na(radenddt)==T & is.na(radbegindt)==T,"Not started/missing",
                        if_else(is.na(radenddt)==T & is.na(radbegindt)==F,"In treatment",
                                if_else(is.na(radenddt)==F,"Treatment Complete",""))),
    rt_end_int = radenddt + 5,
    mo3_start = radenddt %m+% months(2),
    mo3_end = radenddt %m+% months(4),
    mo3_2wk = radenddt %m+% months(4) - 14,
    mo6_start = radenddt %m+% months(4),
    mo6_end = radenddt %m+% months(8),
    mo6_2wk = radenddt %m+% months(8) - 14,
    mo12_start = radenddt %m+% months(6),
    mo12_end = radenddt %m+% months(18),
    mo12_2wk = radenddt %m+% months(18) -14,
    yr2_start = radenddt %m+% months(18),
    yr2_end = radenddt %m+% months(30),
    yr2_2wk = radenddt %m+% months(30) - 14,
    yr3_start = radenddt %m+% months(30),
    yr3_end = radenddt %m+% months(42),
    yr3_2wk = radenddt %m+% months(42) - 14,
    yr4_start = radenddt %m+% months(42),
    yr4_end = radenddt %m+% months(54),
    yr4_2wk = radenddt %m+% months(54) - 14,
    yr5_start = radenddt %m+% months(54),
    yr5_end = radenddt %m+% months(66),
    yr5_2wk = radenddt %m+% months(66) - 14,
    mo3_window = if_else(mo3_start<=today & mo3_2wk>=today,"In window",
                         if_else(mo3_start>today | is.na(radenddt)==T,"Before window",
                                 if_else(mo3_2wk<today & mo3_end>=today,"Within 2 weeks of deadline",
                                         if_else(mo3_end<today,"Past window","")))),
    mo6_window = if_else(mo6_start<=today & mo6_2wk>=today,"In window",
                         if_else(mo6_start>today | is.na(radenddt)==T,"Before window",
                                 if_else(mo6_2wk<today & mo6_end>=today,"Within 2 weeks of deadline",
                                         if_else(mo6_end<today,"Past window","")))),
    
    mo12_window = if_else(mo12_start<=today & mo12_2wk>=today,"In window",
                          if_else(mo12_start>today | is.na(radenddt)==T,"Before window",
                                  if_else(mo12_2wk<today & mo12_end>=today,"Within 2 weeks of deadline",
                                          if_else(mo12_end<today,"Past window","")))),
    yr2_window = if_else(yr2_start<=today & yr2_2wk>=today,"In window",
                         if_else(yr2_start>today | is.na(radenddt)==T,"Before window",
                                 if_else(yr2_2wk<today & yr2_end>=today,"Within 2 weeks of deadline",
                                         if_else(yr2_end<today,"Past window","")))),
    yr3_window = if_else(yr3_start<=today & yr3_2wk>=today,"In window",
                         if_else(yr3_start>today | is.na(radenddt)==T,"Before window",
                                 if_else(yr3_2wk<today & yr3_end>=today,"Within 2 weeks of deadline",
                                         if_else(yr3_end<today,"Past window","")))),
    yr4_window = if_else(yr4_start<=today & yr4_2wk>=today,"In window",
                         if_else(yr4_start>today | is.na(radenddt)==T,"Before window",
                                 if_else(yr4_2wk<today & yr4_end>=today,"Within 2 weeks of deadline",
                                         if_else(yr4_end<today,"Past window","")))),
    yr5_window = if_else(yr5_start<=today & yr5_2wk>=today,"In window",
                         ifelse(yr5_start>today | is.na(radenddt)==T,"Before window",
                                ifelse(yr5_2wk<today & yr5_end>=today,"Within 2 weeks of deadline",
                                       ifelse(yr5_end<today,"Past window",""))))
    
  ) %>% 
  left_join(
    ptrax_cases %>% 
      mutate(clinic = as.numeric(clinic_number),
             ptrax_status_dt = as.Date(status_effective_date,format="%Y-%m-%d"),
             treating_provider = tolower(treating_provider)) %>%
      select(clinic,ptrax_status=status_desc,ptrax_status_dt,
             pri_reason_code,nci_payment,treating_provider)
  ) %>% 
  mutate(
    enroll_dys = today-enroll_dt,
    enroll3mo = ifelse(today-enroll_dt<=90,"Yes","No"),
    enroll6mo = ifelse(today-enroll_dt<=180,"Yes","No"),
    enroll9mo = ifelse(today-enroll_dt<=270,"Yes","No"),
    enroll12mo = ifelse(enroll_dys<=365,"Yes","No"),
    monthly_enroll = paste(year(enroll_dt),month(enroll_dt),sep="-"),
    site=ifelse(site=="Mayo Clinic in Arizona","ARZ",
                ifelse(site=="Mayo Clinic in Florida","FLA","RST")),
    enroll = case_when(
      is.na(enroll_dt)==F ~ 1,
      TRUE ~ 0
    ),
    accrued = case_when(
      is.na(consent_dt)==F ~ 1,
      TRUE ~ 0
    ),
    withdrawal = case_when(
      raveid %in% conswdnav$raveid ~ 1,
      TRUE ~ 0
    ),
    radiationtype = as.factor(radiationtype),
    surgerytype = as.factor(surgerytype),
    reconstruction = as.factor(reconstruction),
    regime_name = as.factor(regime_name),
    age_enroll = as.numeric(enroll_dt-per_bir_dt)/365.25,
    age_rt = as.numeric(radbegindt-per_bir_dt)/365.25,
    race_cat_txt = case_when(
      race_cat_txt==1 ~ "American Indian or Alaska Native",
      race_cat_txt==2 ~ "Asian",
      race_cat_txt==3 ~ "Black or African American",
      race_cat_txt==4 ~ "Native Hawaiian or Other Pacific Islander",
      race_cat_txt==5 ~ "White",
      race_cat_txt==6 ~ "Unknown",
      race_cat_txt==7 ~ "Choose not to Disclose",
      TRUE ~ NA_character_
    ),
    ethn_grp_cat_txt = case_when(
      ethn_grp_cat_txt==1 ~ "Hispanic or Latino",
      ethn_grp_cat_txt==2 ~ "Not Hispanic or Latino",
      ethn_grp_cat_txt==3 ~ "Unknown or not reported",
      ethn_grp_cat_txt==4 ~ "Unable to provide",
      TRUE ~ NA_character_
    ),
    adj_chemo = case_when(
      !is.na(strtadjuvant_dt) ~ T,
      TRUE ~ F
    )
  ) %>% 
  group_by(clinic) %>% 
  mutate(
    total_dose = sum(dose_prior_boost,rttotalboost,na.rm=T),
    total_frac = sum(fractions_prior_boost,fractionstoboost,na.rm=T)
  ) %>% 
  ungroup() %>% 
  mutate(
    dose_prior_boost2 = as.character(dose_prior_boost),
    rttotalboost2 = as.character(rttotalboost),
    fractions_prior_boost2 = as.character(fractions_prior_boost),
    fractionstoboost2 = as.character(fractionstoboost),
    total_dose2 = as.character(total_dose),
    total_frac2 = as.character(total_frac)
  ) %>% 
  mutate(
    ## AE Completion variables ----
    bsl_ae = case_when(
      raveid %in% tox[tox$time=="Baseline",]$raveid ~ "<p><span class = 'glyphicon glyphicon-ok' style='color:#2ECC71'></span><span style='color: #2ECC71;'>Completed</span></p>",
      raveid %nin% tox[tox$time=="Baseline",]$raveid & endstrsn %in% 
        c("Informed consent withdrawal*","Patient refused treatment arm") ~ "Patient withdrew before tx",
      is.na(enroll_dt) | (raveid %nin% tox[tox$time=="Baseline",]$raveid & as.numeric(today-enroll_dt)>7) ~ as.character(glue("<p><span class='glyphicon glyphicon-remove' style='color:#E74C3C'></span><span style='color: #E74C3C;'> Overdue ended {enroll_dt + 7}</span></p>")),
      raveid %nin% tox[tox$time=="Baseline",]$raveid & as.numeric(today-enroll_dt)<=7 ~ as.character(glue("<p><span class='glyphicon glyphicon-hourglass' style='color:#E8C838'></span><span style='color:#E8C838'>In window (ends {enroll_dt + 7})</span></p>")),
      TRUE ~ NA_character_
    ),
    tx_ae = case_when(
      raveid %in% tox[tox$time=="Treatment",]$raveid ~ "<p><span class = 'glyphicon glyphicon-ok' style='color:#2ECC71'><span style='color: #2ECC71;'>Completed</span></p>",
      raveid %nin% tox[tox$time=="Treatment",]$raveid & is.na(radenddt) & endstrsn %in% 
        c("Informed consent withdrawal*","Patient refused treatment arm") ~ "Patient withdrew before tx",
      is.na(radenddt) ~ "Patient did not receive tx",
      (raveid %nin% tox[tox$time=="Treatment",]$raveid & as.numeric(today-radenddt)>5) ~ as.character(glue("<p><span style='color: #E74C3C;'><span class='glyphicon glyphicon-remove' style='color:#E74C3C'></span>Overdue ended {radenddt}</span></p>")),
      raveid %nin% tox[tox$time=="Treatment",]$raveid & as.numeric(today-radenddt)<=7 ~ as.character(glue("<p><span class='glyphicon glyphicon-hourglass' style='color:#E8C838'></span><span style='color: #E8C838'>In window (ends {radenddt})</span></p>")),
      TRUE ~ NA_character_
    ),
    mo3_ae = case_when(
      raveid %in% tox[tox$time=="3 Month",]$raveid ~ "<p><span class = 'glyphicon glyphicon-ok' style='color:#2ECC71'><span style='color: #2ECC71;'>Completed</span></p>",
      raveid %nin% tox[tox$time=="3 Month",]$raveid & is.na(radenddt) & endstrsn %in% 
        c("Informed consent withdrawal*","Patient refused treatment arm") ~ "Patient withdrew before tx",
      is.na(radenddt) ~ "Patient did not receive tx",
      raveid %nin% tox[tox$time=="3 Month",]$raveid & (endstrsn=="Informed consent withdrawal*" | withdrawal==1) & 
        mo3_end>lastvisdt ~ as.character(glue("Patient withdrew before {mo3_end}")),
      raveid %nin% tox[tox$time=="3 Month",]$raveid & as.numeric(today-radenddt)>112 ~ as.character(glue("<p><span style='color: #E74C3C;'><span class='glyphicon glyphicon-remove' style='color:#E74C3C'></span>Overdue ended {mo3_end}</span></p>")),
      raveid %nin% tox[tox$time=="3 Month",]$raveid & as.numeric(today-radenddt)<=112 & as.numeric(today-radenddt)>=56 ~ as.character(glue("<p><span class='glyphicon glyphicon-hourglass' style='color:#E8C838'></span><span style='color: #E8C838'>In window (ends {mo3_end})</span></p>")),
      raveid %nin% tox[tox$time=="3 Month",]$raveid & as.numeric(today-radenddt)<=56 ~ as.character(glue("<p><span class='glyphicon glyphicon-hourglass' style='color:#E8C838'></span><span style='color: #E8C838'>Before window (starts {mo3_start})</span></p>")),
      TRUE ~ NA_character_
    ),
    yr1_ae = case_when(
      raveid %in% tox[tox$time=="1 Year",]$raveid ~ "<p><span class = 'glyphicon glyphicon-ok' style='color:#2ECC71'> <span style='color: #2ECC71;'>Completed</span></p>",
      raveid %nin% tox[tox$time=="1 Year",]$raveid & is.na(radenddt) & endstrsn %in% 
        c("Informed consent withdrawal*","Patient refused treatment arm") ~ "Patient withdrew before tx",
      is.na(radenddt) ~ "Patient did not receive tx",
      raveid %nin% tox[tox$time=="1 Year",]$raveid & (endstrsn=="Informed consent withdrawal*" | withdrawal==1) & 
        mo12_end>lastvisdt ~ as.character(glue("Patient withdrew before {mo12_end}")),
      raveid %nin% tox[tox$time=="1 Year",]$raveid & as.numeric(today-radenddt)>549 ~ as.character(glue("<p><span style='color: #E74C3C;'><span class='glyphicon glyphicon-remove' style='color:#E74C3C'></span>Overdue ended {mo12_end}</span></p>")),
      raveid %nin% tox[tox$time=="1 Year",]$raveid & as.numeric(today-radenddt)<=549 & as.numeric(today-radenddt)>=183 ~ as.character(glue("<p><span class='glyphicon glyphicon-hourglass' style='color:#E8C838'></span><span style='color: #E8C838'>In window (ends {mo12_end})</span></p>")),
      raveid %nin% tox[tox$time=="1 Year",]$raveid & as.numeric(today-radenddt)<=183 ~ as.character(glue("<p><span class='glyphicon glyphicon-hourglass' style='color:#E8C838'></span><span style='color: #E8C838'>Before window (starts {mo12_start})</span></p>")),
      TRUE ~ NA_character_
    ),
    yr2_ae = case_when(
      raveid %in% tox[tox$time=="2 Year",]$raveid ~ "<p><span class = 'glyphicon glyphicon-ok' style='color:#2ECC71'><span style='color: #2ECC71;'>Completed</span></p>",
      raveid %nin% tox[tox$time=="2 Year",]$raveid & is.na(radenddt) & endstrsn %in% 
        c("Informed consent withdrawal*","Patient refused treatment arm") ~ "Patient withdrew before tx",
      is.na(radenddt) ~ "Patient did not receive tx",
      raveid %nin% tox[tox$time=="2 Year",]$raveid & (endstrsn=="Informed consent withdrawal*" | withdrawal==1) & 
        yr2_end>lastvisdt ~ as.character(glue("Patient withdrew before {yr2_end}")),
      raveid %nin% tox[tox$time=="2 Year",]$raveid & as.numeric(today-radenddt)>915 ~ as.character(glue("<p><span style='color: #E74C3C;'><span class='glyphicon glyphicon-remove' style='color:#E74C3C'></span>Overdue ended {yr2_end}</span></p>")),
      raveid %nin% tox[tox$time=="2 Year",]$raveid & as.numeric(today-radenddt)<=915 & as.numeric(today-radenddt)>=549 ~ as.character(glue("<p><span class='glyphicon glyphicon-hourglass' style='color:#E8C838'></span><span style='color: #E8C838'>In window (ends {yr2_end})</span></p>")),
      raveid %nin% tox[tox$time=="2 Year",]$raveid & as.numeric(today-radenddt)<=549 ~ as.character(glue("<p><span class='glyphicon glyphicon-hourglass' style='color:#E8C838'></span><span style='color: #E8C838'>Before window (starts {yr2_start})</span></p>")),
      TRUE ~ NA_character_
    ),
    yr3_ae = case_when(
      raveid %in% tox[tox$time=="3 Year",]$raveid ~ "<p><span class = 'glyphicon glyphicon-ok' style='color:#2ECC71'><span style='color: #2ECC71;'>Completed</span></p>",
      raveid %nin% tox[tox$time=="3 Year",]$raveid & is.na(radenddt) & endstrsn %in% 
        c("Informed consent withdrawal*","Patient refused treatment arm") ~ "Patient withdrew before tx",
      is.na(radenddt) ~ "Patient did not receive tx",
      raveid %nin% tox[tox$time=="3 Year",]$raveid & (endstrsn=="Informed consent withdrawal*" | withdrawal==1) & 
        yr3_end>lastvisdt ~ as.character(glue("Patient withdrew before {yr3_end}")),
      raveid %nin% tox[tox$time=="3 Year",]$raveid & as.numeric(today-radenddt)>1281 ~ as.character(glue("<p><span style='color: #E74C3C;'><span class='glyphicon glyphicon-remove' style='color:#E74C3C'></span>Overdue ended {yr3_end}</span></p>")),
      raveid %nin% tox[tox$time=="3 Year",]$raveid & as.numeric(today-radenddt)<=1281 & as.numeric(today-radenddt)>=915 ~ as.character(glue("<p><span class='glyphicon glyphicon-hourglass' style='color:#E8C838'></span><span style='color: #E8C838'>In window (ends {yr3_end})</span></p>")),
      raveid %nin% tox[tox$time=="3 Year",]$raveid & as.numeric(today-radenddt)<=915 ~ as.character(glue("<p><span class='glyphicon glyphicon-hourglass' style='color:#E8C838'></span><span style='color: #E8C838'>Before window (starts {yr3_start})</span></p>")),
      TRUE ~ NA_character_
    ),
    yr4_ae = case_when(
      raveid %in% tox[tox$time=="4 Year",]$raveid ~ "<p><span class = 'glyphicon glyphicon-ok' style='color:#2ECC71'><span style='color: #2ECC71;'>Completed</span></p>",
      raveid %nin% tox[tox$time=="4 Year",]$raveid & is.na(radenddt) & endstrsn %in% 
        c("Informed consent withdrawal*","Patient refused treatment arm") ~ "Patient withdrew before tx",
      is.na(radenddt) ~ "Patient did not receive tx",
      raveid %nin% tox[tox$time=="4 Year",]$raveid & (endstrsn=="Informed consent withdrawal*" | withdrawal==1) & 
        yr4_end>lastvisdt ~ as.character(glue("Patient withdrew before {yr4_end}")),
      raveid %nin% tox[tox$time=="4 Year",]$raveid & as.numeric(today-radenddt)>1647 ~ as.character(glue("<p><span style='color: #E74C3C;'><span class='glyphicon glyphicon-remove' style='color:#E74C3C'></span>Overdue ended {yr4_end}</span></p>")),
      raveid %nin% tox[tox$time=="4 Year",]$raveid & as.numeric(today-radenddt)<=1647 & as.numeric(today-radenddt)>=1281 ~ as.character(glue("<p><span class='glyphicon glyphicon-hourglass' style='color:#E8C838'></span><span style='color: #E8C838'>In window (ends {yr4_end})</span></p>")),
      raveid %nin% tox[tox$time=="4 Year",]$raveid & as.numeric(today-radenddt)<=1281 ~ as.character(glue("<p><span class='glyphicon glyphicon-hourglass' style='color:#E8C838'></span><span style='color: #E8C838'>Before window (starts {yr4_start})</span></p>")),
      TRUE ~ NA_character_
    ),
    yr5_ae = case_when(
      raveid %in% tox[tox$time=="5 Year",]$raveid ~ "<p><span class = 'glyphicon glyphicon-ok' style='color:#2ECC71'><span style='color: #2ECC71;'>Completed</span></p>",
      raveid %nin% tox[tox$time=="5 Year",]$raveid & is.na(radenddt) & endstrsn %in% 
        c("Informed consent withdrawal*","Patient refused treatment arm") ~ "Patient withdrew before tx",
      is.na(radenddt) ~ "Patient did not receive tx",
      raveid %nin% tox[tox$time=="5 Year",]$raveid & (endstrsn=="Informed consent withdrawal*" | withdrawal==1) & 
        yr5_end>lastvisdt ~ as.character(glue("Patient withdrew before {yr5_end}")),
      raveid %nin% tox[tox$time=="5 Year",]$raveid & as.numeric(today-radenddt)>2013 ~ as.character(glue("<p><span style='color: #E74C3C;'><span class='glyphicon glyphicon-remove' style='color:#E74C3C'></span>Overdue ended {yr5_end}</span></p>")),
      raveid %nin% tox[tox$time=="5 Year",]$raveid & as.numeric(today-radenddt)<=2013 & as.numeric(today-radenddt)>=1647 ~ as.character(glue("<p><span class='glyphicon glyphicon-hourglass' style='color:#E8C838'></span><span style='color: #E8C838'>In window (ends {yr5_end})</span></p>")),
      raveid %nin% tox[tox$time=="5 Year",]$raveid & as.numeric(today-radenddt)<=1647 ~ as.character(glue("<p><span class='glyphicon glyphicon-hourglass' style='color:#E8C838'></span><span style='color: #E8C838'>Before window (starts {yr5_start})</span></p>")),
      TRUE ~ NA_character_
    ),
    ## FU Completion variables -----
    bsl_fu = case_when(
      raveid %in% followup[followup$time=="Baseline",]$raveid ~ "<p><span class = 'glyphicon glyphicon-ok' style='color:#2ECC71'><span style='color: #2ECC71;'>Completed</span></p>",
      raveid %nin% followup[followup$time=="Baseline",]$raveid & endstrsn %in% 
        c("Informed consent withdrawal*","Patient refused treatment arm") ~ "Patient withdrew before tx",
      is.na(enroll_dt) | (raveid %nin% followup[followup$time=="Baseline",]$raveid & as.numeric(today-enroll_dt)>7) ~ as.character(glue("<p><span style='color: #E74C3C;'><span class='glyphicon glyphicon-remove' style='color:#E74C3C'></span>Overdue ended {enroll_dt}</span></p>")),
      raveid %nin% followup[followup$time=="Baseline",]$raveid & as.numeric(today-enroll_dt)<=7 ~ as.character(glue("<p><span class='glyphicon glyphicon-hourglass' style='color:#E8C838'></span><span style='color: #E8C838'>In window (ends {enroll_dt})</span></p>")),
      TRUE ~ NA_character_
    ),
    tx_fu = case_when(
      raveid %in% followup[followup$time=="Treatment (1)",]$raveid ~ "<p><span class = 'glyphicon glyphicon-ok' style='color:#2ECC71'><span style='color: #2ECC71;'>Completed</span></p>",
      raveid %nin% followup[followup$time=="Treatment (1)",]$raveid & is.na(radenddt) & endstrsn %in% 
        c("Informed consent withdrawal*","Patient refused treatment arm") ~ "Patient withdrew before tx",
      is.na(radenddt) ~ "Patient did not receive tx",
      (raveid %nin% followup[followup$time=="Treatment (1)",]$raveid & as.numeric(today-radenddt)>5) ~ as.character(glue("<p><span style='color: #E74C3C;'><span class='glyphicon glyphicon-remove' style='color:#E74C3C'></span>Overdue ended {radenddt}</span></p>")),
      raveid %nin% followup[followup$time=="Treatment (1)",]$raveid & as.numeric(today-radenddt)<=7 ~ as.character(glue("<p><span class='glyphicon glyphicon-hourglass' style='color:#E8C838'></span><span style='color: #E8C838'>In window (ends {radenddt})</span></p>")),
      TRUE ~ NA_character_
    ),
    mo3_fu = case_when(
      raveid %in% followup[followup$time=="Three Month (1)",]$raveid ~ "<p><span class = 'glyphicon glyphicon-ok' style='color:#2ECC71'><span style='color: #2ECC71;'>Completed</span></p>",
      raveid %nin% followup[followup$time=="Three Month (1)",]$raveid & is.na(radenddt) & endstrsn %in% 
        c("Informed consent withdrawal*","Patient refused treatment arm") ~ "Patient withdrew before tx",
      is.na(radenddt) ~ "Patient did not receive tx",
      raveid %nin% followup[followup$time=="Three Month (1)",]$raveid & (endstrsn=="Informed consent withdrawal*" | withdrawal==1) & 
        mo3_end>lastvisdt ~ as.character(glue("Patient withdrew before {mo3_end}")),
      raveid %nin% followup[followup$time=="Three Month (1)",]$raveid & as.numeric(today-radenddt)>112 ~ as.character(glue("<p><span style='color: #E74C3C;'><span class='glyphicon glyphicon-remove' style='color:#E74C3C'></span>Overdue ended {mo3_end}</span></p>")),
      raveid %nin% followup[followup$time=="Three Month (1)",]$raveid & as.numeric(today-radenddt)<=112 & as.numeric(today-radenddt)>=56 ~ as.character(glue("<p><span class='glyphicon glyphicon-hourglass' style='color:#E8C838'></span><span style='color: #E8C838'>In window (ends {mo3_end})</span></p>")),
      raveid %nin% followup[followup$time=="Three Month (1)",]$raveid & as.numeric(today-radenddt)<=56 ~ as.character(glue("<p><span class='glyphicon glyphicon-hourglass' style='color:#E8C838'></span><span style='color: #E8C838'>Before window (starts {mo3_start})</span></p>")),
      TRUE ~ NA_character_
    ),
    mo6_fu = case_when(
      raveid %in% followup[followup$time=="Six Month (1)",]$raveid ~ "<p><span class = 'glyphicon glyphicon-ok' style='color:#2ECC71'><span style='color: #2ECC71;'>Completed</span></p>",
      raveid %nin% followup[followup$time=="Six Month (1)",]$raveid & is.na(radenddt) & endstrsn %in% 
        c("Informed consent withdrawal*","Patient refused treatment arm") ~ "Patient withdrew before tx",
      is.na(radenddt) ~ "Patient did not receive tx",
      raveid %nin% followup[followup$time=="Six Month (1)",]$raveid & (endstrsn=="Informed consent withdrawal*" | withdrawal==1) & 
        radenddt+244>lastvisdt ~ as.character(glue("Patient withdrew before {mo3_end}")),
      raveid %nin% followup[followup$time=="Six Month (1)",]$raveid & as.numeric(today-radenddt)>244 ~ as.character(glue("<p><span style='color: #E74C3C;'><span class='glyphicon glyphicon-remove' style='color:#E74C3C'></span>Overdue ended {mo3_end}</span></p>")),
      raveid %nin% followup[followup$time=="Six Month (1)",]$raveid & as.numeric(today-radenddt)<=244 & as.numeric(today-radenddt)>=121 ~ as.character(glue("<p><span class='glyphicon glyphicon-hourglass' style='color:#E8C838'></span><span style='color: #E8C838'>In window (ends {radenddt+244})</span></p>")),
      raveid %nin% followup[followup$time=="Six Month (1)",]$raveid & as.numeric(today-radenddt)<=121 ~ as.character(glue("<p><span class='glyphicon glyphicon-hourglass' style='color:#E8C838'></span><span style='color: #E8C838'>Before window (starts {radenddt+121})</span></p>")),
      TRUE ~ NA_character_
    ),
    yr1_fu = case_when(
      raveid %in% followup[followup$time=="One Year (1)",]$raveid ~ "<p><span class = 'glyphicon glyphicon-ok' style='color:#2ECC71'> <span style='color: #2ECC71;'>Completed</span></p>",
      raveid %nin% followup[followup$time=="One Year (1)",]$raveid & is.na(radenddt) & endstrsn %in% 
        c("Informed consent withdrawal*","Patient refused treatment arm") ~ "Patient withdrew before tx",
      is.na(radenddt) ~ "Patient did not receive tx",
      raveid %nin% followup[followup$time=="One Year (1)",]$raveid & (endstrsn=="Informed consent withdrawal*" | withdrawal==1) & 
        mo12_end>lastvisdt ~ as.character(glue("Patient withdrew before {mo12_end}")),
      raveid %nin% followup[followup$time=="One Year (1)",]$raveid & as.numeric(today-radenddt)>549 ~ as.character(glue("<p><span style='color: #E74C3C;'><span class='glyphicon glyphicon-remove' style='color:#E74C3C'></span>Overdue ended {mo12_end}</span></p>")),
      raveid %nin% followup[followup$time=="One Year (1)",]$raveid & as.numeric(today-radenddt)<=549 & as.numeric(today-radenddt)>=183 ~ as.character(glue("<p><span class='glyphicon glyphicon-hourglass' style='color:#E8C838'></span><span style='color: #E8C838'>In window (ends {mo12_end})</span></p>")),
      raveid %nin% followup[followup$time=="One Year (1)",]$raveid & as.numeric(today-radenddt)<=183 ~ as.character(glue("<p><span class='glyphicon glyphicon-hourglass' style='color:#E8C838'></span><span style='color: #E8C838'>Before window (starts {mo12_start})</span></p>")),
      TRUE ~ NA_character_
    ),
    yr2_fu = case_when(
      raveid %in% followup[followup$time=="Two Year (1)",]$raveid ~ "<p><span class = 'glyphicon glyphicon-ok' style='color:#2ECC71'><span style='color: #2ECC71;'>Completed</span></p>",
      raveid %nin% followup[followup$time=="Two Year (1)",]$raveid & is.na(radenddt) & endstrsn %in% 
        c("Informed consent withdrawal*","Patient refused treatment arm") ~ "Patient withdrew before tx",
      is.na(radenddt) ~ "Patient did not receive tx",
      raveid %nin% followup[followup$time=="Two Year (1)",]$raveid & (endstrsn=="Informed consent withdrawal*" | withdrawal==1) & 
        yr2_end>lastvisdt ~ as.character(glue("Patient withdrew before {yr2_end}")),
      raveid %nin% followup[followup$time=="Two Year (1)",]$raveid & as.numeric(today-radenddt)>915 ~ as.character(glue("<p><span style='color: #E74C3C;'><span class='glyphicon glyphicon-remove' style='color:#E74C3C'></span>Overdue ended {yr2_end}</span></p>")),
      raveid %nin% followup[followup$time=="Two Year (1)",]$raveid & as.numeric(today-radenddt)<=915 & as.numeric(today-radenddt)>=549 ~ as.character(glue("<p><span class='glyphicon glyphicon-hourglass' style='color:#E8C838'></span><span style='color: #E8C838'>In window (ends {yr2_end})</span></p>")),
      raveid %nin% followup[followup$time=="Two Year (1)",]$raveid & as.numeric(today-radenddt)<=549 ~ as.character(glue("<p><span class='glyphicon glyphicon-hourglass' style='color:#E8C838'></span><span style='color: #E8C838'>Before window (starts {yr2_start})</span></p>")),
      TRUE ~ NA_character_
    ),
    yr3_fu = case_when(
      raveid %in% followup[followup$time=="Three Year (1)",]$raveid ~ "<p><span class = 'glyphicon glyphicon-ok' style='color:#2ECC71'><span style='color: #2ECC71;'>Completed</span></p>",
      raveid %nin% followup[followup$time=="Three Year (1)",]$raveid & is.na(radenddt) & endstrsn %in% 
        c("Informed consent withdrawal*","Patient refused treatment arm") ~ "Patient withdrew before tx",
      is.na(radenddt) ~ "Patient did not receive tx",
      raveid %nin% followup[followup$time=="Three Year (1)",]$raveid & (endstrsn=="Informed consent withdrawal*" | withdrawal==1) & 
        yr3_end>lastvisdt ~ as.character(glue("Patient withdrew before {yr3_end}")),
      raveid %nin% followup[followup$time=="Three Year (1)",]$raveid & as.numeric(today-radenddt)>1281 ~ as.character(glue("<p><span style='color: #E74C3C;'><span class='glyphicon glyphicon-remove' style='color:#E74C3C'></span>Overdue ended {yr3_end}</span></p>")),
      raveid %nin% followup[followup$time=="Three Year (1)",]$raveid & as.numeric(today-radenddt)<=1281 & as.numeric(today-radenddt)>=915 ~ as.character(glue("<p><span class='glyphicon glyphicon-hourglass' style='color:#E8C838'></span><span style='color: #E8C838'>In window (ends {yr3_end})</span></p>")),
      raveid %nin% followup[followup$time=="Three Year (1)",]$raveid & as.numeric(today-radenddt)<=915 ~ as.character(glue("<p><span class='glyphicon glyphicon-hourglass' style='color:#E8C838'></span><span style='color: #E8C838'>Before window (starts {yr3_start})</span></p>")),
      TRUE ~ NA_character_
    ),
    yr4_fu = case_when(
      raveid %in% followup[followup$time=="Four Year (1)",]$raveid ~ "<p><span class = 'glyphicon glyphicon-ok' style='color:#2ECC71'><span style='color: #2ECC71;'>Completed</span></p>",
      raveid %nin% followup[followup$time=="Four Year (1)",]$raveid & is.na(radenddt) & endstrsn %in% 
        c("Informed consent withdrawal*","Patient refused treatment arm") ~ "Patient withdrew before tx",
      is.na(radenddt) ~ "Patient did not receive tx",
      raveid %nin% followup[followup$time=="Four Year (1)",]$raveid & (endstrsn=="Informed consent withdrawal*" | withdrawal==1) & 
        yr4_end>lastvisdt ~ as.character(glue("Patient withdrew before {yr4_end}")),
      raveid %nin% followup[followup$time=="Four Year (1)",]$raveid & as.numeric(today-radenddt)>1647 ~ as.character(glue("<p><span style='color: #E74C3C;'><span class='glyphicon glyphicon-remove' style='color:#E74C3C'></span>Overdue ended {yr4_end}</span></p>")),
      raveid %nin% followup[followup$time=="Four Year (1)",]$raveid & as.numeric(today-radenddt)<=1647 & as.numeric(today-radenddt)>=1281 ~ as.character(glue("<p><span class='glyphicon glyphicon-hourglass' style='color:#E8C838'></span><span style='color: #E8C838'>In window (ends {yr4_end})</span></p>")),
      raveid %nin% followup[followup$time=="Four Year (1)",]$raveid & as.numeric(today-radenddt)<=1281 ~ as.character(glue("<p><span class='glyphicon glyphicon-hourglass' style='color:#E8C838'></span><span style='color: #E8C838'>Before window (starts {yr4_start})</span></p>")),
      TRUE ~ NA_character_
    ),
    yr5_fu = case_when(
      raveid %in% followup[followup$time=="Five Year (1)",]$raveid ~ "<p><span class = 'glyphicon glyphicon-ok' style='color:#2ECC71'><span style='color: #2ECC71;'>Completed</span></p>",
      raveid %nin% followup[followup$time=="Five Year (1)",]$raveid & is.na(radenddt) & endstrsn %in% 
        c("Informed consent withdrawal*","Patient refused treatment arm") ~ "Patient withdrew before tx",
      is.na(radenddt) ~ "Patient did not receive tx",
      raveid %nin% followup[followup$time=="Five Year (1)",]$raveid & (endstrsn=="Informed consent withdrawal*" | withdrawal==1) & 
        yr5_end>lastvisdt ~ as.character(glue("Patient withdrew before {yr5_end}")),
      raveid %nin% followup[followup$time=="Five Year (1)",]$raveid & as.numeric(today-radenddt)>2013 ~ as.character(glue("<p><span style='color: #E74C3C;'><span class='glyphicon glyphicon-remove' style='color:#E74C3C'></span>Overdue ended {yr5_end}</span></p>")),
      raveid %nin% followup[followup$time=="Five Year (1)",]$raveid & as.numeric(today-radenddt)<=2013 & as.numeric(today-radenddt)>=1647 ~ as.character(glue("<p><span class='glyphicon glyphicon-hourglass' style='color:#E8C838'></span><span style='color: #E8C838'>In window (ends {yr5_end})</span></p>")),
      raveid %nin% followup[followup$time=="Five Year (1)",]$raveid & as.numeric(today-radenddt)<=1647 ~ as.character(glue("<p><span class='glyphicon glyphicon-hourglass' style='color:#E8C838'></span><span style='color: #E8C838'>Before window (starts {yr5_start})</span></p>")),
      TRUE ~ NA_character_
    )
  )




# Adverse Event ---- 

# tox related
tox_related <- tox_combined %>%
  select(type, subject,time, 
         toxxx, Grade = grade, Attribution = attrib) %>% 
  group_by(subject, toxxx)

# Highest Toxicity by patient
tox_highest <- tox_related %>% 
  #filter(VALXX >= 3) %>%  
  group_by(subject) %>% 
  slice(which.max(Grade))


tox_highest_tbl <- tox_related %>%
  group_by(time) %>% 
  slice(which.max(Grade))

tox_highest_tbl <- tox_highest %>% select(subject, time,ae=toxxx,Grade) %>% ungroup()


tox_tbl <- tox_highest_tbl %>% count(time, ae, Grade) %>% filter(Grade >0)
tox_tbl_wide <- tox_tbl %>% spread(Grade, n)

tox_tbl_wide2 <-  tox_combined %>% 
  filter(is.na(grade)==F) %>% 
  select(subject, time,toxxx,Grade=grade) %>% ungroup() %>% 
  count(time, toxxx, Grade)%>% spread(Grade, n) %>% 
  rename(Timepoint=time,Toxicity=toxxx) %>% 
  mutate_all(as.character)

#merge an empty table with full col names
temp_table <- setNames(data.frame(matrix(ncol = 8, nrow = 0)), c('Timepoint', 'Toxicity','0', '1', '2', '3', '4','5'))
temp_table <- temp_table %>% 
  mutate_all(as.character)
tox_tbl_wide <- bind_rows(temp_table,tox_tbl_wide2)

tox_tbl_wide <- tox_tbl_wide %>% select(Timepoint, Toxicity,`Grade 0` =`0`, `Grade 1` = `1`, `Grade 2` = `2`, `Grade 3` = `3`, `Grade 4` = `4`,`Grade 5` = `5`)



arm <- accrual %>% select(subject,site,arm=regime_name)
tox_combined <- left_join(tox_combined,arm,by="subject")

tox_related <- tox_combined %>%
  group_by(subject, toxxx) %>% filter(is.na(grade)==T | grade %in% c(1,2,3,4,5))



# Save data ---- 

all_data <- list(ptrax,accrual,tox_related,tox_tbl_wide,tox_combined)

board <- pins::board_register_rsconnect(server = "link to server",
                                        key = Sys.getenv("API key name"))


board %>% pin_write(all_data,name="LANID/data_name",type="rds", force_identical_write = T)
# You can use a feather is your data is very large. 
